gh_user_general <- utils::getFromNamespace ("gh_user_general", "repometrics")
gh_user_follow <- utils::getFromNamespace ("gh_user_follow", "repometrics")

# These are modified versions from repometrics functions. The GraphQL API only
# allows "from" and "to" dates which span 1 year or less. The "gh_user_commits"
# function here calls the query multiple times, adjusting the "end_year" each
# time.
gh_user_commits_qry <- function (login = "",
                                 end_date = Sys.Date (),
                                 nyears = 1,
                                 n_per_page = 100L,
                                 end_cursor = NULL) {

    # These 'format' calls pad with hms = "00:00:00":
    from <- format (end_date - 365.25, "%Y-%m-%dT%H:%M:%S")
    end_date <- format (end_date, "%Y-%m-%dT%H:%M:%S")

    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            createdAt
            contributionsCollection (from: \"", from, "\", to: \"", end_date, "\") {
                startedAt
                endedAt
                totalCommitContributions
                commitContributionsByRepository (maxRepositories: ", n_per_page, ") {
                    contributions (first: ", n_per_page, after_txt, ") {
                        pageInfo {
                            hasNextPage
                            endCursor
                        }
                        totalCount
                        nodes {
                            occurredAt
                            commitCount
                            repository {
                                nameWithOwner
                            }
                        }
                    }
                }
            }
        }
    }")

    return (q)
}

gh_user_commits_internal <- function (login, end_date = Sys.Date ()) {

    user <- repos <- num_commits <- dates <- end_cursor <- end_cursors <- NULL

    finished <- FALSE
    year_count <- 0
    n_per_page <- 100

    while (!finished) {

        has_next_page <- TRUE
        end_cursor <- NULL
        while (has_next_page) {

            q <- gh_user_commits_qry (
                login = login,
                end_date = end_date,
                nyears = nyears,
                n_per_page = n_per_page,
                end_cursor = end_cursor
            )
            dat <- gh::gh_gql (query = q)

            collection <- dat$data$user$contributionsCollection
            commits <- collection$commitContributionsByRepository

            # Query always returns `n_per_page` items, even when empty, so empty
            # ones must first be removed:
            lens <- vapply (
                commits,
                function (i) length (i$contributions$nodes),
                integer (1L)
            )
            commits <- commits [which (lens > 0)]

            repos_i <- vapply (
                commits,
                function (i) i$contributions$nodes [[1]]$repository$nameWithOwner,
                character (1L)
            )

            dates_i <- lapply (commits, function (i) {
                vapply (
                    i$contributions$nodes,
                    function (j) j$occurredAt,
                    character (1L)
                )
            })
            n_i <- vapply (dates_i, length, integer (1L))
            dates <- c (dates, paste0 (as.Date (unlist (dates_i))))
            commit_count_i <- lapply (commits, function (i) {
                vapply (
                    i$contributions$nodes,
                    function (j) j$commitCount,
                    integer (1L)
                )
            })
            num_commits <- c (num_commits, unlist (commit_count_i))

            repos <- c (repos, rep (repos_i, times = n_i))

            has_next_pages <- vapply (commits, function (i) {
                i$contributions$pageInfo$hasNextPage
            }, logical (1L))
            end_cursors_these <- vapply (commits, function (i) {
                i$contributions$pageInfo$endCursor
            }, character (1L))
            end_cursors_these <- unique (end_cursors_these [which (has_next_pages)])
            end_cursors <- c (end_cursors, end_cursors_these)
            has_next_page <- length (end_cursors) > 0L
            if (has_next_page) {
                end_cursor <- end_cursors [1L]
                end_cursors <- end_cursors [-1L]
            }
        }
        end_date <- min (as.Date (dates)) - 1

        user_created_at <- as.Date (dat$data$user$createdAt)
        dat_started_at <- as.Date (dat$data$user$contributionsCollection$startedAt)
        td <- difftime (dat_started_at, user_created_at, units = "weeks")
        if (td < 52) {
            finished <- TRUE
        }

        year_count <- year_count + 1
        if (year_count > 50) {
            finished <- TRUE
        }
    }

    data.frame (
        repo = repos,
        commits = num_commits
    ) |>
        dplyr::group_by (repo) |>
        dplyr::summarise (commits = sum (commits)) |>
        dplyr::ungroup () |>
        dplyr::mutate (org = gsub ("\\/.*$", "", repo), .after = repo) |>
        dplyr::mutate (login = login, .before = repo)
}
gh_user_commits <- memoise::memoise (gh_user_commits_internal)

om_data_gh_contributor <- function (login, end_date = Sys.Date (), nyears = 3) {

    dat_gen <- gh_user_general (login, end_date = end_date, nyears = nyears)
    dat_followers <- gh_user_follow (login, followers = TRUE)
    dat_following <- gh_user_follow (login, followers = FALSE)
    dat_commits <- gh_user_commits (login, end_date = end_date)

    res <- dat_gen$user |>
        dplyr::select (!c (email, bio, avatarUrl))
    res$num_orgs <- nrow (dat_gen$orgs)
    res$followers <- length (dat_followers)
    res$following <- length (dat_following)
    res$commits_total <- sum (dat_commits$commits)
    res$commits_total_repos <- nrow (dat_commits)
    res$commits_total_orgs <- length (unique (dat_commits$org))

    return (res)
}

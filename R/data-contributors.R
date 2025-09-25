gh_user_general <- utils::getFromNamespace ("gh_user_general", "repometrics")
gh_user_follow <- utils::getFromNamespace ("gh_user_follow", "repometrics")

#' Add full data on each contributor after assembling all repo-level data.
#'
#' @param data_org Partial result of 'orgmetrics_collate_org_data', including
#' full data on all repos.
#' @return Same data with additional 'contributors' item.
add_org_contributor_data <- function (data_org) {

    ctbs <- get_unique_ctbs (data_org)
    ctb_dat <- pbapply::pblapply (ctbs, om_data_gh_contributor)

}

get_unique_ctbs <- function (data_org) {

    ctbs <- lapply (data_org$repos, function (repo) {
        repo$rm$contribs_from_gh_api$login
    }) |>
        unlist () |>
        unname () |>
        unique ()
    ctbs <- ctbs [which (!grepl ("\\[bot\\]", ctbs))]
    ctbs <- setdiff (ctbs, "ci-bot")
    # Running 'url_exists' takes way too long, but all ctbs exist as of Sep
    # 2025, so no checks done.

    # "Copilot" is a contributor, but doesn't exist either as a URL or an API
    # endpoint:
    ctbs <- setdiff (ctbs, "Copilot")

    return (ctbs)
}

gh_user_created_at_qry <- function (login = "") {

    paste0 ("{
        user(login:\"", login, "\") {
            login
            createdAt
        }
    }")
}

gh_user_activity_qry <- function (login = "", start_date, end_date) {

    q <- paste0 ("{
        user(login:\"", login, "\") {
            login
            createdAt
            contributionsCollection (from: \"", start_date, "\", to: \"", end_date, "\") {
                startedAt
                endedAt
                totalCommitContributions
                totalIssueContributions
                totalPullRequestContributions
                commitContributionsByRepository (maxRepositories: 100) {
                    repository {
                        nameWithOwner
                    }
                }
                issueContributionsByRepository (maxRepositories: 100) {
                    repository {
                        nameWithOwner
                    }
                }
                pullRequestContributionsByRepository (maxRepositories: 100) {
                    repository {
                        nameWithOwner
                    }
                }
            }
        }
    }")

    return (q)
}

# The 'commitcontributionsByRepository' only accepts the single parameter
# of 'maxRepositories', so the only way to ensure all are captured is to
# restrict the date ranges in the 'contributionsCollection' to ensure < 100
# repositories are returned. The default here is thus to quarterly values.
gh_user_activity_internal <- function (login, end_date = Sys.Date (), period = 0.25) {

    q <- gh_user_created_at_qry (login = login)
    dat <- gh::gh_gql (query = q)

    created_at <- as.Date (dat$data$user$createdAt)
    dates <- seq (created_at, end_date, by = 365.25 * period)
    dates <- c (dates, end_date)
    dates <- data.frame (start = dates [-length (dates)], stop = dates [-1])
    dates <- split (dates, f = as.factor (seq_len (nrow (dates))))

    res <- pbapply::pblapply (dates, function (d) {
        q <- gh_user_activity_qry (
            login = login,
            start_date = format (d$start, "%Y-%m-%dT%H:%M:%S"),
            end_date = format (d$stop, "%Y-%m-%dT%H:%M:%S")
        )
        dat <- gh::gh_gql (query = q)
        cc <- dat$data$user$contributionsCollection

        list (
            end_date = d$stop,
            total_commits = cc$totalCommitContributions,
            total_issues = cc$totalIssueContributions,
            total_prs = cc$totalPullRequestContributions,
            repos_commits = vapply (cc$commitContributionsByRepository, function (repo) {
                repo$repository$nameWithOwner
            }, character (1L)),
            repos_issues = vapply (cc$issueContributionsByRepository, function (repo) {
                repo$repository$nameWithOwner
            }, character (1L)),
            repos_prs = vapply (cc$pullRequestContributionsByRepository, function (repo) {
                repo$repository$nameWithOwner
            }, character (1L)),
            n_repos_commits = length (cc$commitContributionsByRepository),
            n_repos_issues = length (cc$issueContributionsByRepository),
            n_repos_prs = length (cc$pullRequestContributionsByRepository)
        )
    })

    end_dates <- vapply (res, function (i) as.character (i$end_date), character (1L))
    total_commits <- vapply (res, function (i) i$total_commits, integer (1L))
    total_issues <- vapply (res, function (i) i$total_issues, integer (1L))
    total_prs <- vapply (res, function (i) i$total_prs, integer (1L))
    get1unique <- function (res, what = "commits") {
        unique (unlist (unname (
            lapply (res, function (i) i [[paste0 ("repos_", what)]])
        )))
    }
    repos_commits <- get1unique (res, "commits")
    repos_issues <- get1unique (res, "issues")
    repos_prs <- get1unique (res, "prs")
    n_repos_commits <- vapply (res, function (i) i$n_repos_commits, integer (1L))
    n_repos_issues <- vapply (res, function (i) i$n_repos_issues, integer (1L))
    n_repos_prs <- vapply (res, function (i) i$n_repos_prs, integer (1L))

    for (what in c ("issues", "prs", "commits")) {
        dat <- get (paste0 ("n_repos_", what))
        len_missing <- length (which (dat == 100L))
        len_total <- length (dat)
        if (len_missing > 0L) {
            what_cap <- paste0 (
                toupper (substring (what, 1, 1)),
                substring (what, 2, nchar (what))
            )
            cli::cli_alert_danger (paste0 (
                "{what_cap} is missing data for {len_missing} / {len_total} ",
                " periods; maybe reduce time resolution?"
            ))
        }
    }

    res <- list (
        commits = unique (repos_commits),
        issues = unique (repos_issues),
        prs = unique (repos_prs),
        dat_ts = data.frame (
            end_date = end_dates,
            commits = total_commits,
            issues = total_issues,
            prs = total_prs
        )
    )
}
gh_user_activity <- memoise::memoise (gh_user_activity_internal)

om_data_gh_contributor <- function (login, end_date = Sys.Date (), nyears = 3) {

    dat_gen <- gh_user_general (login, end_date = end_date, nyears = nyears)
    dat_followers <- gh_user_follow (login, followers = TRUE)
    dat_following <- gh_user_follow (login, followers = FALSE)
    dat_activity <- gh_user_activity (login, end_date = end_date)

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

gh_user_general <- utils::getFromNamespace ("gh_user_general", "repometrics")
gh_user_follow <- utils::getFromNamespace ("gh_user_follow", "repometrics")

#' Add full data on each contributor after assembling all repo-level data.
#'
#' @param data_org Partial result of 'orgmetrics_collate_org_data', including
#' full data on all repos.
#' @param end_date To up to which to extract data.
#' @param num_cores Set `NULL` to calculate with single thread, which may take
#' a very long time. Otherwise default will calculate with one less than all
#' available cores, or specify an exact integer number.
#' @return Same data with additional 'contributors' item.
#' @noRd
org_contributor_data <- function (data_org, end_date = Sys.Date (), num_cores = -1L) {

    checkmate::assert_integerish (num_cores, len = 1L, lower = -1L)

    ctbs <- get_unique_ctbs (data_org)

    ctb_dat <- pbapply::pblapply (ctbs, function (ctb) {

        ctb_dat <- gh_user_activity (
            login = ctb,
            end_date = end_date,
            period = 1 / 12,
            num_cores = num_cores
        )

        dat_gen <- gh_user_general (login = ctb)
        dat_followers <- gh_user_follow (login = ctb, followers = TRUE)
        dat_following <- gh_user_follow (login = ctb, followers = FALSE)

        data.frame (
            login = ctb,
            created_at = ctb_dat$created_at,
            commits = sum (ctb_dat$dat_ts$commits),
            issues = sum (ctb_dat$dat_ts$issues),
            prs = sum (ctb_dat$dat_ts$prs),
            n_repos_commits = length (unique (ctb_dat$commits)),
            n_repos_issues = length (unique (ctb_dat$issues)),
            n_repos_prs = length (unique (ctb_dat$prs)),
            n_org_commits = length (unique (gsub ("\\/.*$", "", ctb_dat$commits))),
            n_org_issues = length (unique (gsub ("\\/.*$", "", ctb_dat$issues))),
            n_org_prs = length (unique (gsub ("\\/.*$", "", ctb_dat$prs))),
            num_org_memberships = nrow (dat_gen$org),
            n_starred_repos = dat_gen$user$num_starred_repos,
            num_followers = length (dat_followers),
            num_following = length (dat_following)
        )
    })

    do.call (rbind, ctb_dat)
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
gh_user_activity_internal <- function (login,
                                       end_date = Sys.Date (),
                                       period = 0.25,
                                       num_cores = -1L) {

    q <- gh_user_created_at_qry (login = login)
    dat <- gh::gh_gql (query = q)

    created_at <- as.Date (dat$data$user$createdAt)
    dates <- seq (created_at, end_date, by = 365.25 * period)
    dates <- c (dates, end_date)
    dates <- data.frame (start = dates [-length (dates)], stop = dates [-1])
    dates <- split (dates, f = as.factor (seq_len (nrow (dates))))

    if (num_cores == 0L) {
        num_cores <- NULL
    }
    cl <- NULL
    if (!is.null (num_cores)) {

        requireNamespace ("parallel", quietly = TRUE)

        if (num_cores == -1L) {
            num_cores <- parallel::detectCores () - 1L
        }
        cl <- parallel::makeCluster (n_cores)
        parallel::clusterExport (cl, c ("gh_user_activity_qry"))
    }

    # Let pbapply bar get applied over all "login" values, not here
    opb <- pbapply::pboptions (type = "none")
    on.exit (pbapply::pboptions (opb))

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
    }, cl = cl)

    if (!is.null (num_cores)) {
        parallel::stopCluster (cl)
    }

    null2zero <- function (x) {
        ifelse (length (x) == 0, 0L, x)
    }
    end_dates <- vapply (res, function (i) as.character (i$end_date), character (1L))
    total_commits <- vapply (res, function (i) null2zero (i$total_commits), integer (1L))
    total_issues <- vapply (res, function (i) null2zero (i$total_issues), integer (1L))
    total_prs <- vapply (res, function (i) null2zero (i$total_prs), integer (1L))
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

    any_missing <- FALSE
    for (what in c ("issues", "prs", "commits")) {
        dat <- get (paste0 ("n_repos_", what))
        len_missing <- length (which (dat == 100L))
        len_total <- length (dat)
        if (len_missing > 0L) {
            any_missing <- TRUE
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
    if (any_missing) {
        cli::cli_alert_info (paste0 (
            "Missing data only mean lists of unique repositories may be ",
            "incomplete; time series data are complete regardless."
        ))
    }

    res <- list (
        created_at = created_at,
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

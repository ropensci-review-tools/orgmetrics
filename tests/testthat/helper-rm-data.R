# Helper function to use httptest2 mocks to load all data, after which function
# calls are memoised and so can be called without mocking from that point on.

requireNamespace ("repometrics", quietly = TRUE)

rm_data_contribs_from_gh_api <-
    utils::getFromNamespace ("rm_data_contribs_from_gh_api", "repometrics")
rm_data_contribs_from_log <-
    utils::getFromNamespace ("rm_data_contribs_from_log", "repometrics")
rm_data_repo_forks <-
    utils::getFromNamespace ("rm_data_repo_forks", "repometrics")
rm_data_issues_from_gh_api <-
    utils::getFromNamespace ("rm_data_issues_from_gh_api", "repometrics")
rm_data_issue_comments_from_gh_api <- # nolint
    utils::getFromNamespace (
        "rm_data_issue_comments_from_gh_api", "repometrics"
    )
rm_data_prs_from_gh_api <-
    utils::getFromNamespace ("rm_data_prs_from_gh_api", "repometrics")
rm_data_releases_from_gh_api <-
    utils::getFromNamespace ("rm_data_releases_from_gh_api", "repometrics")
rm_data_repo_stargazers <-
    utils::getFromNamespace ("rm_data_repo_stargazers", "repometrics")
rm_data_gh_repo_workflow <-
    utils::getFromNamespace ("rm_data_gh_repo_workflow", "repometrics")
rm_data_repo_from_gh_api <-
    utils::getFromNamespace ("rm_data_repo_from_gh_api", "repometrics")
rm_data_dependencies <-
    utils::getFromNamespace ("rm_data_dependencies", "repometrics")
rm_data_dependencies_downstream <-
    utils::getFromNamespace ("rm_data_dependencies_downstream", "repometrics")
rm_data_libyears <-
    utils::getFromNamespace ("rm_data_libyears", "repometrics")
rm_data_repo <-
    utils::getFromNamespace ("rm_data_repo", "repometrics")
rm_data_gitlog <-
    utils::getFromNamespace ("rm_data_gitlog", "repometrics")
rm_data_r_universe <-
    utils::getFromNamespace ("rm_data_r_universe", "repometrics")

gh_user_general <-
    utils::getFromNamespace ("gh_user_general", "repometrics")
gh_user_follow <-
    utils::getFromNamespace ("gh_user_follow", "repometrics")
gh_user_commit_cmt <-
    utils::getFromNamespace ("gh_user_commit_cmt", "repometrics")
gh_user_commits <-
    utils::getFromNamespace ("gh_user_commits", "repometrics")
gh_user_issues <-
    utils::getFromNamespace ("gh_user_issues", "repometrics")
gh_user_issue_cmts <-
    utils::getFromNamespace ("gh_user_issue_cmts", "repometrics")

get_rm_data_fns <-
    utils::getFromNamespace ("get_rm_data_fns", "repometrics")
get_rm_gh_user_fns <-
    utils::getFromNamespace ("get_rm_gh_user_fns", "repometrics")
get_all_contribs <-
    utils::getFromNamespace ("get_all_contribs", "repometrics")

mock_rm_data <- function (repo = TRUE) {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    path <- generate_test_pkg ()
    ctbs <- httptest2::with_mock_dir ("gh_api_ctbs", {
        rm_data_contribs_from_gh_api (path)
    })
    workflow <- httptest2::with_mock_dir ("gh_api_workflow", {
        rm_data_gh_repo_workflow (path)
    })
    cmts <- httptest2::with_mock_dir ("gh_api_issue_cmts", {
        rm_data_issue_comments_from_gh_api (path)
    })
    issues <- httptest2::with_mock_dir ("gh_api_issues", {
        rm_data_issues_from_gh_api (path)
    })
    libyears <- httptest2::with_mock_dir ("gh_libyears", {
        rm_data_libyears (path)
    })
    prs <- httptest2::with_mock_dir ("gh_api_prs", {
        rm_data_prs_from_gh_api (path)
    })
    releases <- httptest2::with_mock_dir ("gh_api_releases", {
        rm_data_releases_from_gh_api (path)
    })
    forks <- httptest2::with_mock_dir ("gh_api_forks", {
        rm_data_repo_forks (path)
    })
    repo_data <- httptest2::with_mock_dir ("gh_api_repo", {
        rm_data_repo_from_gh_api (path)
    })
    stargazers <- httptest2::with_mock_dir ("gh_api_stars", {
        rm_data_repo_stargazers (path)
    })

    # rm-data-user:
    login <- "mpadge"
    end_date <- as.Date ("2024-01-01")
    pars <- list (
        login = login,
        n_per_page = 1,
        end_date = end_date,
        nyears = 1
    )
    general <- httptest2::with_mock_dir ("gh_user_general", {
        do.call (gh_user_general, pars)
    })
    followers <- httptest2::with_mock_dir ("gh_user_followers", {
        do.call (gh_user_follow, c (pars, followers = TRUE))
    })
    following <- httptest2::with_mock_dir ("gh_user_following", {
        do.call (gh_user_follow, c (pars, followers = FALSE))
    })
    user_commit_cmt <- httptest2::with_mock_dir ("gh_user_commit_cmt", {
        do.call (gh_user_commit_cmt, pars)
    })
    user_commits <- httptest2::with_mock_dir ("gh_user_commits", {
        do.call (gh_user_commits, pars)
    })
    user_issues <- httptest2::with_mock_dir ("gh_user_issues", {
        do.call (gh_user_issues, pars)
    })
    user_issue_cmts <- httptest2::with_mock_dir ("gh_user_issue_cmts", {
        do.call (gh_user_issue_cmts, pars)
    })

    # cran_downloads fn needs modified DESC:
    desc_path <- fs::path (path, "DESCRIPTION")
    desc <- readLines (desc_path)
    desc [1] <- "Package: goodpractice"
    writeLines (desc, desc_path)

    # The return full mocked data set:
    if (repo) {
        data_fns <- get_rm_data_fns ()
        res <- lapply (data_fns, function (i) {
            do.call (i, list (path = path))
        })
        names (res) <- gsub ("^rm\\_data\\_", "", data_fns)
        res$contributors <- get_all_contribs (
            res$contribs_from_log,
            res$contribs_from_gh_api
        )
    } else {
        data_fns <- get_rm_gh_user_fns ()
        pars <- list (
            login = login,
            n_per_page = 1,
            end_date = end_date,
            nyears = 1
        )

        res <- lapply (data_fns, function (i) {
            do.call (i, pars)
        })
        names (res) <- gsub ("^gh\\_user\\_", "", data_fns)

        names (res) <- gsub ("follow", "followers", names (res))
        res$following <- do.call (gh_user_follow, c (pars, followers = FALSE))

        i <- grep ("general", names (res))
        res <- c (res [i], res [-i] [order (names (res) [-i])])
    }

    fs::dir_delete (path)

    return (res)
}

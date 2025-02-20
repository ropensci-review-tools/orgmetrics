# Extends from 'cm-data-github.R' to test the user-specific data

test_that ("rm user data internal structures", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    dat <- mock_rm_data (repo = FALSE)

    expect_type (dat, "list")
    expect_length (dat, 7L)
    nms <- c (
        "general", "commit_cmt", "commits", "followers", "following",
        "issue_cmts", "issues"
    )
    expect_named (dat, nms)

    expect_type (dat$general, "list")
    expect_length (dat$general, 2L)
    nms <- c ("user", "orgs")
    expect_named (dat$general, nms)

    expect_s3_class (dat$general$user, "data.frame")
    expect_equal (nrow (dat$general$user), 1L)
    expect_equal (ncol (dat$general$user), 10L)
    nms <- c (
        "login", "name", "email", "location", "company", "bio", "avatarUrl",
        "num_repositories", "repos_contributed_to", "num_starred_repos"
    )
    expect_named (dat$general$user, nms)

    expect_s3_class (dat$general$orgs, "data.frame")
    expect_true (nrow (dat$general$orgs) > 10L)
    expect_equal (ncol (dat$general$orgs), 6L)
    nms <- c (
        "name", "gh_org", "url", "web_url", "location", "num_members"
    )
    expect_named (dat$general$orgs, nms)

    expect_type (dat$followers, "character")
    expect_true (length (dat$followers) > 1L)
    expect_type (dat$following, "character")
    expect_true (length (dat$following) > 1L)

    expect_s3_class (dat$commit_cmt, "data.frame")
    expect_true (nrow (dat$commit_cmt) > 1L)
    expect_equal (ncol (dat$commit_cmt), 4L)
    nms <- c ("org", "repo", "timestamp", "stargazers")
    expect_named (dat$commit_cmt, nms)

    expect_s3_class (dat$commits, "data.frame")
    expect_true (nrow (dat$commits) > 0L)
    expect_equal (ncol (dat$commits), 3L)
    nms <- c ("repo", "num_commits", "date")
    expect_named (dat$commits, nms)

    expect_s3_class (dat$issues, "data.frame")
    expect_true (nrow (dat$issues) > 1L)
    expect_equal (ncol (dat$issues), 8L)
    nms <- c (
        "opened_at", "closed_at", "org_repo", "issue_num",
        "num_issue_comments", "num_issue_participants", "num_repo_languages",
        "repo_languages"
    )
    expect_named (dat$issues, nms)

    expect_s3_class (dat$issue_cmts, "data.frame")
    expect_true (nrow (dat$issue_cmts) > 1L)
    expect_equal (ncol (dat$issue_cmts), 5L)
    nms <- c ("org_repo", "issue_num", "created_at", "num_comments", "num_participants")
    expect_named (dat$issue_cmts, nms)
})

test_that ("repometrics_data_user fn", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    dat_mocked <- mock_rm_data (repo = FALSE)

    login <- "mpadge"
    ended_at <- as.POSIXct ("2024-01-01T00:00:00")

    dat <- repometrics_data_user (
        login = login,
        n_per_page = 1,
        ended_at = ended_at,
        nyears = 1
    )
    expect_identical (dat, dat_mocked)
})

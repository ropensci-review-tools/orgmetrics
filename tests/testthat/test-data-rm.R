test_that ("rm data full", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    dat <- rm_data_repo (path)

    fs::dir_delete (path)

    expect_type (dat, "list")
    expect_length (dat, 14L)
    nms <- c (
        "contribs_from_gh_api", "contribs_from_log", "dependencies",
        "dependencies_downstream", "gh_repo_workflow", "gitlog",
        "issue_comments_from_gh_api", "issues_from_gh_api", "libyears",
        "prs_from_gh_api", "releases_from_gh_api", "repo_forks",
        "repo_from_gh_api", "repo_stargazers"
    )
    expect_equal (names (dat), nms)

    data_fns <- get_rm_data_fns ()
    data_fn_nms <- gsub ("^rm\\_data\\_", "", data_fns)
    expect_identical (names (dat), data_fn_nms)
})

# Individual components are tested in test-cm-data-github.R and *-git.R
# except for these which use cran_pkg_db:

test_that ("rm data dependencies", {

    path <- generate_test_pkg ()
    deps <- rm_data_dependencies (path)
    fs::dir_delete (path)

    expect_s3_class (deps, "data.frame")
    expect_equal (nrow (deps), 1L)
    expect_equal (ncol (deps), 3L)
    nms <- c ("name", "type", "version")
    expect_equal (names (deps), nms)
    for (n in names (deps)) {
        expect_type (deps [[n]], "character")
    }
})

test_that ("rm data reverse dependencies", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    revdeps <- rm_data_dependencies_downstream (path)

    fs::dir_delete (path)

    expect_type (revdeps, "character")
    # "testpkg" is not on CRAN, so no revdeps:
    expect_length (revdeps, 0L)
})

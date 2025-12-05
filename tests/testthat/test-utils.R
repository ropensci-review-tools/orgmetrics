test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("gh org repos", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true") # for n_per_page --> 2
    org <- "ropensci"
    repo_names <- httptest2::with_mock_dir ("utils_gh_org", {
        list_gh_org_repos (org)
    })

    expect_type (repo_names, "character")
    expect_length (repo_names, n_per_page_in_tests (100))
    expect_true (any (grepl ("\\/", repo_names))) # bare names only
})

test_that ("pkgs are r", {

    pkgs <- "ropensci-review-tools/orgmetrics"
    chk <- httptest2::with_mock_dir ("pkgs_are_r", {
        pkgs_are_r (pkgs)
    })
    expect_length (chk, length (pkgs))
    expect_type (chk, "logical")
    expect_true (all (chk))
    expect_named (chk)
})

test_that ("write pkgs_json", {

    d <- fs::path (fs::path_temp (), "tmpjson")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    repos <- c ("orgmetrics", "repometrics")
    repos <- vapply (repos, function (repo) {
        tmp <- generate_test_pkg ()
        path <- fs::dir_copy (
            tmp,
            fs::path (d, "ropensci-review-tools", repo),
            overwrite = TRUE
        )
        fs::path_rel (path, start = d)
    }, character (1L))
    pkgs <- data.frame (
        repo_path = fs::path (d, repos),
        repos = repos,
        row.names = NULL
    )

    f <- write_pkgs_json (pkgs, dir = d)
    expect_s3_class (f, "fs_path")
    expect_true (fs::file_exists (f))
    expect_equal (basename (f), "packages.json")

    fs::dir_delete (d)
})

skip_if (!test_all)

# No mocking here, just actual cloning on 1st two rOpenSci repos
test_that ("clone gh org repos", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true") # n_per_page = 2

    d <- fs::path (fs::path_temp (), "orgrepos")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    repos <- c ("orgmetrics", "repometrics")
    repos <- vapply (repos, function (repo) {
        tmp <- generate_test_pkg ()
        path <- fs::dir_copy (
            tmp,
            fs::path (d, "ropensci-review-tools", repo),
            overwrite = TRUE
        )
        fs::path_rel (path, start = d)
    }, character (1L))
    pkgs <- data.frame (
        repo_path = fs::path (d, repos),
        repos = repos,
        row.names = NULL
    )

    f <- write_pkgs_json (pkgs, dir = d)
    fs::dir_delete (pkgs$repo_path)

    clone_gh_org_repos (pkgs_json = f)

    f <- fs::dir_ls (d, type = "file")
    expect_length (f, 1L)
    expect_equal (basename (f), "packages.json")
    f <- fs::dir_ls (d, type = "directory")
    expect_length (f, 1L)
    expect_equal (basename (f), "ropensci-review-tools")

    d_org <- f
    f <- fs::dir_ls (d_org, type = "directory")
    expect_length (f, 2L)

    fs::dir_delete (d)
})

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
    pkgs <- "ropensci-review-tools/orgmetrics"
    d <- fs::path (fs::path_temp (), "tmpjson")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    f <- httptest2::with_mock_dir ("pkgs-json", {
        write_pkgs_json (pkgs, dir = d)
    })
    expect_s3_class (f, "fs_path")
    expect_true (fs::file_exists (f))
    expect_equal (basename (f), "packages.json")

    fs::dir_delete (d)
})

test_that ("gh org repos", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    org <- "ropensci"
    repo_names <- httptest2::with_mock_dir ("utils_gh_org", {
        list_gh_org_repos (org)
    })

    expect_type (repo_names, "character")
    expect_length (repo_names, n_per_page_in_tests (30))
    expect_true (any (grepl ("\\/", repo_names))) # bare names only
})

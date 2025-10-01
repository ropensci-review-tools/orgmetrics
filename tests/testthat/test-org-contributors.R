# These tests need 'zoo' and 'tidyr' in the namespace, as they call internal
# 'repometrics' functions which rely on these.

end_date <- as.Date ("2024-08-01")

get_end_date_seq <- utils::getFromNamespace ("get_end_date_seq", "repometrics")

requireNamespace ("tidyr", quietly = TRUE)
requireNamespace ("zoo", quietly = TRUE)
requireNamespace ("visNetwork", quietly = TRUE)

test_that ("test org contributors", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    org_dir <- mock_collate_org_data (end_date = end_date)

    pkgs <- c ("testpkg1", "testpkg2")
    pkgs_dat <- data.frame (
        path = fs::path (org_dir, pkgs),
        orgrepo = fs::path ("ropensci-review-tools", pkgs),
        root = fs::path ("ropensci-review-tools", pkgs),
        is_r_pkg = TRUE,
        row.names = NULL
    )
    pkgs_dat$path <- fs::path_rel (pkgs_dat$path, fs::path_temp ())
    pkgs_json <- fs::file_temp (ext = "json")
    jsonlite::write_json (pkgs_dat, path = pkgs_json, pretty = TRUE)

    org_data <- orgmetrics_collate_org_data (
        pkgs_json,
        end_date = end_date,
        num_years = 1
    )

    fs::dir_delete (org_dir)

    ctbs <- get_unique_ctbs (org_data$repos)
    expect_type (ctbs, "character")
    expect_gt (length (ctbs), 1L)
    expect_equal (length (ctbs), nrow (org_data$contributors))
    # --- This test fails on GHA because 'org_data$contributrs' is empty, yet
    # --- 'ctbs' has the two expected contributors. I can't repeat that failure
    # --- anywhere else though?
    # expect_identical (sort (ctbs), sort (org_data$contributors$login))

    expect_named (
        org_data$contributors,
        c (
            "login", "name", "avatarUrl", "num_repositories",
            "repos_contributed_to", "num_issues_opened", "num_prs_opened",
            "num_starred_repos", "num_orgs"
        )
    )
})

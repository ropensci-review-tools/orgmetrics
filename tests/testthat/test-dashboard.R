test_that ("dashboard data generation", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    Sys.setenv ("ORGMETRICS_TESTS" = "true")

    end_date <- as.Date ("2024-08-01")
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

    # `orgmetrics_collate_org_data` calls `org_contributor_data` which calls
    # `gh_user_general`. `mock_collate_org_data` calls `mock_rm_data` which
    # memoises `gh_user_general`.
    org_data <- orgmetrics_collate_org_data (
        pkgs_json,
        end_date = end_date,
        num_years = 1
    )

    expect_type (org_data, "list")

    # Test dashboard_data_repo_metrics
    metrics_df <- data_metrics_to_df (org_data$metrics)
    dates <- sort (unique (metrics_df$date), decreasing = TRUE)
    repo_metrics <- dashboard_data_repo_metrics (metrics_df, dates)
    expect_type (repo_metrics, "list")
    expect_named (repo_metrics, pkgs)
    expect_true (all (vapply (repo_metrics, is.data.frame, logical (1))))

    # Test dashboard_data_contributors
    contributors <- dashboard_data_contributors (org_data)
    expect_type (contributors, "list")
    expect_named (contributors, pkgs)
    expect_true (all (vapply (contributors, is.data.frame, logical (1))))
    expect_true (all (
        c ("login", "name", "contributions", "is_author") %in%
            names (contributors [[1]])
    ))

    # Test dashboard_data_repo_source
    repo_source <- dashboard_data_repo_source (org_data)
    expect_s3_class (repo_source, "data.frame")
    expect_named (repo_source, c ("package", "orgrepo", "url"))
    expect_equal (nrow (repo_source), 2)

    # Test dashboard_data_cran
    cran_data <- dashboard_data_cran (org_data)
    if (length (cran_data) > 0L) {
        expect_s3_class (cran_data, "data.frame")
    } else {
        expect_type (cran_data, "list")
        expect_length (cran_data, 0)
    }

    # Test dashboard_data_gitlog
    gitlog <- dashboard_data_gitlog (org_data)
    expect_s3_class (gitlog, "data.frame")
    expect_named (gitlog, c (
        "package", "num_commits", "first_commit",
        "latest_commit", "recent_commits"
    ))
    expect_equal (nrow (gitlog), 2)

    # Test dashboard_data_r_universe
    r_universe <- dashboard_data_r_universe (org_data)
    expect_type (r_universe, "list")
    expect_named (r_universe, c (
        "data_is_on_r_univ", "r_univ_stats", "r_univ_jobs", "r_univ_builds"
    ))
    expect_s3_class (r_universe$data_is_on_r_univ, "data.frame")

    # Test dashboard_data_maintainers
    maintainers <- dashboard_data_maintainers (contributors)
    expect_type (maintainers, "list")
    expect_named (maintainers, c ("maintainers", "comaintainers"))
    # maintainers$maintainers is a list named by login
    expect_type (maintainers$maintainers, "list")

    # Test dashboard_data_releases
    releases <- dashboard_data_releases (org_data)
    expect_s3_class (releases, "data.frame")
    expect_named (releases, c ("package", "latest", "total", "rel_per_year"))

    fs::dir_delete (org_dir)
})

end_date <- as.Date ("2024-08-01")

test_that ("collate across orgs", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    dat <- mock_rm_data ()
    path <- generate_test_pkg ()

    # Then mock one set of org data:
    metrics_all <- withr::with_options (
        list ("repometrics_period" = 365.25 / 2),
        metrics_over_end_dates (path, end_date = end_date, num_years = 1)
    )
    models_all <- withr::with_options (
        list ("repometrics_period" = 365.25 / 2),
        models_over_end_dates (path, end_date = end_date, num_years = 1)
    )
    dat <- list (
        repo = repometrics::repometrics_data_repo (path, num_cores = 1L),
        metrics = metrics_all,
        models = models_all
    )

    org_dir <- fs::path (fs::path_temp (), "org")
    if (!fs::dir_exists (org_dir)) {
        fs::dir_create (org_dir)
    }
    path1 <- fs::dir_copy (path, fs::path (org_dir, "testpkg1"))
    path2 <- fs::dir_copy (path, fs::path (org_dir, "testpkg2"))
    fs::dir_delete (path)

    d1 <- desc::desc_set ("Package" = "testpkg1", file = path1)
    d2 <- desc::desc_set ("Package" = "testpkg2", file = path2)

    saveRDS (dat, fs::path (fs::path_temp (), "testpkg1.Rds"))
    saveRDS (dat, fs::path (fs::path_temp (), "testpkg2.Rds"))

    # Then the main call, which loads those pre-saved data rather than
    # re-generating:
    org_data <- repometrics_collate_org_data (
        org_dir,
        end_date = end_date,
        num_years = 1
    )

    fs::dir_delete (c (path1, path2))

    expect_type (org_data, "list")
    expect_named (org_data, c ("repos", "metrics", "models"))
    npkgs <- 2L
    expect_length (org_data$repos, npkgs)
    expect_length (org_data$metrics, npkgs)
    expect_s3_class (org_data$models, "data.frame")
    n_periods <- length (get_end_date_seq (end_date = end_date, num_years = 1))
    expect_equal (nrow (org_data$models), n_periods)
})

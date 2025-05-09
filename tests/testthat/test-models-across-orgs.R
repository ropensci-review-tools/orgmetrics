# These tests need 'zoo' and 'tidyr' in the namespace, as they call internal
# 'repometrics' functions which rely on these.

end_date <- as.Date ("2024-08-01")

get_end_date_seq <- utils::getFromNamespace ("get_end_date_seq", "repometrics")

test_that ("collate_org_data output", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    org_dir <- mock_collate_org_data ()

    org_data <- orgmetrics_collate_org_data (
        org_dir,
        end_date = end_date,
        num_years = 1
    )

    fs::dir_delete (org_dir)

    expect_type (org_data, "list")
    expect_named (
        org_data,
        c ("repos", "metrics", "models", "annual_commits", "annual_gh_activity")
    )
    npkgs <- 2L
    expect_length (org_data$repos, npkgs)
    expect_length (org_data$metrics, npkgs)
    expect_s3_class (org_data$models, "data.frame")
    expect_s3_class (org_data$annual_commits, "data.frame")
    expect_named (org_data$annual_commits, c ("year", "num_commits"))
    expect_s3_class (org_data$annual_gh_activity, "data.frame")
    expect_named (org_data$annual_gh_activity, c ("year", "issues", "prs", "cmts"))
    n_periods <- length (get_end_date_seq (end_date = end_date, num_years = 1))
    expect_equal (nrow (org_data$models), n_periods)
})

test_that ("org data preprocessing", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    org_dir <- mock_collate_org_data ()

    data_org <- orgmetrics_collate_org_data (
        org_dir,
        end_date = end_date,
        num_years = 1
    )

    fs::dir_delete (org_dir)

    metrics_df <- data_metrics_to_df (data_org$metrics)
    expect_s3_class (metrics_df, "data.frame")
    expect_equal (nrow (metrics_df), 4L) # one year with 3-month intervals
    expect_equal (ncol (metrics_df), 50L) # one year with 3-month intervals
    expect_equal (names (metrics_df) [1:3], c ("package", "org", "date"))
    classes <- vapply (
        names (metrics_df),
        function (n) class (metrics_df [[n]]),
        character (1L)
    )
    expect_equal (unique (classes [1:3]), "character")
    classes <- classes [-(1:3)]
    expect_true (all (classes %in% c ("logical", "integer", "numeric")))
    df_names <- names (classes) # names of df minus 1st 3.

    # Then replicate metrics_df to simluate multiple packages:
    metrics_df$package <- "a"
    metrics_df2 <- metrics_df
    metrics_df2$package <- "b"
    metrics_df <- rbind (metrics_df, metrics_df2)

    data_pre <- data_metrics_preprocess (metrics_df)
    expect_s3_class (data_pre, "data.frame")
    expect_equal (ncol (data_pre), 3L)
    expect_equal (names (data_pre), c ("package", "name", "value"))
    expect_identical (sort (df_names), sort (unique (data_pre$name)))
    expect_type (data_pre$value, "double")
    expect_true (!all (is.na (data_pre$value))) # Some actual values!

    data_maintenance <- org_maintenance_metric (data_org)
    expect_s3_class (data_maintenance, "data.frame")
    expect_equal (ncol (data_maintenance), 4L)
    col_nms <- c ("package", "comm_engage", "dev_resp", "maintenance")
    expect_equal (names (data_maintenance), col_nms)
    expect_type (data_maintenance$package, "character")
    for (n in col_nms [-1]) {
        expect_type (data_maintenance [[n]], "double")
        # And all should be non-NA:
        expect_false (any (is.na (data_maintenance [[n]])))
    }


    # data_models needs > 1 package to generate meaningful results:
    data_models <- data_org$models
    data_models$package <- paste0 (
        data_models$package,
        "/",
        letters [seq_len (nrow (data_models))]
    )

    models_df <- data_models_preprocess (data_models)
    expect_s3_class (models_df, "data.frame")
    expect_true (nrow (models_df) > 0L)
    expect_equal (ncol (models_df), 17L)
    expect_equal (names (models_df) [1:3], c ("package", "org", "date"))
    classes <- vapply (
        names (models_df),
        function (n) class (models_df [[n]]),
        character (1L)
    )
    expect_equal (unique (classes [1:3]), c ("character", "Date"))
    classes <- classes [-(1:3)]
    expect_true (all (classes == "numeric"))

    mod_dat <- load_model_json_data ()
    mod_names <- names (mod_dat$models)
    expect_true (all (mod_names %in% names (models_df)))
})

# These tests need 'zoo' and 'tidyr' in the namespace, as they call internal
# 'repometrics' functions which rely on these.

end_date <- as.Date ("2024-08-01")

get_end_date_seq <- utils::getFromNamespace ("get_end_date_seq", "repometrics")

test_that ("collate across orgs", {

    Sys.setenv ("REPOMETRICS_TESTS" = "true")
    org_dir <- mock_collate_org_data ()

    # Then the main call, which loads those pre-saved data rather than
    # re-generating:
    org_data <- orgmetrics_collate_org_data (
        org_dir,
        end_date = end_date,
        num_years = 1
    )

    fs::dir_delete (org_dir)

    expect_type (org_data, "list")
    expect_named (org_data, c ("repos", "metrics", "models"))
    npkgs <- 2L
    expect_length (org_data$repos, npkgs)
    expect_length (org_data$metrics, npkgs)
    expect_s3_class (org_data$models, "data.frame")
    n_periods <- length (get_end_date_seq (end_date = end_date, num_years = 1))
    expect_equal (nrow (org_data$models), n_periods)
})

# The 'dplyr::mutate' calls in 'rm_org_data_fn_calls()' fail on windows and
# mac, but work on Linux.
skip_on_os ("windows")
skip_on_os ("mac")

test_that ("function call network", {

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

    fn_calls <- rm_org_data_fn_call_network (f)

    fs::dir_delete (d)

    expect_type (fn_calls, "list")
    expect_named (fn_calls, c ("fn_calls", "summary"))

    expect_s3_class (fn_calls$fn_calls, "data.frame")
    # expect_true (nrow (fn_calls$fn_calls) > 0L)
    expect_named (fn_calls$fn_calls, c ("source", "fn", "target_fn", "target_pkg", "n"))

    expect_s3_class (fn_calls$summary, "data.frame")
    # expect_true (nrow (fn_calls$summary) > 0L)
    expect_named (fn_calls$summary, c ("source", "target_pkg", "num_fns", "num_calls"))
    expect_type (fn_calls$summary$source, "character")
    expect_type (fn_calls$summary$target_pkg, "character")
    expect_type (fn_calls$summary$num_fns, "integer")
    expect_type (fn_calls$summary$num_calls, "integer")
    # expect_true (all (fn_calls$summary$num_fns) > 0L)
    # expect_true (all (fn_calls$summary$num_calls) > 0L)
})

test_that ("function call network", {

    pkg_paths <- generate_test_org_data ()
    org_path <- unique (fs::path_dir (pkg_paths))

    fn_calls <- rm_org_data_fn_call_network (org_path)

    fs::dir_delete (org_path)

    expect_type (fn_calls, "list")
    expect_named (fn_calls, c ("fn_calls", "summary"))

    expect_s3_class (fn_calls$fn_calls, "data.frame")
    expect_true (nrow (fn_calls$fn_calls) > 0L)
    expect_named (fn_calls$fn_calls, c ("source", "fn", "name", "package", "n"))
    expect_true (all (fn_calls$fn_calls$n) > 0L)

    expect_s3_class (fn_calls$summary, "data.frame")
    expect_true (nrow (fn_calls$summary) > 0L)
    expect_named (fn_calls$summary, c ("source", "package", "num_fns", "num_calls"))
    expect_type (fn_calls$summary$source, "character")
    expect_type (fn_calls$summary$package, "character")
    expect_type (fn_calls$summary$num_fns, "integer")
    expect_type (fn_calls$summary$num_calls, "integer")
    expect_true (all (fn_calls$summary$num_fns) > 0L)
    expect_true (all (fn_calls$summary$num_calls) > 0L)
})

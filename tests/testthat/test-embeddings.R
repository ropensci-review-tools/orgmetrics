test_that ("embeddings fns", {

    npkgs <- 10L
    emb <- get_test_embeddings (npkgs = npkgs, seed = 1L)

    d <- rm_org_emb_distances (embeddings_data = emb, what = "code")
    expect_type (d, "double")
    expect_true ("dim" %in% names (attributes (d)))
    expect_equal (attr (d, "dim"), rep (npkgs, 2L))
    expect_true (all (is.na (diag (d))))
})

rm_org_data_embeddings <- function (pkgs_json) {

    requireNamespace ("pkgmatch", quietly = TRUE)
    requireNamespace ("jsonlite", quietly = TRUE)

    # Suppress no visible binding notes:
    is_r_pkg <- NULL

    pkgs_dat <- jsonlite::read_json (pkgs_json, simplify = TRUE) |>
        dplyr::filter (is_r_pkg) |>
        update_pj_path (fs::path_dir (pkgs_json))

    ollama_running <- tryCatch (
        pkgmatch::ollama_check (),
        error = function (e) FALSE
    )
    if (ollama_running) {
        embeddings <- pkgmatch::pkgmatch_embeddings_from_pkgs (pkgs_dat$path)
        ret <- embeddings_to_similarities (embeddings)
    } else {
        ret <- pkg_bm25 (pkgs_dat)
    }

    return (ret)
}

embeddings_to_similarities <- function (embeddings) {

    npkgs <- ncol (embeddings [[1]])

    lapply (embeddings, function (emb_type) {
        simil <- lapply (seq_len (npkgs), function (i) {
            cosine_similarity (emb_type [, i], emb_type)
        })
        do.call (rbind, simil)
    })
}

rm_org_emb_distances <- function (org_paths, embeddings_data = NULL, what = "code") {

    what <- match.arg (what, c ("text_with_fns", "text_wo_fns", "code"))

    if (is.null (embeddings_data)) {
        embeddings_data <- rm_org_data_embeddings (org_paths)
    } else {
        check_embeddings_param (embeddings_data)
    }

    this_mat <- embeddings_data [[what]]
    npkgs <- ncol (embeddings_data [[what]])
    embeddings <- lapply (seq_len (npkgs), function (i) {
        this_vec <- this_mat [, i]
        cosine_similarity (this_vec, this_mat)
    })
    emb_matrix <- do.call (cbind, embeddings)
    diag (emb_matrix) <- NA_real_

    return (emb_matrix)
}

# Adapted from pkgmatch/R/similarity-metrics.R, but to just return vector of
# similarities.
cosine_similarity <- function (this_vec, this_mat) {

    nrow <- length (this_vec)
    ncol <- ncol (this_mat)
    emb_mat <- matrix (this_vec, nrow = nrow, ncol = ncol)

    cs_num <- colSums (emb_mat * this_mat)
    cs_denom <- sqrt (colSums (emb_mat^2) * colSums (this_mat^2))
    cs <- cs_num / cs_denom

    return (cs)
}

pkg_bm25 <- function (pkgs_dat) {

    # suppress no visible binding note:
    n <- NULL

    # These must be inside this fn, to ensure 'requireNsmespace()' has been
    # called.
    get_pkg_text <- utils::getFromNamespace ("get_pkg_text", "pkgmatch")
    bm25_idf <- utils::getFromNamespace ("bm25_idf", "pkgmatch")
    bm25_tokens_list <- utils::getFromNamespace ("bm25_tokens_list", "pkgmatch")
    rcpp_bm25 <- utils::getFromNamespace ("rcpp_bm25", "pkgmatch")

    txt <- lapply (pkgs_dat$path, get_pkg_text)
    tokens_idf <- bm25_idf (txt)
    tokens_list <- bm25_tokens_list (txt)

    ntoks_list <- vapply (tokens_list, function (i) sum (i$n), integer (1L))
    ntoks_avg <- mean (ntoks_list)

    pkg_bm25_one <- function (toks_i, tokens_list, tokens_idf, ntoks_avg) {
        tokens_i <- dplyr::rename (toks_i, np = n)
        rcpp_bm25 (tokens_idf, tokens_list, tokens_i, ntoks_avg)
    }
    bm25 <- do.call (
        rbind,
        lapply (
            tokens_list,
            function (i) pkg_bm25_one (i, tokens_list, tokens_idf, ntoks_avg)
        )
    )
    rownames (bm25) <- colnames (bm25) <- pkgs_dat$package
    bm25 <- apply (bm25, 1, function (i) i / max (i))

    list (text_with_fns = bm25, text_wo_fns = bm25, code = bm25)
}

check_embeddings_param <- function (embeddings_data) {

    expected_emb_len <- 768L
    checkmate::expect_list (embeddings_data)
    checkmate::expect_names (
        names (embeddings_data),
        identical.to = c ("text_with_fns", "text_wo_fns", "code")
    )
    nrow <- unique (vapply (embeddings_data, nrow, integer (1L)))
    if (length (nrow) > 1L) {
        cli::cli_abort ("embeddings data must have same numbers of rows")
    }
    if (nrow != expected_emb_len) {
        cli::cli_abort ("embeddings data should have {expected_emb_len} rows")
    }
    ncol <- unique (vapply (embeddings_data, ncol, integer (1L)))
    if (length (ncol) > 1L) {
        cli::cli_abort ("embeddings data must have same numbers of columns")
    }
}

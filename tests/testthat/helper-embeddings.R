# Adapted from file of same name in pkgmatch pkg
mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

npkgs <- 10L
expected_embedding_length <- 768
n <- npkgs * expected_embedding_length

get_test_embeddings <- function (npkgs,
                                 embedding_len = expected_embedding_length,
                                 seed = 1L) {

    set.seed (seed)

    n <- npkgs * embedding_len

    pkg_nms <- vapply (seq_len (npkgs), function (i) mknm (), character (1L))

    emb_code <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    emb_txt2 <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    emb_txt1 <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    colnames (emb_txt1) <- colnames (emb_txt2) <- colnames (emb_code) <- pkg_nms

    list (text_with_fns = emb_txt1, text_wo_fns = emb_txt2, code = emb_code)
}

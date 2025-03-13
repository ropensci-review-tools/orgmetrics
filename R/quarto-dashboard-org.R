#' Start quarto dashboard with results of
#' `orgmetrics_collate_org_data` function for collation of data across orgs.
#'
#' @param data_org Data on GitHub organization as returned from
#' `orgmetrics_collate_org_data` function.
#' @param fn_calls Data on function calls between packages of the specified
#' organization, as returned from the `rm_org_data_fn_call_network()` function.
#' @param action One of "preview", to start and open a live preview of the
#' dashboard website, or "render" to render a static version without previewing
#' or opening.
#' @param emb_matrix Matrix of embeddings between packages, returned from
#' `rm_org_emb_distances()` function.
#' @return (Invisibly) Path to main "index.html" document of quarto site. Note
#' that the site must be served with `action = "preview"`, and will not work by
#' simply opening this "index.html" file.
#' @export
orgmetrics_dashboard <- function (data_org, fn_calls, emb_matrix, action = "preview") {

    # Suppress no visible binding notes:
    org <- package <- NULL

    data_models <- data_models_preprocess (data_org$models) |>
        dplyr::select (-org, -date) |>
        tidyr::pivot_longer (-package)

    requireNamespace ("jsonlite")
    requireNamespace ("quarto")
    requireNamespace ("withr")

    action <- match.arg (action, c ("preview", "render"))
    quarto_action <- paste0 ("quarto::quarto_", action)

    path_src <- system.file ("extdata", "quarto", package = "orgmetrics")
    path_dest <- fs::path (fs::path_temp (), "quarto")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)
    saveRDS (data_models, fs::path (dir, "results-models.Rds"))
    saveRDS (fn_calls, fs::path (dir, "fn-calls.Rds"))
    saveRDS (emb_matrix, fs::path (dir, "emb-matrix.Rds"))

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}

#' Pre-process organization data by converting all model values to standard
#' z-scores, retrieving the latest value only for each package, and generating
#' a "final" score from the sum across all model scores. Higher values of this
#' final score are better than lower values.
#' @noRd
data_models_preprocess <- function (data_models) {

    # Suppress no visible binding notes:
    package <- final <- NULL

    data_models |>
        dplyr::mutate (
            dplyr::across (dplyr::where (is.numeric), ~ scale (.) [, 1])
        ) |>
        dplyr::group_by (package) |>
        dplyr::slice_head (n = 1L) |>
        dplyr::mutate (org = gsub ("\\/.*$", "", package), .after = package) |>
        dplyr::mutate (package = gsub ("^.*\\/", "", package)) |>
        dplyr::mutate (
            final = sum (dplyr::across (dplyr::where (is.numeric))),
            .after = "date"
        ) |>
        dplyr::arrange (dplyr::desc (final))
}

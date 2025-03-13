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
    data_metrics <- data_metrics_to_df (data_org$metrics) |>
        data_metrics_preproces ()

    requireNamespace ("jsonlite")
    requireNamespace ("quarto")
    requireNamespace ("withr")

    action <- match.arg (action, c ("preview", "render"))
    quarto_action <- paste0 ("quarto::quarto_", action)

    path_src <- system.file ("extdata", "quarto", package = "orgmetrics")
    path_dest <- fs::path (fs::path_temp (), "quarto")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)
    saveRDS (data_models, fs::path (dir, "results-models.Rds"))
    saveRDS (data_metrics, fs::path (dir, "results-metrics.Rds"))
    saveRDS (fn_calls, fs::path (dir, "fn-calls.Rds"))
    saveRDS (emb_matrix, fs::path (dir, "emb-matrix.Rds"))

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}

#' Convert metrics data to rectangular `data.frame`.
#'
#' @param data_metrics An embedded list of [package] [date stamp] [metric],
#' where some metrics are vectors of length > 1 which need to be reduced to
#' single value.
#' @return Single `data.frame` of metrics with aditional columns of [org, repo,
#' date].
#'
#' @noRd
data_metrics_to_df <- function (data_metrics) {

    # Convert vectors to single values:
    nms_counts <- c ("watchers", "issues", "prs") # committer_count
    nms_msms <- c ("mean", "sd", "median", "sum") # pr_cmt_count, response_time
    data_metrics <- lapply (data_metrics, function (i) {
        lapply (i, function (j) {
            lapply (j, function (k) {
                if (identical (names (k), nms_counts)) {
                    k <- k [["watchers"]] # committer_count
                } else if (identical (names (k), nms_msms)) {
                    k <- k [["mean"]] # pr_cmt_count, response_time
                } else if (is.null (names (k)) && length (k) > 1L) {
                    k <- mean (as.numeric (k))
                } else if (is.null (names (k)) && length (k) == 0L) {
                    k <- NA_real_
                }
                return (k)
            })
        })
    })
    # Then add pkg name and datestamp columns:
    data_metrics_df <- lapply (seq_along (data_metrics), function (i) {
        df <- lapply (seq_along (data_metrics [[i]]), function (j) {
            data.frame (
                org = gsub ("\\/.*$", "", names (data_metrics) [i]),
                package = gsub ("^.*\\/", "", names (data_metrics) [i]),
                date = names (data_metrics [[i]]) [j],
                as.data.frame (data_metrics [[i]] [[j]])
            )
        })
        do.call (rbind, df)
    })
    do.call (rbind, data_metrics_df) |>
        dplyr::arrange (package, dplyr::desc (date))
}

#' Convert `data.frame` of metrics data returned from `data_metrics_to_df` to
#' long-form tibble reduced to re-scaled values of latest metrics only for each
#' package.
#' @noRd
data_metrics_preproces <- function (data_metrics) {

    data_metrics |>
        dplyr::mutate (
            dplyr::across (dplyr::where (is.logical), as.numeric)
        ) |>
        dplyr::mutate (
            dplyr::across (dplyr::where (is.numeric), ~ scale (.) [, 1])
        ) |>
        dplyr::group_by (package) |>
        dplyr::slice_head (n = 1L) |>
        dplyr::select (-org, -date) |>
        tidyr::pivot_longer (-package)
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

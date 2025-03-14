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
    data_maintenance <- org_maintenance_metric (data_org)

    # Plus mapping from repos to org/repo:
    data_repo_src <- names (data_org$repos)
    data_repo_src <- data.frame (
        package = gsub ("^.*\\/", "", data_repo_src),
        orgrepo = data_repo_src,
        url = paste0 ("https://github.com/", data_repo_src)
    )

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
    saveRDS (data_maintenance, fs::path (dir, "results-maintenance-org.Rds"))
    saveRDS (data_repo_src, fs::path (dir, "results-data-repo-src.Rds"))
    saveRDS (fn_calls, fs::path (dir, "fn-calls.Rds"))
    saveRDS (emb_matrix, fs::path (dir, "emb-matrix.Rds"))

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}

# functions to identify maintenance deficits at both org and repo levels

load_model_json_data <-
    utils::getFromNamespace ("load_model_json_data", "repometrics")

#' A single metric for each repo of the maintenance deficit, identifying repos
#' with high community engagement yet low developer responsiveness.
#' @noRd
org_maintenance_metric <- function (data_org) {

    # Suppress no visible binding notes:
    value <- name <- package <- comm_engage <- dev_resp <- maintenance <- NULL

    data_metrics <- data_metrics_to_df (data_org$metrics) |>
        data_metrics_preproces ()

    mod_dat <- load_model_json_data ()$models
    stopifnot (all (models_comm_engage %in% names (mod_dat)))
    stopifnot (all (models_dev_resp %in% names (mod_dat)))

    metrics_comm_engage <- unique (unlist (lapply (
        models_comm_engage,
        function (h) mod_dat [[h]]
    )))
    metrics_dev_resp <- unique (unlist (lapply (
        models_dev_resp,
        function (h) mod_dat [[h]]
    )))

    data_metrics_comm_engage <- data_metrics |>
        dplyr::filter (name %in% metrics_comm_engage) |>
        dplyr::group_by (package) |>
        dplyr::summarise (comm_engage = mean (value, na.rm = TRUE))
    data_metrics_dev_resp <- data_metrics |>
        dplyr::filter (name %in% metrics_dev_resp) |>
        dplyr::group_by (package) |>
        dplyr::summarise (dev_resp = mean (value, na.rm = TRUE))
    data_metrics_maintenance <-
        dplyr::left_join (
            data_metrics_comm_engage,
            data_metrics_dev_resp,
            by = dplyr::join_by (package)
        ) |>
        dplyr::mutate (maintenance = comm_engage - dev_resp) |>
        dplyr::arrange (dplyr::desc (maintenance))

    return (data_metrics_maintenance)
}

# These model scores should be high for packages in need of maintenance:
models_comm_engage <- c (
    "community_activity",
    "proj_awareness",
    "viability_community"
)

# And these will be high for well-maintained packages, but low otherwise:
models_dev_resp <- c (
    "proj_engagement",
    "dev_responsiveness",
    "collab_devel_index",
    "comm_welcoming"
)

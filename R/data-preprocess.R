# Pre-processing routines using the collated org data as input, and returning
# data then pass to dashboard functions.

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

    # Suppress no visible binding notes:
    pr_reviews_approved <- pr_revs_approved <- NULL

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
                package = gsub ("^.*\\/", "", names (data_metrics) [i]),
                org = gsub ("\\/.*$", "", names (data_metrics) [i]),
                date = names (data_metrics [[i]]) [j],
                as.data.frame (data_metrics [[i]] [[j]])
            )
        })
        do.call (rbind, df)
    })

    # Suppress no visible binding note:
    package <- NULL
    data_metrics_df <- do.call (rbind, data_metrics_df) |>
        dplyr::arrange (package, dplyr::desc (date))

    # And until data are updated to new repometric structure, need to manually
    # remove these columns:
    if (all (c ("pr_revs_approved", "pr_reviews_approved") %in%
        names (data_metrics_df))) {
        data_metrics_df <- data_metrics_df |>
            dplyr::select (-pr_reviews_approved) |>
            dplyr::rename (pr_reviews_approved = pr_revs_approved)
    }

    return (data_metrics_df)
}

#' Convert `data.frame` of metrics data returned from `data_metrics_to_df` to
#' long-form tibble reduced to re-scaled values of latest metrics only for each
#' package. Values are scaled and converted according to the specifications in
#' repometrics JSON data, and then rescaled so that returned values are all on
#' same scales, with higher values being better than lower values.
#'
#' @noRd
data_metrics_preprocess <- function (data_metrics, longer = TRUE) {

    # Suppress no visible binding notes:
    org <- date <- package <- airtable_name <- description <- url <-
        field_group <- name <- value <- allna <-
        minval <- minval0 <- minval1 <- better <- what <- NULL

    metric_direction <- load_model_json_data ()$metrics |>
        dplyr::select (-airtable_name, -description, -url, -field_group)

    metrics <- data_metrics |>
        dplyr::mutate (
            dplyr::across (dplyr::where (is.logical), as.numeric)
        ) |>
        dplyr::group_by (package) |>
        dplyr::slice_head (n = 1L) |>
        dplyr::select (-org, -date) |>
        tidyr::pivot_longer (-package) |>
        dplyr::left_join (metric_direction, by = "name")
    # libyears is log-scaled, but can be negative
    i <- which (metrics$name == "libyears")
    metrics$value [i] <-
        metrics$value [i] - min (metrics$value [i], na.rm = TRUE)

    # Use random metrics values for tests:
    is_test_env <- Sys.getenv ("ORGMETRICS_TESTS") == "true"
    if (is_test_env) {
        set.seed (1L)
        metrics$value <- stats::runif (nrow (metrics))
    }
    # Remove any metrics which have no non-NA values, or all identical values
    chk <- metrics |>
        dplyr::group_by (name) |>
        dplyr::summarise (allna = dplyr::if_else (
            all (is.na (value)) ||
                dplyr::n_distinct (value, na.rm = TRUE) == 1L,
            TRUE, FALSE
        )) |>
        dplyr::filter (!allna)
    metrics <- metrics |> dplyr::filter (name %in% chk$name)

    # Then rescale, first establishing min value as half of the first value > 0
    # if that exists. This ensures that log-scaled distributions are
    # appropriately spaced.
    metrics <- metrics |>
        dplyr::group_by (name) |>
        dplyr::mutate (minval0 = min (value, na.rm = TRUE)) |>
        dplyr::mutate (minval1 = sort (unique (value)) [2]) |>
        dplyr::mutate (
            minval = dplyr::if_else (!is.na (minval1), minval1 / 2, minval0)
        ) |>
        dplyr::select (-minval0, -minval1) |>
        dplyr::mutate (value = dplyr::if_else (
            (is.na (value) | is.nan (value) | value == 0), minval, value
        )) |>
        dplyr::select (-minval) |>
        dplyr::mutate (value = dplyr::if_else (
            value == 0 & scale == "log", 0.001, value
        )) |>
        dplyr::mutate (value = dplyr::if_else (
            scale == "log", log10 (value), value
        )) |>
        dplyr::mutate (value = scale (value) [, 1]) |>
        dplyr::mutate (value = dplyr::if_else (
            better == "lower", -value, value
        )) |>
        dplyr::mutate (value = if (all (is.na (value))) {
            NA_real_
        } else {
            (value - min (value, na.rm = TRUE)) / diff (range (value, na.rm = TRUE))
        }) |>
        dplyr::select (-what, -scale, -better) |>
        dplyr::ungroup ()

    if (!longer) {
        # Table output for airtable
        requireNamespace ("tidyr", quietly = TRUE)

        metrics <- tidyr::pivot_wider (
            metrics,
            names_from = name,
            values_from = value
        )
    }


    return (metrics)
}

#' Group metrics into categories defined in repometrics JSON schema
#'
#' @param metrics_all Output of `data_metrics_to_df() |> data_metrics_preprocess()`
#' @noRd
data_metrics_group <- function (metrics_all) {

    # Suppress no visible binding notes:
    package <- field_group <- NULL

    metrics_metadata <- load_model_json_data ()$metrics
    index <- which (metrics_metadata$name %in% names (metrics_all))
    metrics_metadata <- metrics_metadata [index, ]

    field_groups <- unique (metrics_metadata$field_group)
    grouped_metrics <- lapply (field_groups, function (group) {
        metrics_in <- metrics_metadata |> dplyr::filter (field_group == group)
        metrics_out <- metrics_metadata |> dplyr::filter (field_group != group)
        data_group <- metrics_all |>
            dplyr::select (-dplyr::all_of (metrics_out$name), -package) |>
            as.matrix ()
        rowMeans (data_group, na.rm = TRUE)
    })
    grouped_metrics <- cbind (metrics_all$package, data.frame (do.call (cbind, grouped_metrics)))
    names (grouped_metrics) <- c ("package", field_groups)
    grouped_metrics$total <- rowMeans (grouped_metrics [, -1])

    return (grouped_metrics)
}

#' Pre-process organization data by converting all model values to standard
#' z-scores, retrieving the latest value only for each package, and generating
#' a "final" score from the sum across all model scores. Higher values of this
#' final score are better than lower values.
#' @noRd
data_models_preprocess <- function (data_models) {

    # Suppress no visible binding notes:
    package <- final <- NULL

    dm <- data_models |>
        dplyr::group_by (package) |>
        dplyr::slice_head (n = 1L) |>
        dplyr::mutate (org = gsub ("\\/.*$", "", package), .after = package) |>
        dplyr::mutate (package = gsub ("^.*\\/", "", package))

    if (nrow (dm) > 1L) {
        is_numeric <- vapply (
            names (dm),
            function (n) is.numeric (dm [[n]]),
            logical (1L)
        )
        index <- which (is_numeric)
        dm [, index] <- scale (dm [, index])
        dm [, index] <- apply (
            dm [, index],
            2,
            function (i) (i - min (i)) / diff (range (i))
        )
    }

    dm |>
        dplyr::mutate (
            final = sum (dplyr::across (dplyr::where (is.numeric))),
            .after = "date"
        ) |>
        dplyr::arrange (dplyr::desc (final))
}

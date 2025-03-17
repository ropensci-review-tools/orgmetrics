match_string_vecs <- utils::getFromNamespace ("match_string_vecs", "repometrics")

#' Extract data on contributor absence
#'
#' For contributors who have been entirely absent in recent commits, this
#' returns the overall proportion of their commits. For those present in recent
#' logs, it returns the ratio of historical to recent commit proportions.
#' Maintainer absence is then reflects in high values of this, indicating
#' people with little historical commits now making many more.
#'
#' @param data_org Org-level data, from `orgmetrics_collate_org_data()`.
#' @param end_date Date to end analyses
#' @param period Period of recent analysis, in days.
#' @param threshold_change For contributors present in recent log, a threshold
#' above which the ratio of recent-to-historical commits should be kept in
#' output.
#' @param threshold_abs For contributors absent from recent log, a threshold of
#' total commit proportion above which contributors should be kept in output.
#'
#' @return A `data.frame` with a "repo" column, and for each repo the names of
#' contributors, a "what" column indicating either "change" (present in recent
#' log), or "absence" (not present), and a "measure" of absence.
#'
#' @noRd
ctb_absence <- function (data_org,
                         end_date = Sys.Date (),
                         period = 365,
                         threshold_change = 2,
                         threshold_abs = 0.2) {

    checkmate::assert_date (end_date, len = 1L)
    checkmate::assert_numeric (period, len = 1L, lower = 7)

    start_date <- end_date - as.integer (period)

    abs <- lapply (data_org$repos, function (repo) {

        ctbs_gh <- repo$rm$contribs_from_gh_api |>
            dplyr::filter (login != "actions-user")
        ctbs_log <- repo$rm$contribs_from_log |>
            dplyr::filter (!grepl ("github", handle, ignore.case = TRUE))
        log <- repo$rm$gitlog |>
            dplyr::filter (!grepl ("github", aut_name, ignore.case = TRUE))

        # Reconcile names:
        nms <- unique (log$aut_name)
        nms_norm <- match_string_vecs (nms, nms)
        log$aut_name <- nms_norm [match (log$aut_name, nms)]

        log_ctbs_all <- sort (table (log$aut_name), decreasing = TRUE)
        log_ctbs_all <- log_ctbs_all / sum (log_ctbs_all)

        log_recent <- dplyr::mutate (log, date = as.Date (timestamp)) |>
            dplyr::filter (date >= start_date & date <= end_date)
        log_ctbs_recent <- sort (table (log_recent$aut_name), decreasing = TRUE)
        log_ctbs_recent <- log_ctbs_recent / sum (log_ctbs_recent)

        index <- match (names (log_ctbs_recent), names (log_ctbs_all))
        ctb_change <- log_ctbs_all [index] / log_ctbs_recent
        index <- which (!names (log_ctbs_all) %in% names (log_ctbs_recent))
        ctb_abs <- log_ctbs_all [index]

        data.frame (
            repo = repo$rm$repo_from_gh_api$name,
            name = c (names (ctb_change), names (ctb_abs)),
            measure = c (as.numeric (ctb_change), as.numeric (ctb_abs)),
            what = c (rep ("change", length (ctb_change)), rep ("absence", length (ctb_abs)))
        )
    })

    abs <- do.call (rbind, abs)
    rownames (abs) <- NULL

    abs <- dplyr::filter (
        abs,
        (what == "change" & measure >= threshold_change) |
            (what == "absence" & measure >= threshold_abs)
    )

    return (abs)
}

issue_responses <- function (data_org,
                             end_date = Sys.Date (),
                             period = 365) {

    start_date <- end_date - as.integer (period)

    response_dat_all <- lapply (data_org$repos, function (repo) {

        issues <- repo$rm$issues_from_gh_api
        prop_labelled <- length (which (nzchar (issues$labels))) / nrow (issues)
        ctbs_gh <- repo$rm$contribs_from_gh_api
        n_ctbs <- cumsum (ctbs_gh$contributions / sum (ctbs_gh$contributions))
        ctbs_main <- ctbs_gh$login [which (n_ctbs <= 0.9)]

        issues_others <- issues |> dplyr::filter (!user_login %in% ctbs_main)
        issue_cmts <- repo$rm$issue_comments_from_gh_api |>
            dplyr::filter (issue_number %in% issues_others$number)
        # Put issue author column on cmts:
        index <- match (issue_cmts$issue_number, issues_others$number)
        issue_cmts$issue_author <- issues_others$user_login [index]
        issue_cmts <- dplyr::filter (issue_cmts, user_login != issue_author)
        issue_cmt_first <- dplyr::group_by (issue_cmts, issue_number) |>
            dplyr::slice_head (n = 1L)
        index <- match (issue_cmt_first$issue_number, issues_others$number)
        issue_cmt_first$opened_date <-
            as.Date (issues_others$created_at [index])
        issue_cmt_first <- dplyr::mutate (
            issue_cmt_first,
            created_at = as.Date (created_at)
        ) |>
            dplyr::select (-c (user_id, updated_at, issue_body)) |>
            dplyr::mutate (
                response_duration =
                    difftime (created_at, opened_date, units = "days")
            )

        data.frame (
            repo = rep (repo$rm$repo_from_gh_api$name, nrow (issue_cmt_first)),
            issue_number = issue_cmt_first$issue_number,
            created_at = issue_cmt_first$opened_date,
            responded_at = issue_cmt_first$created_at,
            response_duration = issue_cmt_first$response_duration
        )
    })

    resp_dur <- do.call (rbind, response_dat_all) |>
        dplyr::filter (created_at >= start_date & responded_at <= end_date) |>
        dplyr::group_by (repo) |>
        dplyr::summarise (response = as.integer (mean (response_duration, na.rm = TRUE))) |>
        dplyr::arrange (dplyr::desc (response))

    return (resp_dur)
}

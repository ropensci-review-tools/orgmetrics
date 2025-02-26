gitlog_unique_contributors <- function (path, start_date, end_date) {

    # Suppress no visible binding note:
    timestamp <- aut_name <- aut_email <- nfiles_changed <- lines_added <-
        lines_removed <- index <- ncommits <- NULL

    log <- rm_data_gitlog (path) |>
        dplyr::mutate (date = as.Date (timestamp)) |>
        dplyr::filter (date >= start_date & date <= end_date) |>
        dplyr::filter (aut_name != "GitHub") |>
        dplyr::group_by (aut_name, aut_email) |>
        dplyr::summarise (
            ncommits = dplyr::n (),
            nfiles_changed = sum (nfiles_changed),
            lines_added = sum (lines_added),
            lines_removed = sum (lines_removed)
        )

    log$index <- index_partial_duplicates (log) # in utils-author-matches.R
    # Then use that index to group all unique contributors:
    dplyr::group_by (log, index) |>
        dplyr::summarise (
            aut_name = dplyr::first (aut_name),
            aut_email = dplyr::first (aut_email),
            ncommits = sum (ncommits),
            nfiles_changed = sum (nfiles_changed),
            lines_changed = sum (lines_added + lines_removed)
        ) |>
        dplyr::select (-index)

}

user_from_gh_api <- function (user) {

    u_base <- "https://api.github.com/users/"
    u_endpoint <- paste0 (u_base, user)

    req <- httr2::request (u_endpoint) |>
        add_gh_token_to_req ()
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    body <- httr2::resp_body_json (resp)

    data.frame (
        login = body$login,
        ctb_id = body$id,
        name = null2na_char (body$name),
        company = null2na_char (body$company),
        email = null2na_char (body$email),
        location = null2na_char (body$location),
        blog = null2na_char (body$blog),
        bio = null2na_char (body$bio),
        public_repos = body$public_repos,
        followers = body$followers,
        following = body$following,
        created_at = body$created_at,
        updated_at = body$updated_at
    )
}

gh_user_general <- utils::getFromNamespace ("gh_user_general", "repometrics")


#' Add full data on each contributor after assembling all repo-level data.
#'
#' @param data_org Partial result of 'orgmetrics_collate_org_data', including
#' full data on all repos.
#' @param end_date To up to which to extract data.
#' @param num_cores Set `NULL` to calculate with single thread, which may take
#' a very long time. Otherwise default will calculate with one less than all
#' available cores, or specify an exact integer number.
#' @return Same data with additional 'contributors' item.
#' @noRd
org_contributor_data <- function (data_org) {

    ctbs <- get_unique_ctbs (data_org)

    ctb_dat <- pbapply::pblapply (ctbs, function (ctb) {

        ctb_dat <- gh_user_general (ctb)

        ctb_dat$user |>
            dplyr::select (!c (email, location, company, bio)) |>
            dplyr::mutate (num_orgs = nrow (ctb_dat$orgs))
    })

    do.call (rbind, ctb_dat)
}

get_unique_ctbs <- function (data_org) {

    ctbs <- lapply (data_org$repos, function (repo) {
        repo$rm$contribs_from_gh_api$login
    }) |>
        unlist () |>
        unname () |>
        unique ()
    ctbs <- ctbs [which (!grepl ("\\[bot\\]", ctbs))]
    ctbs <- setdiff (ctbs, "ci-bot")
    # Running 'url_exists' takes way too long, but all ctbs exist as of Sep
    # 2025, so no checks done.

    # "Copilot" is a contributor, but doesn't exist either as a URL or an API
    # endpoint:
    ctbs <- setdiff (ctbs, "Copilot")

    return (ctbs)
}

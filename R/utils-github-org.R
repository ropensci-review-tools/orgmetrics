n_per_page_in_tests <-
    utils::getFromNamespace ("n_per_page_in_tests", "repometrics")

list_gh_org_repos <- function (org = "ropensci", n_per_page = 100) {

    checkmate::assert_character (org, len = 1L)

    is_test_env <- Sys.getenv ("ORGMETRICS_TESTS") == "true"
    n_per_page <- n_per_page_in_tests (n_per_page)

    u_base <- "https://api.github.com/orgs/"
    u_org <- paste0 (u_base, org, "/repos")

    page_num <- 1L
    is_empty <- FALSE
    names <- NULL

    while (!is_empty) {

        req <- httr2::request (u_org) |>
            add_gh_token_to_req () |>
            httr2::req_url_query (per_page = n_per_page, page = page_num)

        resp <- httr2::req_retry (req, max_tries = 5L) |>
            httr2::req_perform ()
        httr2::resp_check_status (resp)

        body <- httr2::resp_body_json (resp)

        names <- c (
            names,
            vapply (body, function (i) i$name, character (1L))
        )
        page_num <- page_num + 1L
        is_empty <- length (body) == 0L || is_test_env
    }

    return (paste0 (org, "/", names))
}

#' For individual packages in separate sub-dir, where each may belong to a
#' separate GitHub org.
#'
#' @noRd
list_gh_extra_repos <- function (org_path) {

    repos <- fs::dir_ls (org_path, type = "directory")
    unname (vapply (repos, function (i) {
        u <- gert::git_remote_info (repo = i)$url
        if (length (u) == 0L) {
            return (NULL)
        }
        u <- strsplit (u, "\\/") [[1]]
        paste0 (utils::tail (u, n = 2L), collapse = "/")
    }, character (1L)))
}

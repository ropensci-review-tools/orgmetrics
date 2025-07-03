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

        resp <- httr2::req_perform (req)
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
        paste0 (tail (u, n = 2L), collapse = "/")
    }, character (1L)))
}

pkgs_are_r <- function (pkgs) {

    # Supress no visible binding notes:
    type <- NULL

    u_base <- "https://api.github.com/repos/"

    urls <- paste0 (u_base, pkgs, "/contents")

    is_r_pkg <- vapply (urls, function (u) {

        req <- httr2::request (u)

        req <- add_gh_token_to_req (req)
        resp <- tryCatch (
            httr2::req_perform (req),
            error = function (e) NULL
        )
        if (is.null (resp)) {
            return (FALSE)
        }
        httr2::resp_check_status (resp)

        body_files <- httr2::resp_body_json (resp, simplify = TRUE)
        required <- c ("DESCRIPTION", "NAMESPACE", "R", "man")
        return (all (required %in% body_files$name))
    }, logical (1L))

    return (is_r_pkg)
}

write_pkgs_json <- function (pkgs, dir = getwd ()) {

    requireNamespace ("jsonlite", quietly = TRUE)
    requireNamespace ("rprojroot", quietly = TRUE)

    checkmate::assert_directory_exists (dir)

    pkg_dir_exists <- vapply (
        pkgs [, 1],
        function (d) fs::dir_exists (d),
        logical (1L)
    )

    root <- rprojroot::is_r_package
    pkg_root <- unlist (apply (pkgs, 1, function (p) {
        tryCatch (
            rprojroot::find_root (criterion = root, path = p [1]),
            error = function (e) ""
        )
    }))

    pkgs <- data.frame (pkgs)
    names (pkgs) <- c ("path", "orgrepo")
    path <- fs::path_common (pkgs$path)
    pkgs$path <- gsub (path, "", pkgs$path)
    # These have initial path separators which are removed here:
    rm_init_path_sep <- function (pkgs, what) {
        pkgs [[what]] <- vapply (fs::path_split (pkgs [[what]]), function (p) {
            p_red <- p [which (p != .Platform$file.sep)]
            do.call (fs::path, as.list (p_red))
        }, character (1L))
        return (pkgs)
    }
    pkgs <- rm_init_path_sep (pkgs, "path")
    pkgs$root <- gsub (path, "", pkg_root)

    files_required <- c ("DESCRIPTION", "NAMESPACE")
    dirs_required <- c ("R", "man")
    pkgs$is_r_pkg <- vapply (pkgs$path, function (p) {
        path_p <- fs::path (path, p)
        if (!fs::dir_exists (path_p)) {
            return (FALSE)
        }
        files_p <- basename (fs::dir_ls (path_p, type = "file"))
        dirs_p <- basename (fs::dir_ls (path_p, type = "directory"))
        all (files_required %in% files_p) && all (dirs_required %in% dirs_p)
    }, logical (1L))
    index <- which (pkgs$is_r_pkg)
    pkgs [index, ] <- rm_init_path_sep (pkgs [index, ], "root")

    # Then get remote data for any dirs not existing:
    index <- which (!pkg_dir_exists)
    pkgs$is_r_pkg [index] <-
        vapply (pkgs$orgrepo [index], pkgs_are_r, logical (1L))

    outfile <- fs::path (dir, "packages.json")

    jsonlite::write_json (pkgs, path = outfile, pretty = TRUE)

    return (outfile)
}

clone_gh_org_repos <- function (dir = getwd (), orgs = NULL) {

    # Supress no visible binding notes:
    is_r <- NULL

    checkmate::assert_directory_exists (dir)
    checkmate::assert_character (orgs, min.len = 1L)

    pkgs <- lapply (orgs, list_gh_org_repos)
    pkgs <- unlist (pkgs)
    if (length (pkgs) == 0L) {
        return (NULL)
    }
    outfile <- fs::path (dir, "packages.json")
    if (!fs::file_exists (outfile)) {
        pkgs <- cbind (fs::path (dir, pkgs), pkgs)
        write_pkgs_json (pkgs, dir = dir)
    }
    pkgs_json <- jsonlite::read_json (outfile, simplify = TRUE) |>
        dplyr::filter (is_r_pkg)

    for (p in pkgs_json$orgrepo) {
        url <- paste0 ("https://github.com/", p)
        dir_org <- fs::path (dir, gsub ("\\/.*$", "", p))
        if (!fs::dir_exists (dir_org)) {
            fs::dir_create (dir_org)
        }
        dest_dir <- fs::path (dir_org, p)
        if (!fs::dir_exists (dest_dir)) {
            withr::with_dir (
                dir_org,
                gert::git_clone (url)
            )
        } else {
            withr::with_dir (
                dest_dir,
                gert::git_pull (verbose = FALSE)
            )
        }
    }
}

#' Collate and write the 'package.json' file for org
#'
#' @param org_path Path to root directory of organization repositories. Should
#' contain sub-directories for different GitHub organizations. These
#' sub-directories may be initially empty, and will be populated by the
#' (currently interanl) function, `clone_gh_org_repos()`.
#' @return Path to 'packages.json' file containing data on all repositories
#' within organization(s).
#'
#' @export
om_packages_json <- function (org_path = NULL) {

    checkmate::assert_character (org_path, len = 1L)
    checkmate::assert_directory_exists (org_path)

    org_paths <- fs::dir_ls (path, type = "directory")
    org_paths <- org_paths [which (!grepl ("(temp|extra)$", org_paths))]

    repos <- lapply (basename (org_paths), function (p) {
        repos <- list_gh_org_repos (p)
        repo_name <- vapply (strsplit (repos, "\\/"), function (i) i [2], character (1L))
        repo_path <- paste0 (p, "/", repo_name)
        cbind (repo_path, repos)
    })
    repos <- do.call (rbind, repos)
    extra_path <- fs::path (path, "extra")
    if (fs::dir_exists (extra_path)) {
        extra_repos <- list_gh_extra_repos (extra_path)
        extra_path <- vapply (strsplit (extra_repos, "\\/"), function (i) i [2], character (1L))
        extra_path <- paste0 ("extra/", extra_path)
        extra_repos <- cbind (extra_path, extra_repos)
        repos <- rbind (repos, extra_repos)
    }
    repos [, 1] <- paste0 (path, repos [, 1])

    write_pkgs_json (repos, dir = org_path)
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

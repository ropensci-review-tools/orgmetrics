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

    org_paths <- fs::dir_ls (org_path, type = "directory")
    org_paths <- org_paths [which (!grepl ("(temp|extra)$", org_paths))]

    repos <- lapply (basename (org_paths), function (p) {
        repos <- list_gh_org_repos (p)
        repo_name <- vapply (strsplit (repos, "\\/"), function (i) i [2], character (1L))
        repo_path <- paste0 (p, "/", repo_name)
        cbind (repo_path, repos)
    })
    repos <- do.call (rbind, repos)

    extra_path <- fs::path (org_path, "extra")
    if (fs::dir_exists (extra_path)) {
        extra_repos <- list_gh_extra_repos (extra_path)
        extra_path <- vapply (strsplit (extra_repos, "\\/"), function (i) i [2], character (1L))
        extra_path <- paste0 ("extra/", extra_path)
        extra_repos <- data.frame (repo_path = extra_path, repos = extra_repos)
        repos <- rbind (repos, extra_repos)
    }
    repos [, 1] <- paste0 (org_path, repos [, 1])

    write_pkgs_json (repos, dir = org_path)
}

#' Write the 'packages.json' file.
#'
#' @param pkgs A `data.frame` of two columns: "repo_path" with full local path
#' to repositories, and "repos" with GitHub 'org/repo'.
#' @noRd
write_pkgs_json <- function (pkgs, dir = getwd ()) {

    # Supress no visible binding notes:
    is_r_pkg <- root <- p <- NULL

    requireNamespace ("jsonlite", quietly = TRUE)
    requireNamespace ("rprojroot", quietly = TRUE)

    checkmate::assert_directory_exists (dir)

    pkg_dir_exists <- vapply (
        pkgs [, 1],
        function (d) fs::dir_exists (d),
        logical (1L)
    )

    root_crit <- rprojroot::is_r_package
    pkg_root <- unlist (apply (pkgs, 1, function (p) {
        tryCatch (
            rprojroot::find_root (criterion = root_crit, path = p [1]),
            error = function (e) ""
        )
    }))

    pkgs <- data.frame (pkgs)
    names (pkgs) <- c ("path", "orgrepo")
    path <- fs::path_common (pkgs$path)
    # If there is only one sub-dir, step down:
    root_dirs <- fs::path_dir (pkg_root [which (nzchar (pkg_root))])
    root_minus1 <- fs::path_common (root_dirs)
    if (identical (path, root_minus1)) {
        path <- fs::path_dir (path)
    }
    pkgs$path <- gsub (path, "", pkgs$path)

    # These have initial path separators which are removed here:
    rm_init_path_sep <- function (pkgs, what) {
        pkgs [[what]] <- vapply (fs::path_split (pkgs [[what]]), function (p) {
            if (length (p) == 0L) {
                return ("")
            }
            p_red <- p [which (p != .Platform$file.sep)]
            do.call (fs::path, as.list (p_red))
        }, character (1L))
        return (pkgs)
    }
    pkgs <- rm_init_path_sep (pkgs, "path")
    pkgs$root <- gsub (path, "", pkg_root)
    pkgs <- rm_init_path_sep (pkgs, "root")

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

    # Then check whether 'root' is missing for any R pkgs, and clone if so:
    pkgs_r_no_root <- dplyr::filter (pkgs, is_r_pkg) |>
        dplyr::filter (!nzchar (root))
    if (nrow (pkgs_r_no_root) > 0L) {
        for (i in seq_len (nrow (pkgs_r_no_root))) {
            pkg_dir_i <- fs::path (dir, p)
            if (!fs::dir_exists (pkg_dir_i)) {
                url <- paste0 ("https://github.com/", pkgs_r_no_root$orgrepo [i])
                withr::with_dir (
                    fs::path_dir (pkg_dir_i),
                    gert::git_clone (url)
                )
            }

            pkgs_r_no_root$root <- tryCatch (
                rprojroot::find_root (criterion = root_crit, path = pkg_dir_i),
                error = function (e) ""
            )
        }
        index <- match (pkgs_r_no_root$path, pkgs$path)
        pkgs$root [index] <- pkgs_r_no_root$root [index]
    }

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
        resp <- tryCatch ({
            httr2::req_retry (req, max_tries = 5L) |>
                httr2::req_perform ()
            }, error = function (e) NULL
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

#' Clone or update all repositories defined in 'packages.json'
#'
#' @param pkgs_json Local path to 'packages.json' as created or updated by
#' running \link{om_packages_json}. That function must be run first, prior to
#' calling this function!
#' @param pkgs_dir Defaults to cloning repositories in the root directory of
#' 'packages.json'. A specific path may be specified here to clone elsewhere.
#' @return Function primarily called for side-effect of clone or updating all
#' repositories defined in 'packages.json', but does invisibly return a vector
#' of paths to all local repositories of R packages as listed in `pkgs_json`.
#'
#'
#' @export
clone_gh_org_repos <- function (pkgs_json = NULL, pkgs_dir = NULL) {

    requireNamespace ("jsonlite", quietly = TRUE)

    checkmate::assert_character (pkgs_json, len = 1L)
    checkmate::assert_file_exists (pkgs_json)
    if (is.null (pkgs_dir)) {
        pkgs_dir <- fs::path_dir (pkgs_json)
    } else {
        checkmate::assert_directory_exists (pkgs_dir)
    }

    # Supress no visible binding notes:
    is_r_pkg <- NULL

    pj <- jsonlite::read_json (pkgs_json, simplify = TRUE) |>
        dplyr::filter (is_r_pkg) |>
        update_pj_path (fs::path_dir (pkgs_json))

    cli::cli_alert_info ("Cloning or updating {nrow (pj)} repositories.")

    out <- pbapply::pblapply (seq_len (nrow (pj)), function (i) {
        url <- paste0 ("https://github.com/", pj$orgrepo [i])
        dir_org <- fs::path_dir (pj$path [i])
        if (!fs::dir_exists (dir_org)) {
            fs::dir_create (dir_org)
        }
        if (!fs::dir_exists (pj$path [i])) {
            withr::with_dir (
                dir_org,
                gert::git_clone (url)
            )
        } else {
            withr::with_dir (
                pj$path [i],
                gert::git_fetch (verbose = FALSE)
            )
        }
    })

    invisible (unlist (out))
}

update_pj_path <- function (pj, pkgs_dir) {

    if ("path" %in% names (pj)) {
        path_dir <- vapply (fs::path_split (pj$path), function (p) {
            ifelse (
                length (p) <= 2,
                "",
                do.call (fs::path, as.list (p [seq_len (length (p) - 2)]))
            )
        }, character (1L))
        index <- which (!fs::dir_exists (path_dir))
        pj$path [index] <- fs::path (pkgs_dir, pj$path [index])
    } else {
        pj$path <- fs::path (pkgs_dir, pj$package)
    }

    return (pj)
}

#' Read a single r-universe "packages.json" file, clone all repos, and update
#' "packages.json" to expected format.
#'
#' Standard r-universe "packages.json" has just "package" and "url". The format
#' here requires these additional fields:
#' - "is_r_pkg", generated from the `pkgs_are_r()` function applied to
#' 'org/repo' strings.
#'  - "orgrepo" as the GitHub 'org/repo' strings.
#' - "path" as a local path to "pkgs_dir" which must include org-level
#' sub-directories.
#'
#' @noRd
clone_r_univ_pkgs_json <- function (pkgs_json = NULL, pkgs_dir = fs::path_temp ()) {

    checkmate::assert_character (pkgs_json, len = 1L)
    checkmate::assert_file_exists (pkgs_json)
    checkmate::assert_directory_exists (pkgs_dir)

    pj <- jsonlite::read_json (pkgs_json, simplify = TRUE)
    pj$orgrepo <- vapply (strsplit (pj$url, "\\/"), function (i) {
        paste0 (utils::tail (i, 2L), collapse = "/")
    }, character (1L))
    pj$is_r_pkg <- pkgs_are_r (pj$orgrepo)

    dir_parts <- fs::path_split (pkgs_dir) [[1]]
    pj$path <- vapply (strsplit (pj$orgrepo, "\\/"), function (i) {
        do.call (fs::path, as.list (c (dir_parts, utils::tail (i, 2L))))
    }, character (1L))

    pkgs_json_new <- fs::path (pkgs_dir, "packages.json")
    jsonlite::write_json (pj, pkgs_json_new, pretty = TRUE)

    clone_gh_org_repos (pkgs_json_new)
}

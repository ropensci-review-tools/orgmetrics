#' Main function to return a function call network between all packages defined
#' in `org_paths`.
#'
#' Takes the result of `rm_org_data_fn_calls`, which includes details of all
#' actual functions called, and reduces down to two summary metrics of
#' connections between packages in terms of total numbers of functions called
#' by each pair of packages, and numbers of actual calls made.
#' @noRd
rm_org_data_fn_call_network <- function (pkgs_json) {

    requireNamespace ("jsonlite", quietly = TRUE)

    # Suppress no visible binding notes:
    is_r_pkg <- target_pkg <- n <- NULL

    pkgs_dat <- jsonlite::read_json (pkgs_json, simplify = TRUE) |>
        dplyr::filter (is_r_pkg)
    pkgs_dat$path <- fs::path (fs::path_dir (pkgs_json), pkgs_dat$path)

    fn_calls <- rm_org_data_fn_calls (pkgs_dat)

    fn_call_summary <- fn_calls |>
        dplyr::group_by (source, target_pkg) |>
        dplyr::summarise (
            num_fns = dplyr::n (),
            num_calls = sum (n),
            .groups = "keep"
        )

    return (list (fn_calls = fn_calls, summary = fn_call_summary))
}

#' Collate calls to all functions defined within packages of an organization.
#' @noRd
rm_org_data_fn_calls <- function (pkgs_dat) {

    # Suppress no visible binding notes:
    fn <- name <- package <- NULL

    requireNamespace ("pkgmatch", quietly = TRUE)

    pkg_names <- get_all_pkg_names (pkgs_dat)

    fn_calls <- pbapply::pblapply (pkg_names$path, function (p) {
        get_pkg_fn_calls (p, pkg_names)
    })
    fn_calls <- do.call (rbind, fn_calls)

    fn_calls |>
        dplyr::mutate (source = gsub ("\\:\\:.*$", "", fn), .before = 1L) |>
        dplyr::mutate (fn = gsub ("^.*\\:\\:", "", fn)) |>
        dplyr::rename (target_fn = name, target_pkg = package)
}

get_all_pkg_names <- function (pkgs_dat = NULL) {

    dir <- fs::path_common (pkgs_dat$path)
    if (length (unique (fs::path_dir (pkgs_dat$path))) == 1L) {
        dir <- fs::path_dir (dir)
    }
    pkgs_root <- fs::path (dir, pkgs_dat$root)
    pkg_names <- vapply (pkgs_root, function (i) {
        get_pkg_name (i)
    }, character (1L), USE.NAMES = FALSE)
    res <- data.frame (path = as.character (pkgs_root), pkg_name = pkg_names)
    res [which (nzchar (pkg_names)), ]
}

get_pkg_name <- function (path) {
    tryCatch (
        suppressWarnings (
            desc::desc_get_field (key = "Package", file = path)
        ),
        error = function (e) ""
    )
}

#' Get calls within single package to all packages named in `pkg_names`.
#' @noRd
get_pkg_fn_calls_internal <- function (path, pkg_names) {

    # Suppress no visible binding notes:
    package <- name <- fn <- NULL

    fns <- pkgmatch::pkgmatch_treesitter_fn_tags (path)
    if (nrow (fns) == 0L) {
        return (NULL)
    }

    this_pkg <- get_pkg_name (path)

    fns |>
        dplyr::mutate (package = gsub ("\\:\\:.*$", "", name), .after = name) |>
        dplyr::filter (package %in% pkg_names$pkg_name) |>
        dplyr::group_by (fn, name, package) |>
        dplyr::summarise (n = dplyr::n (), .groups = "keep") |>
        dplyr::filter (!package == get_pkg_name (path))
}
get_pkg_fn_calls <- memoise::memoise (get_pkg_fn_calls_internal)

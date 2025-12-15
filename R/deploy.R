#' Clone an r-universe
#' @noRd
clone_univ <- function (url, dest_dir) {

    checkmate::assert_directory_exists (dest_dir)
    td <- fs::path (fs::path_temp (), "orgmetrics")
    if (!fs::dir_exists (td)) {
        fs::dir_create (td, recurse = TRUE)
    }
    gert::git_clone (url, path = td)
    flist <- fs::dir_ls (td, type = "file")
    pkgs_json <- grep ("packages\\.json", flist, value = TRUE)
    config <- grep ("orgmetrics.*config\\.yaml", flist, value = TRUE)
    if (length (pkgs_json) != 1L) {
        cli::cli_abort (
            "Repository must have a 'packages.json' file."
        )
    }
    if (length (config) != 1L) {
        cli::cli_abort (
            "Repository must have an 'orgmetrics-config.yaml' file."
        )
    }
    check_config_yaml (config)

    if (!fs::dir_exists (dest_dir)) {
        fs::dir_create (dest_dir)
    }
    repos <- clone_r_univ_pkgs_json (pkgs_json = pkgs_json, pkgs_dir = dest_dir)

    pkgs_json <- fs::dir_ls (
        dest_dir,
        regexp = "packages\\.json$",
        type = "file"
    )
    fs::file_copy (config, dest_dir, overwrite = TRUE)
    config <- fs::dir_ls (
        dest_dir,
        regexp = "config\\.yaml$",
        type = "file"
    )

    list (
        packages_json = pkgs_json,
        orgmetrics_config = config,
        repos = repos
    )
}

check_config_yaml <- function (config_path) {

    requireNamespace ("yaml", quietly = TRUE)
    config <- yaml::read_yaml (config_path)

    checkmate::assert_list (config, len = 1L)
    checkmate::assert_named (config)
    checkmate::assert_names (names (config), permutation.of = c ("orgmetrics"))

    config_om <- config$orgmetrics
    checkmate::assert_list (config_om, min.len = 2L)
    checkmate::assert_named (config_om)
    checkmate::assert_names (
        names (config_om),
        must.include = c ("title", "aggregation_period")
    )

    checkmate::assert_character (config_om$title, len = 1L)
    checkmate::assert_numeric (
        config_om$aggregation_period,
        len = 1L,
        lower = 1,
        upper = 365.25
    )
}

#' Matrrix of 'orgmetrics' function names and associated names of data objects
#' where results of those functions are stored.
#' @noRd
deploy_fn_data <- function () {
    rbind (
        c ("orgmetrics_collate_org_data", "data_org"),
        c ("rm_org_data_fn_call_network", "fn_calls"),
        c ("rm_org_data_embeddings", "embeddings")
    )
}

get_data_from_cloned_univ <- function (url, dest_dir) {

    rm_old_data_from_cloned_univ (dest_dir)

    local_univ_dat <- clone_univ (url, dest_dir)
    config <- yaml::read_yaml (local_univ_dat$orgmetrics_config)$orgmetrics
    options ("repometrics_period" = config$aggregation_period)
    pkgs_json <- fs::dir_ls (
        dest_dir,
        regexp = "packages\\.json$",
        type = "file"
    )

    end_date <- Sys.Date ()
    num_years <- 1

    fn_dat <- deploy_fn_data ()

    data <- apply (fn_dat, 1, function (i) {
        f <- fs::path (dest_dir, paste0 (i [2], ".Rds"))
        if (!fs::file_exists (f)) {
            pars <- list (pkgs_json = pkgs_json)
            if (i [2] == "data_org") {
                pars <- list (
                    pkgs_json = pars [[1]],
                    end_date = end_date,
                    num_years = num_years
                )
            }
            x <- do.call (i [1], pars)
            saveRDS (x, f)
        } else {
            x <- readRDS (f)
        }
        return (x)
    })

    attr (data, "title") <- config$title

    return (data)
}

rm_old_data_from_cloned_univ <- function (dest_dir) {

    fn_dat <- deploy_fn_data ()
    f <- fs::path (dest_dir, paste0 (fn_dat [, 2], ".Rds"))
    info <- fs::file_info (f)
    td <- difftime (Sys.time (), info$modification_time, unit = "days")
    files_to_rm <- info$path [which (td > 1)]
    if (length (files_to_rm) > 0L) {
        fs::file_delete (files_to_rm)
    }
}

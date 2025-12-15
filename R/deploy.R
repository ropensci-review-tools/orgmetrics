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
}

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

    if (!fs::dir_exists (dest_dir)) {
        fs::dir_create (dest_dir)
    }
    repos <- clone_r_univ_pkgs_json (pkgs_json = pkgs_json, pkgs_dir = dest_dir)

    pkgs_json <- fs::dir_ls (
        dest_dir,
        regexp = "packages\\.json$",
        type = "file"
    )
    config <- grep ("orgmetrics.*config\\.yaml", flist, value = TRUE)
    fs::file_copy (config, dest_dir)
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

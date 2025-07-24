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

generate_test_pkg <- function () {

    pkg <- system.file ("extdata", "testpkg.zip", package = "orgmetrics")
    pkg_dest <- fs::path (fs::path_temp (), "testpkg")
    if (fs::dir_exists (pkg_dest)) {

        # Check that pkg_dest holds same contents:
        flist <- fs::dir_ls (pkg_dest, recurse = TRUE)
        flist0 <- fs::path_rel (flist, fs::path_common (flist))

        # new tmp version of same:
        nm <- paste0 (sample (c (letters, LETTERS), size = 10), collapse = "")
        pkg_dest_tmp <- fs::path (fs::path_temp (), nm)
        fs::dir_create (pkg_dest_tmp)
        flist1 <- unzip (pkg, exdir = pkg_dest_tmp)
        pkg_dest_tmp_pkg <- fs::path (pkg_dest_tmp, basename (pkg_dest))

        f1 <- fs::dir_ls (pkg_dest_tmp_pkg, recurse = TRUE)
        flist1 <- fs::path_rel (f1, fs::path_common (f1))

        is_same <- length (setdiff (flist0, flist1)) == 0L
        if (is_same) {
            flist <- fs::dir_ls (pkg_dest, recurse = TRUE)
        } else {
            flist <- fs::dir_ls (pkg_dest_tmp_pkg)
        }

    } else {
        flist <- unzip (pkg, exdir = fs::path_temp ())
    }

    return (fs::path_common (flist))
}

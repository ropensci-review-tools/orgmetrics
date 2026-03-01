#' Deploy a dashboard from an 'r-universe' "packages.json" file.
#'
#' @inheritParams orgmetrics_dashboard
#'
#' @param url URL of a '<username>.r-universe.dev' GitHub repository,
#' containing a "packages.json" file defining the repositories contained within
#' the 'r-universe'.
#' If the parameter is not specified, function is presumed to be called within
#' local version of an r-universe-like repository.
#' @param dest_dir A local directory where all repositories of the specified
#' 'r-universe' will be cloned, and also where data generated for the
#' 'orgmetrics' dashboard will be stored. The default is a temporary path, but
#' a permanent local path is better for local usage of this function, to avoid
#' re-generating the same data each time.
#' @param title Title for 'orgmetrics' dashboard. Default is `NULL`, in
#' which case the title is taken to be the terminal element of `url`.
#' @param aggregation_period Period in days over which prior activity is to be
#' aggregated. Should be an integer greater than or equal to `90L`, with a default of one year (`365L`)
#' @export
orgmetrics_deploy_r_univ <- function (url = NULL,
                                      dest_dir = fs::path_temp (),
                                      title = NULL,
                                      aggregation_period = 365L,
                                      action = NULL) {

    checkmate::assert_directory_exists (dest_dir)
    if (!is.null (title)) {
        checkmate::assert_character (title, len = 1L)
    }
    checkmate::assert_integerish (aggregation_period, lower = 90L, len = 1L)

    if (is.null (title) && !is.null (url)) {
        title <- gsub ("\\.git$", "", basename (url))
    }
    if (!is.null (action)) {
        action <- match.arg (action, c ("preview", "render"))
    }

    data <- get_data_from_cloned_univ (url, dest_dir, aggregation_period)
    data_org <- data [[1]]
    fn_calls <- data [[2]]
    embeddings <- data [[3]]
    rm (data)

    cli::cli_inform ("All data extracted; building dashboard ...")

    path <- orgmetrics_dashboard (data_org, fn_calls, embeddings, title = title, action = action)
    update_url_segments (path, url)

    # Move everything to 'quarto' sub-dir here:
    if (!identical (fs::path_abs ("."), fs::path_temp ())) {
        path <- fs::path_abs (fs::dir_copy (path, "."))
    }

    cli::cli_inform ("Dashboard built.")
    invisible (path)
}

#' Clone an r-universe
#' @noRd
clone_univ <- function (url, dest_dir) {

    checkmate::assert_directory_exists (dest_dir)
    if (!is.null (url)) {
        td <- fs::path (fs::path_temp (), "orgmetrics")
        if (!fs::dir_exists (td)) {
            fs::dir_create (td, recurse = TRUE)
        }
        gert::git_clone (url, path = td)
    } else {
        td <- fs::path_abs (".")
    }
    flist <- fs::dir_ls (td, type = "file")
    pkgs_json <- grep ("packages\\.json", flist, value = TRUE)
    if (length (pkgs_json) != 1L) {
        cli::cli_abort (
            "Repository must have a 'packages.json' file."
        )
    }

    if (!fs::dir_exists (dest_dir)) {
        fs::dir_create (dest_dir)
    }
    repos <- clone_r_univ_pkgs_json (pkgs_json = pkgs_json, pkgs_dir = dest_dir)

    pkgs_json <- fs::dir_ls (
        dest_dir,
        regexp = "packages\\.json$",
        type = "file"
    )

    list (
        packages_json = pkgs_json,
        repos = repos
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

get_data_from_cloned_univ <- function (url, dest_dir, aggregation_period) {


    if (as.character (dest_dir) == as.character (fs::path_temp ())) {
        dest_dir <- fs::path (dest_dir, "orgmetrics")
        fs::dir_create (dest_dir, recurse = TRUE)
    }

    rm_old_data_from_cloned_univ (dest_dir)

    local_univ_dat <- clone_univ (url, dest_dir)
    options ("repometrics_period" = aggregation_period)
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

    return (data)
}

rm_old_data_from_cloned_univ <- function (dest_dir) {

    fn_dat <- deploy_fn_data ()
    f <- fs::path (dest_dir, paste0 (fn_dat [, 2], ".Rds"))
    info <- fs::file_info (f)
    td <- difftime (Sys.time (), info$modification_time, units = "days")
    files_to_rm <- info$path [which (td > 1)]
    if (length (files_to_rm) > 0L) {
        fs::file_delete (files_to_rm)
    }
}


#' Update URL segments in OJS chunks
#'
#' Internal URL refs to other pages need to be updated in deployed version to
#' include URL segments before the page refs. This is automatically done in all
#' \pkg{bslib} R segments. This updates all internal refs in OJS chunks.
#'
#' Note that for locally-deployed versions, this can and should not be done, so
#' it is not part of the main dashboard function. It is only necessary when
#' actually deploying the site to an external host on which it will be served
#' at some segment of a main URL (such as via GitHub pages).
#'
#' @noRd
update_url_segments <- function (path, url) {

    url_segment <- gsub ("^.*\\/", "", url)
    flist <- fs::dir_ls (path, regexp = "\\.qmd$")

    for (f in flist) {

        contents <- readLines (f)

        chunks <- t (matrix (grep ("^\\`\\`\\`", contents), nrow = 2))
        chunks <- data.frame (
            from = chunks [, 1],
            to = chunks [, 2],
            ojs = FALSE
        )
        chunks_ojs <- grep ("^\\`\\`\\`\\{ojs", contents)
        chunks$ojs [match (chunks_ojs, chunks$from)] <- TRUE
        chunks_ojs <- as.matrix (chunks [which (chunks$ojs), 1:2])
        contents_ojs <- apply (chunks_ojs, 1, function (i) {
            seq (i [1], i [2])
        }) |>
            unlist () |>
            unname () |>
            sort ()

        hrefs <- grep ("href(\\s*)\\=", contents)
        hrefs_ojs <- hrefs [which (hrefs %in% contents_ojs)]
        is_abs <- grepl ("https\\:\\/\\/", contents [hrefs_ojs])
        is_chaoss <- grepl ("commhealth", contents [hrefs_ojs], ignore.case = TRUE)
        hrefs_ojs <- hrefs_ojs [which (!is_abs & !is_chaoss)]
        # Lots of hrefs in repo.qmd are to interal variables:
        is_href_var <- grepl ("href(\\s*)\\=(\\s*)(\\\"?)\\$", contents [hrefs_ojs])
        hrefs_ojs <- hrefs_ojs [which (!is_href_var)]

        if (length (hrefs_ojs) > 0L) {
            # Double quotations on href strings:
            contents [hrefs_ojs] <- gsub (
                "href(\\s*)\\=(\\s*)\\\"\\.\\.\\/",
                paste0 ("href=\"../", url_segment, "/"),
                contents [hrefs_ojs]
            )
            # Single quotation on href strings:
            contents [hrefs_ojs] <- gsub (
                "href(\\s*)\\=(\\s*)\'\\.\\.\\/",
                paste0 ("href='../", url_segment, "/"),
                contents [hrefs_ojs]
            )

            writeLines (contents, f)
        }
    }
}

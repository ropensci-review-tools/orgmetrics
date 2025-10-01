org_repo_from_path <-
    utils::getFromNamespace ("org_repo_from_path", "repometrics")
metrics_over_end_dates <-
    utils::getFromNamespace ("metrics_over_end_dates", "repometrics")
models_over_end_dates <-
    utils::getFromNamespace ("models_over_end_dates", "repometrics")

orgmetrics_collate_org_data <- function (pkgs_json, end_date = Sys.Date (), num_years = 3) {

    requireNamespace ("jsonlite", quietly = TRUE)

    # Suppress no visible binding notes:
    is_r_pkg <- NULL

    pkgs_dat <- jsonlite::read_json (pkgs_json, simplify = TRUE) |>
        dplyr::filter (is_r_pkg)
    pkgs_dat$path <- fs::path (fs::path_dir (pkgs_json), pkgs_dat$path)
    is_test_env <- Sys.getenv ("ORGMETRICS_TESTS") == "true"

    # pkgs_dat has [path, orgrepo, root, is_r_pkg]
    data <- lapply (seq_len (nrow (pkgs_dat)), function (i) {

        if (!pkgs_dat$is_r_pkg [i] && !is_test_env) {
            return (NULL)
        }
        if (!fs::dir_exists (pkgs_dat$path [i])) {
            return (NULL)
        }

        cli::cli_alert_info ("[{i} / {nrow (pkgs_dat)}]: {pkgs_dat$orgrepo[i]}")
        pkg_i <- basename (pkgs_dat$orgrepo [i])
        f_tmp <- fs::path (fs::path_temp (), paste0 (pkg_i, ".Rds"))
        if (fs::file_exists (f_tmp)) {
            dat_i <- readRDS (f_tmp)
        } else {
            dat_repo <- repometrics::repometrics_data_repo (
                pkgs_dat$path [i],
                date_interval = "year"
            )
            dat_i <- list (
                repo = dat_repo_to_end_date (dat_repo, end_date = end_date),
                metrics = metrics_over_end_dates (
                    pkgs_dat$path [i],
                    end_date = end_date,
                    num_years = num_years
                ),
                models = models_over_end_dates (
                    pkgs_dat$path [i],
                    end_date = end_date,
                    num_years = num_years
                )
            )
            saveRDS (dat_i, f_tmp)
        }
        return (dat_i)
    })

    index <- which (vapply (data, length, integer (1L)) > 0L)
    pkgs_dat <- pkgs_dat [index, ]
    data <- data [index]

    pkgs_repos <- lapply (data, function (i) i$repo)
    pkgs_metrics <- lapply (data, function (i) i$metrics)
    pkgs_models <- lapply (data, function (i) i$models)

    or <- vapply (pkgs_dat$path, function (i) {
        paste0 (org_repo_from_path (i), collapse = "/")
    }, character (1L), USE.NAMES = FALSE)
    names (pkgs_repos) <- names (pkgs_metrics) <- names (pkgs_models) <- or

    pkgs_models <- lapply (seq_along (pkgs_models), function (i) {
        pkgs_models [[i]] |>
            dplyr::mutate (package = names (pkgs_models) [i], .before = 1)
    })
    pkgs_models <- do.call (rbind, pkgs_models)

    rm_tmp_pkg_files (or)

    org_paths <- unique (fs::path_dir (pkgs_dat$path))
    annual_commits <- org_annual_commits (org_paths)
    annual_gh_activity <- org_annual_gh_activity (pkgs_repos)

    ctb_dat <- org_contributor_data (pkgs_repos)

    data <- list (
        repos = pkgs_repos,
        metrics = pkgs_metrics,
        models = pkgs_models,
        contributors = ctb_dat,
        annual_commits = annual_commits,
        annual_gh_activity = annual_gh_activity
    )

    attr (data, "end_date") <- end_date

    return (data)
}

rm_tmp_pkg_files <- function (pkgs) {
    pkgs <- unique (gsub ("^.*\\/", "", pkgs))
    f_tmp_list <- fs::path (fs::path_temp (), paste0 (pkgs, ".Rds"))
    f_tmp_list <- f_tmp_list [which (fs::file_exists (f_tmp_list))]
    if (length (f_tmp_list) > 0L) {
        fs::file_delete (f_tmp_list)
    }
}

dat_repo_to_end_date <- function (dat_repo, end_date = Sys.Date ()) {

    # Suppress no visible binding notes:
    timestamp <- created_at <- created <- starred_at <- NULL

    if (!inherits (end_date, "Date")) {
        end_date <- as.Date (end_date)
    }

    dat_repo$pkgstats <- lapply (dat_repo$pkgstats, function (p) {
        if (length (p) == 0L) {
            return (p)
        }
        p |> dplyr::filter (as.Date (date) <= end_date)
    })

    dat_repo$rm$gh_repo_workflow <- dat_repo$rm$gh_repo_workflow |>
        dplyr::filter (as.Date (created) <= end_date)
    dat_repo$rm$gitlog <- dat_repo$rm$gitlog |>
        dplyr::filter (as.Date (timestamp) <= end_date)
    if (!is.null (dat_repo$issue_comments_from_gh_api)) {
        dat_repo$rm$issue_comments_from_gh_api <-
            dat_repo$rm$issue_comments_from_gh_api |>
            dplyr::filter (as.Date (created_at) <= end_date)
    }
    if (!is.null (dat_repo$issues_from_gh_api)) {
        dat_repo$rm$issues_from_gh_api <-
            dat_repo$rm$issues_from_gh_api |>
            dplyr::filter (as.Date (created_at) <= end_date)
    }
    if (!is.null (dat_repo$prs_from_gh_api)) {
        dat_repo$rm$prs_from_gh_api <- dat_repo$rm$prs_from_gh_api |>
            dplyr::filter (as.Date (created_at) <= end_date)
    }
    if (!is.null (dat_repo$releases_from_gh_api)) {
        dat_repo$rm$releases_from_gh_api <-
            dat_repo$rm$releases_from_gh_api |>
            dplyr::filter (as.Date (created_at) <= end_date)
    }
    if (!is.null (dat_repo$repo_forks)) {
        dat_repo$rm$repo_forks <- dat_repo$rm$repo_forks |>
            dplyr::filter (as.Date (created) <= end_date)
    }
    if (!is.null (dat_repo$repo_stargazers)) {
        dat_repo$rm$repo_stargazers <- dat_repo$rm$repo_stargazers |>
            dplyr::filter (as.Date (starred_at) <= end_date)
    }

    return (dat_repo)
}

org_annual_commits <- function (org_paths) {

    pkgs <- fs::dir_ls (org_paths, type = "directory")
    annual_commits <- lapply (pkgs, function (i) {
        cmts <- git2r::commits (repo = i)
        cmt_times <- vapply (
            cmts,
            function (j) as.character (j$author$when),
            character (1L)
        )
        cmt_years <- as.integer (gsub ("\\-.*$", "", cmt_times))
        return (cmt_years)
    })
    annual_commits <- table (unlist (annual_commits))

    data.frame (
        year = names (annual_commits),
        num_commits = as.integer (annual_commits)
    )
}

org_annual_gh_activity <- function (pkgs_repos) {
    annual_gh <- lapply (pkgs_repos, function (repo) {
        issues <- repo$rm$issues_from_gh_api
        prs <- repo$rm$prs_from_gh_api
        issue_cmts <- repo$rm$issue_comments_from_gh_api

        issues_created_at <- as.integer (gsub ("\\-.*$", "", issues$created_at))
        prs_created_at <- as.integer (gsub ("\\-.*$", "", prs$created_at))
        cmts_created_at <- as.integer (gsub ("\\-.*$", "", issue_cmts$created_at))

        list (
            issues = issues_created_at,
            prs = prs_created_at,
            cmts = cmts_created_at
        )
    })

    years <- unlist (lapply (annual_gh, function (i) i$issues))
    if (length (years) > 0L) {
        years <- seq (min (years), max (years))
    }
    res <- lapply (c ("issues", "prs", "cmts"), function (what) {
        data <- lapply (annual_gh, function (i) i [[what]]) |>
            unlist () |>
            table ()
        data_years <- rep (0L, length (years))
        index <- match (names (data), years)
        data_years [index] <- as.integer (data)
        return (data_years)
    })
    data.frame (
        year = years,
        issues = res [[1]],
        prs = res [[2]],
        cmts = res [[3]]
    )

}

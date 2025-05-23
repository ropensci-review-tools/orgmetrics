org_repo_from_path <-
    utils::getFromNamespace ("org_repo_from_path", "repometrics")
metrics_over_end_dates <-
    utils::getFromNamespace ("metrics_over_end_dates", "repometrics")
models_over_end_dates <-
    utils::getFromNamespace ("models_over_end_dates", "repometrics")

orgmetrics_collate_org_data <- function (org_paths, end_date = Sys.Date (), num_years = 3) {

    pkgs <- fs::dir_ls (org_paths, type = "directory")
    org_repo <- t (vapply (pkgs, function (p) {
        p_s <- fs::path_split (p) [[1]]
        n <- length (p_s)
        p_s [(n - 1):n]
    }, character (2L), USE.NAMES = FALSE))

    data <- lapply (seq_along (pkgs), function (i) {

        is_r <- pkgs_are_r (paste0 (org_repo [i, ], collapse = "/"))
        if (!is_r) {
            return (NULL)
        }

        cli::cli_alert_info ("[{i} / {length(pkgs)}]: {pkgs[i]}")
        path_i <- pkgs [i]
        pkg_i <- basename (path_i)
        f_tmp <- fs::path (fs::path_temp (), paste0 (pkg_i, ".Rds"))
        if (fs::file_exists (f_tmp)) {
            dat_i <- readRDS (f_tmp)
        } else {
            dat_i <- list (
                repo = repometrics::repometrics_data_repo (path_i),
                metrics = metrics_over_end_dates (
                    path_i,
                    end_date = end_date,
                    num_years = num_years
                ),
                models = models_over_end_dates (
                    path_i,
                    end_date = end_date,
                    num_years = num_years
                )
            )
            saveRDS (dat_i, f_tmp)
        }
        return (dat_i)
    })

    pkgs_repos <- lapply (data, function (i) i$repo)
    pkgs_metrics <- lapply (data, function (i) i$metrics)
    pkgs_models <- lapply (data, function (i) i$models)

    or <- vapply (pkgs, function (i) {
        paste0 (org_repo_from_path (i), collapse = "/")
    }, character (1L), USE.NAMES = FALSE)
    names (pkgs_repos) <- names (pkgs_metrics) <- names (pkgs_models) <- or

    pkgs_models <- lapply (seq_along (pkgs_models), function (i) {
        pkgs_models [[i]] |>
            dplyr::mutate (package = names (pkgs_models) [i], .before = 1)
    })
    pkgs_models <- do.call (rbind, pkgs_models)

    rm_tmp_pkg_files (or)

    annual_commits <- org_annual_commits (org_paths)
    annual_gh_activity <- org_annual_gh_activity (pkgs_repos)

    data <- list (
        repos = pkgs_repos,
        metrics = pkgs_metrics,
        models = pkgs_models,
        annual_commits = annual_commits,
        annual_gh_activity = annual_gh_activity
    )

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
    years <- seq (min (years), max (years))
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

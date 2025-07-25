#' Start quarto dashboard with results of
#' `orgmetrics_collate_org_data` function for collation of data across orgs.
#'
#' @param data_org Data on GitHub organization as returned from
#' `orgmetrics_collate_org_data` function.
#' @param fn_calls Data on function calls between packages of the specified
#' organization, as returned from the `rm_org_data_fn_call_network()` function.
#' @param action One of "preview", to start and open a live preview of the
#' dashboard website, or "render" to render a static version without previewing
#' or opening.
#' @param similarities Matrix of cosine similarities from embeddings between
#' packages, returned from `rm_org_emb_distances()` function.
#' @return (Invisibly) Path to main "index.html" document of quarto site. Note
#' that the site must be served with `action = "preview"`, and will not work by
#' simply opening this "index.html" file.
#' @export
orgmetrics_dashboard <- function (data_org, fn_calls, similarities, action = "preview") {

    # Suppress no visible binding notes:
    org <- package <- NULL

    action <- match.arg (action, c ("preview", "render"))
    quarto_action <- paste0 ("quarto::quarto_", action)

    requireNamespace ("jsonlite", quietly = TRUE)
    requireNamespace ("quarto", quietly = TRUE)
    requireNamespace ("withr", quietly = TRUE)

    # -------- CHAOSS MODELS and METRICS: START -------
    data_models <- data_models_preprocess (data_org$models) |>
        dplyr::select (-org, -date) |>
        tidyr::pivot_longer (-package)

    data_metrics <- data_metrics_to_df (data_org$metrics)

    dates <- sort (unique (data_metrics$date), decreasing = TRUE)
    repo_metrics <- dashboard_data_repo_metrics (data_metrics, dates)

    data_metrics <- lapply (dates, function (d) {
        data_metrics |>
            dplyr::filter (date == d) |>
            data_metrics_preprocess ()
    })
    names (data_metrics) <- dates
    # -------- CHAOSS MODELS and METRICS: END -------

    # -------- ADDITIONAL DATA IN R -------
    data_maintenance <- org_maintenance_metric (data_org)
    data_abs <- ctb_absence (data_org)
    data_resp <- issue_responses (data_org)
    data_bugs <- issue_bugs (data_org)
    data_contributors <- dashboard_data_contributors (data_org)
    data_repo_src <- dashboard_data_repo_source (data_org)
    data_pkgcheck <- lapply (data_org$repos, function (i) i$pkgcheck)

    # -------- ADDITIONAL DATA IN JSON -------
    # All saved as single JSON structure; only used in 'repo.qmd' and 'maintainer.qmd'
    data_cran <- dashboard_data_cran (data_org)
    not_cran <- gsub ("^.*\\/", "", attr (data_cran, "not_cran"))
    attr (data_cran, "not_cran") <- NULL
    data_gitlog <- dashboard_data_gitlog (data_org)
    data_r_universe <- dashboard_data_r_universe (data_org)
    rm_metrics_json <- system.file (
        "extdata",
        "metrics-models",
        "metrics-models.json",
        package = "repometrics"
    )
    rm_metrics_models <- jsonlite::read_json (rm_metrics_json, simplify = TRUE)
    maintainers <- databoard_data_maintainers (data_contributors)

    data_json <- list (
        cran = data_cran,
        not_cran = not_cran,
        gitlog = data_gitlog,
        r_universe = data_r_universe,
        rm_metrics_models = rm_metrics_models,
        maintainer_pkgs = maintainers$maintainers,
        comaintainers = maintainers$comaintainers
    )

    path_src <- system.file ("extdata", "quarto", package = "orgmetrics")
    path_dest <- fs::path (fs::path_temp (), "quarto")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)
    dir_data <- fs::path (dir, "data")
    if (!fs::dir_exists (dir_data)) {
        fs::dir_create (dir_data, recurse = TRUE)
    }

    # -------- SAVE CHAOSS DATA
    saveRDS (data_models, fs::path (dir_data, "results-models.Rds"))
    saveRDS (data_metrics, fs::path (dir_data, "results-metrics.Rds"))
    saveRDS (repo_metrics, fs::path (dir_data, "results-repo-metrics.Rds"))
    saveRDS (data_org$annual_commits, fs::path (dir_data, "results-annual-commits.Rds"))
    saveRDS (data_org$annual_gh_activity, fs::path (dir_data, "results-annual-gh-activity.Rds"))

    # -------- SAVE ADDITIONAL R DATA
    saveRDS (data_maintenance, fs::path (dir_data, "results-maintenance-org.Rds"))
    saveRDS (data_contributors, fs::path (dir_data, "results-maintenance-contribs.Rds"))
    saveRDS (data_repo_src, fs::path (dir_data, "results-data-repo-src.Rds"))
    saveRDS (data_abs, fs::path (dir_data, "results-data-ctb-absence.Rds"))
    saveRDS (data_resp, fs::path (dir_data, "results-data-issue-resp.Rds"))
    saveRDS (data_bugs, fs::path (dir_data, "results-data-issue-bugs.Rds"))
    saveRDS (data_pkgcheck, fs::path (dir_data, "results-pkgcheck.Rds"))

    # -------- SAVE JSON DATA
    # Observable FileAttachment requires string literals which can't be build
    # in platform-independent ways, so these are dumped in root.
    jsonlite::write_json (data_json, fs::path (dir, "results-json-data.json"))

    saveRDS (fn_calls, fs::path (dir_data, "fn-calls.Rds"))
    saveRDS (similarities, fs::path (dir_data, "similarities.Rds"))

    withr::with_dir (dir, {
        do.call (eval (parse (text = quarto_action)), list ())
    })
}

dashboard_data_repo_metrics <- function (data_metrics, dates) {

    # Suppress no visible binding notes:
    org <- package <- date <- name <- NULL

    # Repo summaries for repo page:
    repo_summary_vars <- c (
        "change_req_n_opened",
        "code_change_lines",
        "commit_count",
        "committer_count",
        "cran_downloads",
        "ctb_count",
        "ctb_diversity",
        "dependency_count",
        "issues_active",
        "issue_cmt_count",
        "maintainer_count",
        "num_commits",
        "num_contributors",
        "num_forks",
        "num_stars",
        "recent_releases",
        "release_freq",
        "test_coverage"
    )
    repo_metrics <- data_metrics |>
        dplyr::filter (date == max (dates)) |>
        dplyr::select (-org, -date) |>
        tidyr::pivot_longer (-package) |>
        dplyr::filter (name %in% repo_summary_vars)

    pkgs <- unique (repo_metrics$package)
    repo_metrics <- split (repo_metrics, f = as.factor (repo_metrics$package)) |>
        lapply (function (m) dplyr::select (m, -package))

    return (repo_metrics)
}

dashboard_data_contributors <- function (data_org) {

    # Suppress no visible binding notes:
    name <- login <- contributions <- NULL

    data_contributors <- lapply (data_org$repos, function (repo) {
        ctbs_gh <- repo$rm$contribs_from_gh_api |>
            dplyr::select (login, name, contributions)
        repo$rm$contributors |>
            dplyr::left_join (ctbs_gh, by = c ("gh_handle" = "login", "name")) |>
            dplyr::filter (!name == "GitHub Actions") |>
            dplyr::arrange (dplyr::desc (contributions))
    })
    names (data_contributors) <- gsub ("^.*\\/", "", names (data_contributors))

    return (data_contributors)
}

# Map from repos to org/repo:
dashboard_data_repo_source <- function (data_org) {

    data_repo_src <- names (data_org$repos)
    data.frame (
        package = gsub ("^.*\\/", "", data_repo_src),
        orgrepo = data_repo_src,
        url = paste0 ("https://github.com/", data_repo_src)
    )
}

dashboard_data_cran <- function (data_org) {

    # Suppress no visible binding notes:
    result <- NULL

    data_cran <- lapply (data_org$repos, function (i) {
        if (!inherits (i$cran_checks, "data.frame")) {
            return (i$cran_checks)
        }
        dplyr::filter (i$cran_checks, result != "OK")
    })
    not_cran <- which (vapply (
        data_cran,
        function (i) !inherits (i, "data.frame"),
        logical (1L)
    ))
    index <- seq_along (data_cran) [-(not_cran)]
    data_cran <- do.call (rbind, data_cran [index])
    attr (data_cran, "not_cran") <- names (not_cran)
    rownames (data_cran) <- NULL

    return (data_cran)
}

dashboard_data_gitlog <- function (data_org) {

    data_gitlog <- t (vapply (data_org$repos, function (i) {
        c (
            num_commits = as.character (nrow (i$rm$gitlog)),
            first_commit = as.character (as.Date (min (i$rm$gitlog$timestamp)))
        )
    }, character (2L)))

    data.frame (
        package = gsub ("^.*\\/", "", rownames (data_gitlog)),
        num_commits = as.integer (data_gitlog [, 1]),
        first_commit = as.Date (data_gitlog [, 2]),
        row.names = NULL
    )
}

dashboard_data_r_universe <- function (data_org) {

    dat <- lapply (data_org$repos, function (i) {
        c (i$rm$r_universe$universe, i$rm$r_universe$package)
    })
    dat <- do.call (rbind, unname (dat))
    data_is_on_r_univ <- data.frame (universe = dat [, 1], package = dat [, 2])

    r_univ_jobs <- lapply (data_org$repos, function (i) {
        data.frame (
            universe = i$rm$r_universe$universe,
            package = i$rm$r_universe$package,
            i$rm$r_universe$jobs
        )
    })
    r_univ_jobs <- do.call (rbind, r_univ_jobs)
    rownames (r_univ_jobs) <- NULL
    r_univ_jobs <- split (r_univ_jobs, f = as.factor (r_univ_jobs$package))

    r_univ_builds <- lapply (data_org$repos, function (i) {
        data.frame (
            universe = i$rm$r_universe$universe,
            package = i$rm$r_universe$package,
            i$rm$r_universe$binaries
        )
    })
    r_univ_builds <- do.call (rbind, r_univ_builds)
    rownames (r_univ_builds) <- NULL
    r_univ_builds <- split (r_univ_builds, f = as.factor (r_univ_builds$package))

    list (
        data_is_on_r_univ = data_is_on_r_univ,
        r_univ_jobs = r_univ_jobs,
        r_univ_builds = r_univ_builds
    )
}

# data_contributors is package-based; this inverts to be maintainer-based:
databoard_data_maintainers <- function (data_contributors) {

    data_maintainers <- do.call (rbind, data_contributors)
    pkgs <- gsub ("\\.[0-9]+$", "", rownames (data_maintainers))
    rownames (data_maintainers) <- NULL
    data_maintainers$package <- pkgs

    maintainer_pkgs <- data_maintainers |>
        dplyr::arrange (gh_handle, package) |>
        dplyr::filter (!is.na (gh_handle)) |>
        dplyr::select (gh_handle, package)
    maintainer_pkgs_json <- lapply (split (
        maintainer_pkgs,
        f = as.factor (maintainer_pkgs$gh_handle)
    ), function (m) unique (m$package))

    comaintainers <- maintainer_pkgs |>
        dplyr::filter (!is.na (gh_handle)) |>
        dplyr::arrange (package) |>
        dplyr::select (gh_handle, package) |>
        dplyr::distinct ()
    comaintainers <- lapply (split (
        comaintainers,
        f = as.factor (comaintainers$package)
    ), function (m) {
        if (nrow (m) < 2L) {
            return (NULL)
        }
        t (combn (unique (m$gh_handle), 2L))
    })
    comaintainers <- rbind (
        do.call (rbind, comaintainers),
        do.call (rbind, comaintainers) [, 2:1]
    ) |>
        data.frame () |>
        dplyr::distinct () |>
        dplyr::arrange (X1)
    comaintainers_json <- lapply (split (
        comaintainers,
        f = as.factor (comaintainers$X1)
    ), function (i) i$X2)

    list (
        maintainers = maintainer_pkgs_json,
        comaintainers = comaintainers_json
    )
}

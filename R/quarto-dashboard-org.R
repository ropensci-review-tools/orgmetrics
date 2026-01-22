#' Start quarto dashboard with results of
#' `orgmetrics_collate_org_data` function for collation of data across orgs.
#'
#' @param data_org Data on GitHub organization as returned from
#' `orgmetrics_collate_org_data` function.
#' @param fn_calls Data on function calls between packages of the specified
#' organization, as returned from the `rm_org_data_fn_call_network()` function.
#' @param embeddings List of language model embeddings returned from
#' `rm_org_emb_distances()`. These are calculated with the 'pkgmatch' package
#' which in turn relies on \url{https://ollama.com}.
#' @param action One of "preview", to start and open a live preview of the
#' dashboard website, "render" to render a static version without previewing
#' or opening, or `NULL` to set up the quarto structure in the current
#' temporary directory without doing anything. This option is useful to
#' generate the dashboard structure so that it can be moved to a non-temporary
#' location, and deployed or previewed from there.
#' @param title If not `NULL` (default), a string specifying the organizational
#' title for the dashboard.
#' @return (Invisibly) Path to main "index.html" document of quarto site. Note
#' that the site must be served with `action = "preview"`, and will not work by
#' simply opening this "index.html" file.
#' @export
orgmetrics_dashboard <- function (data_org,
                                  fn_calls,
                                  embeddings,
                                  title = NULL,
                                  action = "preview") {

    # Suppress no visible binding notes:
    org <- package <- NULL

    if (!is.null (action)) {
        action <- match.arg (action, c ("preview", "render"))
        quarto_action <- paste0 ("quarto::quarto_", action)
    }

    requireNamespace ("jsonlite", quietly = TRUE)
    requireNamespace ("quarto", quietly = TRUE)
    requireNamespace ("withr", quietly = TRUE)

    # -------- CHAOSS MODELS and METRICS: START -------
    cli::cli_inform ("   -> data_models_preprocess")
    data_models <- data_models_preprocess (data_org$models) |>
        dplyr::select (-org, -date) |>
        tidyr::pivot_longer (-package)

    cli::cli_inform ("   -> data_metrics_to_df")
    data_metrics <- data_metrics_to_df (data_org$metrics)

    dates <- sort (unique (data_metrics$date), decreasing = TRUE)
    cli::cli_inform ("   -> dashboard_data_repo_metrics")
    repo_metrics <- dashboard_data_repo_metrics (data_metrics, dates)

    cli::cli_inform ("   -> data_metrics_preprocess")
    data_metrics <- lapply (dates, function (d) {
        data_metrics |>
            dplyr::filter (date == d) |>
            data_metrics_preprocess ()
    })
    names (data_metrics) <- dates
    # -------- CHAOSS MODELS and METRICS: END -------

    # -------- ADDITIONAL DATA IN R -------
    cli::cli_inform ("   -> org_maintenance_metric")
    data_maintenance <- org_maintenance_metric (data_org)
    cli::cli_inform ("   -> dashboard_data_releases")
    data_releases <- dashboard_data_releases (data_org)
    cli::cli_inform ("   -> ctb_absence")
    data_abs <- ctb_absence (data_org)
    cli::cli_inform ("   -> issue_responses")
    data_resp <- issue_responses (data_org)
    cli::cli_inform ("   -> issue_bugs")
    data_bugs <- issue_bugs (data_org)
    cli::cli_inform ("   -> dashboard_data_contributors")
    data_contributors <- dashboard_data_contributors (data_org)
    cli::cli_inform ("   -> dashboard_data_repo_source")
    data_repo_src <- dashboard_data_repo_source (data_org)
    data_pkgcheck <- lapply (data_org$repos, function (i) i$pkgcheck)

    # -------- ADDITIONAL DATA IN JSON -------
    # All saved as single JSON structure; only used in 'repo.qmd' and 'maintainer.qmd'
    cli::cli_inform ("   -> dashboard_data_cran")
    data_cran <- dashboard_data_cran (data_org)
    not_cran <- NULL
    if (length (data_cran) > 0L) {
        not_cran <- gsub ("^.*\\/", "", attr (data_cran, "not_cran"))
        attr (data_cran, "not_cran") <- NULL
    }

    cli::cli_inform ("   -> dashboard_data_gitlog")
    data_gitlog <- dashboard_data_gitlog (data_org)
    cli::cli_inform ("   -> dashboard_data_r_universe")
    data_r_universe <- dashboard_data_r_universe (data_org)
    rm_metrics_json <- system.file (
        "extdata",
        "metrics-models",
        "metrics-models.json",
        package = "repometrics"
    )
    rm_metrics_models <- jsonlite::read_json (rm_metrics_json, simplify = TRUE)
    cli::cli_inform ("   -> dashboard_data_maintainers")
    maintainers <- dashboard_data_maintainers (data_contributors)

    data_json <- list (
        cran = data_cran,
        not_cran = not_cran,
        gitlog = data_gitlog,
        data_releases = data_releases,
        r_universe = data_r_universe,
        rm_metrics_models = rm_metrics_models,
        maintainer_pkgs = maintainers$maintainers,
        comaintainers = maintainers$comaintainers
    )

    path_src <- system.file ("extdata", "quarto", package = "orgmetrics")
    path_dest <- fs::path (fs::path_temp (), "quarto")
    dir <- fs::dir_copy (path_src, path_dest, overwrite = TRUE)
    if (!is.null (title)) {
        update_quarto_yml_title (dir, title)
    }
    orgs <- names (get_orgs_from_data (data_org))
    f <- update_quarto_yml_file (dir, orgs)
    f <- update_quarto_orgnames (dir, orgs, title)

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

    embeddings_are_similarities <- vapply (
        embeddings,
        function (i) length (unique (dim (i))) == 1L,
        logical (1L)
    )
    if (all (embeddings_are_similarities)) {
        similarities <- embeddings_to_similarities (embeddings)
    } else {
        similarities <- embeddings
    }
    saveRDS (similarities, fs::path (dir_data, "similarities.Rds"))

    if (!is.null (action)) {
        withr::with_dir (dir, {
            do.call (eval (parse (text = quarto_action)), list ())
        })
    }

    invisible (dir)
}

# Repo summaries for repo page:
get_repo_summary_vars <- function () {
    c (
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
}

dashboard_data_repo_metrics <- function (data_metrics, dates) {

    # Suppress no visible binding notes:
    org <- package <- date <- name <- NULL

    repo_vars <- c ("package", get_repo_summary_vars ())

    repo_metrics <- data_metrics |>
        dplyr::filter (date == max (dates)) |>
        dplyr::select (-org, -date) |>
        dplyr::select (dplyr::all_of (repo_vars)) |>
        tidyr::pivot_longer (-package)

    pkgs <- unique (repo_metrics$package)
    repo_metrics <- split (repo_metrics, f = as.factor (repo_metrics$package)) |>
        lapply (function (m) dplyr::select (m, -package))

    return (repo_metrics)
}

match_names <- utils::getFromNamespace ("match_names", "repometrics")

dashboard_data_contributors <- function (data_org, desc_name_match = 0.8) {

    # Suppress no visible binding notes:
    name <- login <- contributions <- what <- NULL

    data_contributors <- lapply (data_org$repos, function (repo) {

        auts <- gsub ("(\\s?)(\\[|<).*$", "", repo$authors)
        ctbs <- repo$rm$contribs_from_gh_api |>
            dplyr::select (login, name, contributions) |>
            dplyr::filter (!grepl ("github\\-actions|\\[bot\\]", login))
        aut_matches <- do.call (rbind, lapply (
            auts,
            function (a) {
                rbind (
                    cbind (match_names (a, ctbs$name) [1, ], what = "name"),
                    cbind (match_names (a, ctbs$login) [1, ], what = "login")
                )
            }
        )) |> data.frame ()
        login_matches <- dplyr::filter (aut_matches, what == "login")

        # If there are no aut_matches, df only has "what"
        if (nrow (aut_matches) > 0L && is.numeric (aut_matches$match)) {
            aut_matches <- aut_matches |>
                dplyr::filter (what == "name") |>
                dplyr::filter (match >= desc_name_match)
        }
        if (nrow (aut_matches) == 0) {
            desc_name_match <- desc_name_match * max (login_matches$match)
        }

        if (is.numeric (login_matches$match)) {
            # FALSE is no aut_matches at all
            login_matches <- login_matches |>
                dplyr::filter (match >= desc_name_match)
        }

        ctbs$is_author <- ctbs$name %in% aut_matches$name |
            ctbs$login %in% login_matches

        return (ctbs)
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
    if (length (not_cran) > 0L) {
        data_cran <- data_cran [-(not_cran)]
    }

    if (length (data_cran) > 0L) {
        data_cran <- do.call (rbind, data_cran)
        attr (data_cran, "not_cran") <- names (not_cran)
        rownames (data_cran) <- NULL
    }

    return (data_cran)
}

dashboard_data_gitlog <- function (data_org, period = 365) {

    end_date <- attr (data_org, "end_date")
    if (length (end_date) == 0L) {
        end_date <- Sys.Date ()
    }
    start_date <- end_date - 365

    data_gitlog <- t (vapply (data_org$repos, function (i) {
        log <- i$rm$gitlog
        index <- which (as.Date (log$timestamp) >= start_date)
        c (
            num_commits = as.character (nrow (log)),
            first_commit = as.character (as.Date (min (log$timestamp))),
            latest_commit = as.character (as.Date (log$timestamp [1])),
            recent_commits = as.character (length (index))
        )
    }, character (4L)))

    data.frame (
        package = gsub ("^.*\\/", "", rownames (data_gitlog)),
        num_commits = as.integer (data_gitlog [, 1]),
        first_commit = as.Date (data_gitlog [, 2]),
        latest_commit = as.Date (data_gitlog [, 3]),
        recent_commits = as.integer (data_gitlog [, 4]),
        row.names = NULL
    )
}

dashboard_data_r_universe <- function (data_org) {

    dat <- lapply (data_org$repos, function (i) {
        c (i$rm$r_universe$universe, i$rm$r_universe$package)
    })
    dat <- do.call (rbind, unname (dat))
    data_is_on_r_univ <- data.frame (universe = dat [, 1], package = dat [, 2])

    r_univ_stats <- vapply (data_org$repos, function (i) {
        r_i <- i$rm$r_universe
        if (length (r_i) == 0L) {
            return (rep (NA_real_, 3L))
        }
        c (r_i$score, r_i$downloads, r_i$n_scripts)
    }, numeric (3L))
    r_univ_stats <- data.frame (
        universe = gsub ("\\/.*$", "", colnames (r_univ_stats)),
        package = gsub ("^.*\\/", "", colnames (r_univ_stats)),
        score = r_univ_stats [1, ],
        downloads = as.integer (r_univ_stats [2, ]),
        n_scripts = as.integer (r_univ_stats [3, ]),
        row.names = NULL
    )

    make_r_univ_df <- function (repos, what = "jobs") {
        dat <- lapply (repos, function (i) {
            data.frame (
                universe = i$rm$r_universe$universe,
                package = i$rm$r_universe$package,
                i$rm$r_universe [[what]]
            )
        })
        dat <- do.call (rbind, dat)
        rownames (dat) <- NULL
        split (dat, f = as.factor (dat$package))
    }

    r_univ_jobs <- make_r_univ_df (data_org$repos, "jobs")
    r_univ_builds <- make_r_univ_df (data_org$repos, "binaries")

    list (
        data_is_on_r_univ = data_is_on_r_univ,
        r_univ_stats = r_univ_stats,
        r_univ_jobs = r_univ_jobs,
        r_univ_builds = r_univ_builds
    )
}

# data_contributors is package-based; this inverts to be maintainer-based:
dashboard_data_maintainers <- function (data_contributors) {

    # Suppress no visible binding notes:
    gh_handle <- package <- login <- X1 <- NULL

    data_maintainers <- do.call (rbind, data_contributors)
    pkgs <- gsub ("\\.[0-9]+$", "", rownames (data_maintainers))
    rownames (data_maintainers) <- NULL
    data_maintainers$package <- pkgs

    maintainer_pkgs <- data_maintainers |>
        dplyr::arrange (login, package) |>
        dplyr::filter (!is.na (login)) |>
        dplyr::select (login, package)
    maintainer_pkgs_json <- lapply (split (
        maintainer_pkgs,
        f = as.factor (maintainer_pkgs$login)
    ), function (m) unique (m$package))

    comaintainers <- maintainer_pkgs |>
        dplyr::filter (!is.na (login)) |>
        dplyr::arrange (package) |>
        dplyr::select (login, package) |>
        dplyr::distinct ()
    comaintainers <- lapply (split (
        comaintainers,
        f = as.factor (comaintainers$package)
    ), function (m) {
        if (nrow (m) < 2L) {
            return (NULL)
        }
        t (utils::combn (unique (m$login), 2L))
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

dashboard_data_releases <- function (data_org) {

    rel_dat <- t (vapply (data_org$repos, function (repo) {
        releases <- repo$rm$releases_from_gh_api
        if (nrow (releases) == 0L) {
            return (rep (NA_character_, 3L))
        }

        latest <- as.Date (releases$published_at [1])
        intervals <- diff (as.Date (releases$published_at))
        intervals <- as.integer (-intervals)
        rel_per_year <- NA
        if (length (intervals) > 0L) {
            rel_per_year <- format (365 / mean (intervals), digits = 2)
        }

        c (as.character (latest), as.character (nrow (releases)), rel_per_year)
    }, character (3L)))

    rel_dat <- data.frame (
        package = gsub ("^.*\\/", "", rownames (rel_dat)),
        latest = rel_dat [, 1],
        total = as.integer (rel_dat [, 2]),
        rel_per_year = as.numeric (rel_dat [, 3]),
        row.names = NULL
    )
    rel_dat$latest [which (is.na (rel_dat$latest))] <- "no"
    rel_dat$total [which (is.na (rel_dat$total))] <- 0
    rel_dat$rel_per_year [which (is.na (rel_dat$rel_per_year))] <- 0

    return (rel_dat)
}

copy_pkg_logos <- function (data_org, path) {

    logos_dir <- fs::path (path, "logos")
    if (!fs::dir_exists (logos_dir)) {
        fs::dir_create (logos_dir, recurse = TRUE)
    }

    lapply (data_org$repos, function (repo) {

        path <- fs::path (repo$pkgcheck$pkg$path, "man")
        message (path)

        man_subdirs <- fs::dir_ls (path, type = "directory")
        logo_path <- NULL

        if (length (man_subdirs) > 0L) {
            i <- grep ("figure", basename (man_subdirs))
            if (length (i) == 1L) {
                man_fig_subdir <- man_subdirs [i [1]]
                figs <- fs::dir_ls (man_fig_subdir, type = "file")
                fig_i <- grepl ("logo", basename (figs), ignore.case = TRUE)
                if (length (fig_i) == 1L) {
                    logo_path <- figs [fig_i]
                }
            }
        }

        if (!is.null (logo_path)) {
            pkg_name <- repo$pkgcheck$pkg$name
            logo_ext <- fs::path_ext (logo_path)
            f <- fs::path (logos_dir, paste0 (pkg_name, ".", logo_ext))
            if (!fs::file_exists (f)) {
                fs::file_copy (logo_path, f)
            }
        }
    })
}

get_orgs_from_data <- function (data_org, threshold = 0.02) {

    orgs <- gsub ("\\/.*$", "", names (data_org$repos)) |>
        table () |>
        sort (decreasing = TRUE)
    orgs [which (orgs / sum (orgs) > threshold)]
}

update_quarto_yml_file <- function (path, orgs) {

    f <- fs::dir_ls (path, regexp = "\\_quarto\\.y(a?)ml$")
    y <- yaml::read_yaml (f)

    orgs_fmt <- paste0 ("github.com/", orgs)

    menu <- y$website$navbar$right [[1]]$menu
    menu <- c (
        menu [1:2],
        lapply (orgs_fmt, function (o) {
            list (text = o, url = paste0 ("https://", o))
        })
    )
    y$website$navbar$right [[1]]$menu <- menu

    yaml::write_yaml (
        y,
        file = f,
        indent.mapping.sequence = TRUE,
        handlers = list (logical = yaml::verbatim_logical)
    )

    invisible (f)
}

update_quarto_yml_title <- function (dir, title) {

    requireNamespace ("yaml", quietly = TRUE)

    qml_file <- fs::dir_ls (dir, regexp = "\\_quarto\\.yml$", type = "file")
    qml <- yaml::read_yaml (qml_file)
    qml$website$title <- title
    # But standard yaml::write_yaml fails to re-map these items for quarto
    # yaml format:
    qml$website$navbar$left [[2]]$text <-
        quarto::yaml_quote_string (qml$website$navbar$left [[2]]$text)
    qml$website$navbar$right [[1]]$icon <-
        quarto::yaml_quote_string (qml$website$navbar$right [[1]]$icon)
    yaml::write_yaml (
        qml,
        file = qml_file,
        indent.mapping.sequence = TRUE,
        handlers = list (logical = yaml::verbatim_logical)
    )

    # Then also the title in "index.qmd"
    index_file <- fs::dir_ls (dir, regexp = "\\index\\.qmd$", type = "file")
    index <- readLines (index_file)
    i <- grep ("title\\:\\s\"", index)
    index [i] <- gsub ("EpiVerse", title, index [i])
    writeLines (index, index_file)
}

update_quarto_orgnames <- function (dir, orgs, title = NULL) {

    if (is.null (title)) {
        org <- orgs [1]
    } else {
        org <- title
    }
    for (what in c ("index", "models")) {
        f <- fs::path (dir, paste0 (what, ".qmd"))
        stopifnot (fs::file_exists (f))
        x <- readLines (f)
        x <- gsub ("aaa", org, x)
        if (!is.null (title)) {
            x <- gsub ("GitHub\\sorganization", "universe", x)
        }
        writeLines (x, f)
    }
}

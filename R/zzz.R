# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    op <- options ()

    op.orgmetrics <- list ( # nolint
        repometrics_period = 90
    )

    toset <- !(names (op.orgmetrics) %in% names (op))
    if (any (toset)) {
        options (op.orgmetrics [toset])
    }
    invisible ()
}
# nocov end

get_repometrics_period <- function () {
    period <- getOption ("repometrics_period")
    checkmate::assert_numeric (period, lower = 1L)
    return (period)
}

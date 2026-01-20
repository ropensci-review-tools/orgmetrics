#' Convert single values of length 0 or NULL to `NA_integer`.
#' @noRd
null2na_int <- function (x) {
    ifelse (length (x) == 0, NA_integer_, x)
}

#' Convert single values of length 0 or NULL to `NA_character_`.
#' @noRd
null2na_char <- function (x) {
    ifelse (length (x) == 0, NA_character_, x)
}

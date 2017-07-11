#' @export
is_course <- function(x) {
    assertthat::assert_that(assertthat::is.count(x), length(x) == 1)
    x %in% c(180, 200)
}

assertthat::on_failure(is_course) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid course number")
}


#' @export
is_quarter <- function(x) {
    assertthat::assert_that(assertthat::is.string(x), length(x) == 1)
    toupper(x) %in% c("AU", "WI", "SP", "SU")
}

assertthat::on_failure(is_quarter) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid quarter")
}


#' @export
is_section <- function(x) {
    assertthat::assert_that(assertthat::is.string(x), length(x) == 1)
    stringi::stri_detect_regex(x, "^[A-Za-z]{1,2}$")
}

assertthat::on_failure(is_section) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid section")
}


# TODO: is_?

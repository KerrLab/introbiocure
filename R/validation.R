#' Validate Values Given
#'
#' @description \code{is_course} determines whether or not a given course number
#' is valid. Valid course numbers are 180 and 200.
#'
#' @param x Value to test
#'
#' @return Logical value indicating whether or not input \code{x} is a valid
#' value
#' @export
#' @rdname validation
#'
#' @examples
#' is_course(180)
is_course <- function(x) {
    assertthat::assert_that(assertthat::is.count(x), length(x) == 1)
    x %in% c(180, 200)
}

assertthat::on_failure(is_course) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid course number")
}


#' @rdname validation
#' @description \code{is_quarter} determines whether or not a given quarter
#' name is valid. Valid values include "AU" (Autumn), "WI" (Winter),
#' "SP" (Spring), and "SU" (Summer)
#' @export
#' @examples
#' is_quarter("WI")
is_quarter <- function(x) {
    assertthat::assert_that(assertthat::is.string(x), length(x) == 1)
    toupper(x) %in% c("AU", "WI", "SP", "SU")
}

assertthat::on_failure(is_quarter) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid quarter")
}


#' @rdname validation
#' @description \code{is_section} Determines whether or not a given section name
#' is valid. Section names are letters A-Z. When more than 26 sections exist,
#' two letter codes are used.
#' @export
#' @examples
#' is_section("C")
is_section <- function(x) {
    assertthat::assert_that(assertthat::is.string(x), length(x) == 1)
    stringi::stri_detect_regex(x, "^[A-Za-z]{1,2}$")
}

assertthat::on_failure(is_section) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid section")
}


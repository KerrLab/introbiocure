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


#' @rdname validation
#' @description \code{is_drug} Determines whether or not a given antibiotic
#' is valid (in the context of the courses)
#' @export
#' @examples
#' is_drug("Streptomycin")
is_drug <- function(x) {
    assertthat::assert_that(assertthat::is.string(x), length(x) == 1)
    tolower(x) %in% c("rifampicin", "streptomycin")
}

assertthat::on_failure(is_drug) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid antibiotic")
}


#' @rdname validation
#' @description \code{is_drug_abbr} Determines whether or not a given
#' abbreviated antibiotic is valid (in the context of the courses)
#' @export
#' @examples
#' is_drug("STR")
is_drug_abbr <- function(x) {
    assertthat::assert_that(assertthat::is.string(x), length(x) == 1)
    tolower(x) %in% c("RIF", "STR")
}


#' @rdname validation
#' @description \code{is_strain_id} Determines whether or not a given
#' string is a valid strain ID. Valid strain IDs contain two uppercase letters
#' followed by three digits.
#' @export
#' @examples
#' is_strain_id("AG103")
is_strain_id <- function(x) {
    assertthat::assert_that(assertthat::is.string(x), length(x) == 1)
    stringi::stri_detect_regex(x, "^[A-Z]{2}[0-9]{3}$")
}

#' Convert Numbers to/from Strain IDs
#'
#' Strain IDs are 5-character strings with two uppercase letters followed by
#' three digits.
#'
#' @param x A number (\code{number_as_strainid}) or strain ID (\code{strainid_as_number})
#'
#' @return \code{number_as_strainid} returns a string, and \code{strainid_as_number} returns a numeric value
#' @export
#'
#' @examples
#' strainid_as_number("BC828")
#' number_as_strainid(90210)
strainid_as_number <- function(x) {
    valid <- vapply(x, is_strain_id, logical(1))

    if (!all(valid)) {
        stop(paste("invalid strain ID(s)", paste0(x[valid == FALSE], collapse = ", "), sep = ": "))
    }

    m <- stringi::stri_match_first_regex(str = x, pattern = "^([A-Z])([A-Z])([0-9]{3})$")
    apply(m, 1, function(x) { as.numeric(x[4]) + ((utf8ToInt(x[3]) - 65) * 10^3) + ((utf8ToInt(x[2]) - 65) * 26 * 10^3) })
}

#' @rdname strainid_as_number
#' @export
number_as_strainid <- function(x) {
    assertthat::assert_that(is.numeric(x))
    sprintf("%s%s%03d", LETTERS[floor(x / 26000) + 1], LETTERS[floor((x %% 26000)/ 1000) + 1], x %% 1000)
}

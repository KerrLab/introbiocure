#' Convert Numbers to/from Section Names
#'
#' @param x A number (\code{number_as_section}) or strain ID (\code{section_as_number})
#'
#' @return \code{number_as_ssection} returns a string, and \code{section_as_number} returns a numeric value
#' @export
#'
#' @examples
#' section_as_number("C")
#' number_as_section(36)
#'
section_as_number <- function(x) {
    m <- stringi::stri_match_first_regex(str = x, pattern = "^([A-Z]?)([A-Z])$")
    apply(
        m,
        1,
        function(x) {
            utf8ToInt(x[3]) - 64 + ifelse(nchar(x[2]) > 0, (utf8ToInt(x[2]) - 64) * 26, 0)
        }
    )
}

#' @rdname section_as_number
#' @export
number_as_section <- function(x) {
    assertthat::assert_that(all(x > 0), all(x <= 52))

    paste0(
        ifelse(x > 26, LETTERS[floor(x / 26)], ""),
        LETTERS[1 + ((x - 1) %% 26)]
    )
}

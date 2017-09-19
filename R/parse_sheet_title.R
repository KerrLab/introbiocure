#' Parse a Course Spreadsheet Title
#'
#' @param title Title for a course spreadsheet (e.g., "BIO200 WI2018 Section B")
#'
#' @return A list of lists, where each list contains information about the
#' corresponding title
#' @export
#'
#' @examples
#' parse_sheet_title("BIO200 WI2018 Section B")
#'
parse_sheet_title <- function(title) {

    x <- stringi::stri_match_first_regex(
        str = title,
        pattern = "BIO(180|200) (AU|WI|SP|SU)([0-9]{4}) Section ([A-Za-z]{1,2})"
    )

    apply(x, 1, function(x) {
        list(
            Course = as.integer(x[2]),
            Year = as.integer(x[4]),
            Quarter = x[3],
            Section = x[5])
    })
}

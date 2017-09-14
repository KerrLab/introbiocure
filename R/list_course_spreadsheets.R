#' List Existing Course Spreadsheets
#'
#' \code{list_course_spreadsheets} creates a table with information about
#' existing course spreadsheets. If any arguments are provided, only sheets
#' matching those parameters are shown. For example, if \code{course = 180},
#' only sheets for BIO 180 are returned.
#'
#' @param course The course number (e.g., 180 or 200) (optional)
#' @param year The year (optional)
#' @param quarter The quarter (optional)
#' @param section The section name (optional)
#'
#' @return A data frame with one row per matching sheet
#' @export
#'
#' @examples
#' \dontrun{
#' sheets_2017 <- list_course_spreadsheets(year = 2017)
#' }
list_course_spreadsheets <- function(course = NULL,
                                     year = NULL,
                                     quarter = NULL,
                                     section = NULL) {
    pattern <- sprintf(
        "BIO%s %s%s Section %s",
        ifelse(is.null(course), "(180|200)", course),
        ifelse(is.null(quarter), "(AU|WI|SP|SU)", toupper(quarter)),
        ifelse(is.null(year), "[0-9]{4}", year),
        ifelse(is.null(section), "[A-Za-z]{1,2}", toupper(section))
    )

    sheets <- googlesheets::gs_ls(regex = pattern)
    x <- stringi::stri_match_first_regex(
        str = sheets$sheet_title,
        pattern = "^BIO(180|200) (AU|WI|SP|SU)([0-9]{4}) Section ([A-Za-z]{1,2})$"
    )

    sheets <- tibble::add_column(
        sheets,
        Course = as.numeric(x[,2]),
        Quarter = x[,3],
        Year = as.numeric(x[,4]),
        Section = x[,5],
        .after = "sheet_title"
    )

    sheets
}


#' @rdname list_course_spreadsheets
#' @description \code{sheet_exists} determines whether or not a spreadsheet with
#' the given title exists. Note that the match is case sensitive, so "my sheet"
#' does not match "My Sheet".
#' @param title Spreadsheet title
#' @export
sheet_exists <- function(title) {
    # alternate:
    # sheets <- googlesheets::gs_ls()
    # tolower(title) %in% tolower(sheets$sheet_title)
    !is.null(googlesheets::gs_ls(regex = sprintf("^%s$", title)))
}

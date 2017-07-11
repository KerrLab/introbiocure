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
#' sheets_2017 <- list_course_spreadsheets(year = 2017)
#'
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

    # TODO: add columns for course, year, quarter, section
    googlesheets::gs_ls(regex = pattern)
}

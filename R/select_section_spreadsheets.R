#' Select Section Sheets Based on Course, Year, Quarter, and/or Section
#'
#' \code{select_section_spreadsheets} selects course spreadsheets. If any arguments
#' are provided, only sheets matching those parameters are shown. For example,
#' if \code{course = 180}, only sheets for BIO 180 are returned
#'
#' @param course The course number (e.g., 180 or 200) (optional)
#' @param year The year (optional)
#' @param quarter The quarter (optional)
#' @param section The section name (optional)
#'
#' @return A vector of spreadsheet keys
#' @export
#'
#' @examples
#' \dontrun{
#' select_section_spreadsheets(course = 180)
#' }
select_section_spreadsheets <- function(course = NULL,
                                  year = NULL,
                                  quarter = NULL,
                                  section = NULL) {
    dplyr::pull(
        list_course_spreadsheets(
            course = course,
            year = year,
            quarter = quarter,
            section = section
        ),
        "sheet_key"
    )
}

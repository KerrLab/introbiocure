#' Open the Google Sheet for a BIO 180/200 section
#'
#' @param course The course number (e.g., 180 or 200)
#' @param year The year
#' @param quarter The quarter. One of: "AU", "WI", "SP", "SU".
#' @param section The section name (e.g., "G")
#'
#' @export
#'
#' @examples
#' \dontrun{
#' view_section_spreadsheet(course = 180, year = 2017, quarter = "AU", section = "C")
#' }
view_section_spreadsheet <- function(course, year, quarter, section) {
    section_title <- build_section_spreadsheet_title(
        course = course,
        year = year,
        quarter = quarter,
        section = section
    )
    googlesheets::gs_browse(googlesheets::gs_title(section_title))
}


#' @rdname view_section_spreadsheet
#' @export
view_section_spreadsheet_180 <- function(year, quarter, section) {
    view_section_spreadsheet(
        course = 180,
        year = year,
        quarter = quarter,
        section = section
    )
}

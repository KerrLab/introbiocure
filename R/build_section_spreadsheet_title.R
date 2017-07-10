#' Title TODO
#'
#' @param course The course number (e.g., 180 or 200)
#' @param year The year
#' @param quarter The quarter. One of: "AU", "WI", "SP", "SU".
#' @param section The section name (e.g., "G")
#'
#' @return TODO
#' @export
#'
#' @examples
#' build_section_title(180, 2017, "AU", "C")
build_section_spreadsheet_title <- function(course, year, quarter, section) {
    # TODO add assertions
    sprintf("BIO%s %s%d Section %s", course, toupper(quarter), year, toupper(section))
}

#' Title TODO
#'
#' @param course The course number (e.g., 180 or 200)
#' @param year The year
#' @param quarter The quarter. One of: "AU", "WI", "SP", "SU".
#' @param section The section name (e.g., "G"). Section names are named with a single letter from A-Z. When more than 26 sections exist, subsequent sections are named AA-AZ.
#'
#' @return A string containing the title for the corresponding spreadsheet
#' @export
#'
#' @examples
#' build_section_spreadsheet_title(180, 2017, "AU", "C")
#'
build_section_spreadsheet_title <- function(course, year, quarter, section) {
    assertthat::assert_that(is_course(course))
    assertthat::assert_that(assertthat::is.count(year))
    assertthat::assert_that(year > 2016)
    assertthat::assert_that(is_quarter(quarter))
    assertthat::assert_that(is_section(section))

    sprintf("BIO%s %s%d Section %s", course, toupper(quarter), year,
            toupper(section))
}

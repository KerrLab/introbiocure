#' Create a Google Sheet for a BIO 180 section
#'
#' @param year The year
#' @param quarter The quarter. One of: "AU", "WI", "SP", "SU".
#' @param section The section name (e.g., "G")
#' @param num_groups The number of groups to create
#' @param create_group_0 Whether or not to create a group 0 for controls (default: TRUE)
#' @param ... Additional arguments (ignored)
#'
#' @return A \code{\link[googlesheets]{googlesheet}} object
#' @export
#'
#' @examples
#' \dontrun{
#' s4 <- create_section_spreadsheet_180(year = 2017, quarter = "WI", section = "C", num_groups = 5, create_group_0 = TRUE)
#' }
create_section_spreadsheet_180 <- function(year, quarter, section, num_groups, create_group_0 = TRUE, ...) {
    21
}

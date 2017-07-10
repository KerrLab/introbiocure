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
#' @importFrom rlang !!
#' @export
#'
#' @examples
#' \dontrun{
#' s4 <- create_section_spreadsheet_180(year = 2017, quarter = "WI", section = "C", num_groups = 5, create_group_0 = TRUE)
#' }
create_section_spreadsheet_180 <- function(year, quarter, section, num_groups, create_group_0 = TRUE, ...) {
    assertthat::assert_that(assertthat::is.count(year))
    assertthat::assert_that(year > 2016)
    assertthat::assert_that(assertthat::is.string(quarter))
    assertthat::assert_that(toupper(quarter) %in% c("AU", "WI", "SP", "SU"))
    assertthat::assert_that(assertthat::is.string(section))
    # TODO: validate section string
    assertthat::assert_that(assertthat::is.count(num_groups))
    assertthat::assert_that(assertthat::is.flag(create_group_0))

    section_title <- build_section_spreadsheet_title(
        course = 180,
        year = year,
        quarter = quarter,
        section = section
    )

    section_data <- tibble::as.tibble(
            expand.grid(
            Year = year,
            Quarter = toupper(quarter),
            Section = section,
            Group = seq(ifelse(create_group_0, 0, 1), num_groups),
            Anc.or.Des = c("A", "D"),
            Fitness = as.character("")
        )
    ) %>%
        dplyr::arrange_("Group")

    googlesheets::gs_new(title = section_title, input = section_data, trim = TRUE)
}

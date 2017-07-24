#' Create a Google Sheet for a BIO 180 section
#'
#' @inheritParams build_section_spreadsheet_title
#' @param num_groups The number of groups in the section
#' @param create_group_0 Whether or not to create a group 0 for controls (default: TRUE)
#' @param trim Should the resulting worksheet only include the necessary cells? (default: TRUE)
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_new}}
#'
#' @return A \code{\link[googlesheets]{googlesheet}} object
#' @export
#'
#' @examples
#' \dontrun{
#' s4 <- create_section_spreadsheet_180(year = 2017,
#'                                      quarter = "WI",
#'                                      section = "C",
#'                                      num_groups = 5,
#'                                      create_group_0 = TRUE,
#'                                      trim = TRUE,
#'                                      ...)
#' }
create_section_spreadsheet_180 <- function(year,
                                           quarter,
                                           section,
                                           num_groups,
                                           create_group_0 = TRUE,
                                           trim = TRUE,
                                           ...) {
    assertthat::assert_that(assertthat::is.count(year))
    assertthat::assert_that(year > 2016)
    assertthat::assert_that(is_quarter(quarter))
    assertthat::assert_that(is_section(section))
    assertthat::assert_that(assertthat::is.count(num_groups))
    assertthat::assert_that(assertthat::is.flag(create_group_0))
    assertthat::assert_that(assertthat::is.flag(trim))

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
            Section = toupper(section),
            Group = seq(ifelse(create_group_0, 0, 1), num_groups),
            Anc.or.Des = c("A", "D"),
            Drug.at.Isolation = "",
            Fitness = "",
            Drug = "",
            MIC = ""
        )
    ) %>%
        dplyr::arrange_("Group")

    googlesheets::gs_new(
        title = section_title,
        input = section_data,
        trim = trim,
        ...
    )

    # TODO: add course/quarter/section info to returned object
}

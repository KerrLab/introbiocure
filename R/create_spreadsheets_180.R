#' Create all Section Spreadsheets for BIO 180 for a Given Quarter
#'
#' @inheritParams build_section_spreadsheet_title
#' @param master URL of the master spreadsheet
#' @param num_sections Total number of sections
#' @param num_groups Number of groups for each section (default: 6)
#' @param create_group_0 Whether or not to create a group 0 for controls (default: TRUE)
#' @param drugs_iso List of drugs used when creating progenitors (default: \code{c("None", "Rifampicin", "Streptomycin")})
#' @param trim Should the resulting worksheet only include the necessary cells? (default: TRUE)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_spreadsheets_180("Intro Bio Cure Master", year = 2018, quarter = "AU", num_sections = 3)
#' }
create_spreadsheets_180 <- function(master, year, quarter, num_sections,
                                    num_groups = 6, create_group_0 = TRUE,
                                    drugs_iso = c("None", "Rifampicin", "Streptomycin"),
                                    trim = TRUE) {
    assertthat::assert_that(
        assertthat::is.string(master),
        assertthat::is.count(year),
        is_quarter(quarter),
        assertthat::is.count(num_sections),
        num_sections < 53,
        assertthat::is.count(num_groups)
    )

    strains_per_section <- (num_groups + ifelse(create_group_0, 1, 0)) * length(drugs_iso) * 2

    strainid_nums <- seq(
        from = get_next_strainid(master = master, numeric = TRUE),
        length.out = num_sections * strains_per_section
    )

    for (section_number in 1:num_sections) {
        create_section_spreadsheet_180(
            year = year,
            quarter = quarter,
            section = number_as_section(section_number),
            num_groups = num_groups,
            create_group_0 = TRUE,
            start_strainid = strainid_nums[1 + (strains_per_section * (section_number - 1))],
            drugs_iso = drugs_iso,
            trim = trim
        )
    }
}

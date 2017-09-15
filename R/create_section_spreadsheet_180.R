#' Create a Google Sheet for a BIO 180 section
#'
#' @inheritParams build_section_spreadsheet_title
#' @param num_groups The number of groups in the section
#' @param create_group_0 Whether or not to create a group 0 for controls (default: FALSE)
#' @param start_strainid First strain ID to use. Can be numeric (e.g., 1000) or a strain ID ("AB000"). If none is provided, the StrainID column will be left empty.
#' @param drugs_iso List of drugs used when creating progenitors (default: \code{c("None", "RIF", "STR")})
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
#'                                      create_group_0 = FALSE,
#'                                      start_strainid = 300,
#'                                      trim = TRUE,
#'                                      ...)
#' }
create_section_spreadsheet_180 <- function(year,
                                           quarter,
                                           section,
                                           num_groups,
                                           create_group_0 = FALSE,
                                           start_strainid = NULL,
                                           drugs_iso = c("None", "RIF", "STR"),
                                           trim = TRUE,
                                           ...) {
    assertthat::assert_that(assertthat::is.count(year))
    assertthat::assert_that(year >= as.numeric(format(Sys.time(), "%Y")))
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
            Pro.or.Des = c("Progenitor", "Descendant"),
            Drug.at.Isolation = drugs_iso
        )
    ) %>%
        dplyr::mutate(
            StrainID = "",
            Fitness = "",
            Drug1 = "",
            Drug1.MIC = "",
            Drug2 = "",
            Drug2.MIC = "",
            ProblemIdentified = "No"
        ) %>%
        dplyr::select(Year, Quarter, Section, Group, StrainID, Pro.or.Des,
                      Drug.at.Isolation, Fitness, Drug1, Drug1.MIC, Drug2,
                      Drug2.MIC, ProblemIdentified) %>%
        dplyr::arrange_("Group")

    if (!is.null(start_strainid)) {
        assertthat::assert_that(assertthat::is.scalar(start_strainid))

        if (is.numeric(start_strainid)) {
            section_data$StrainID <- number_as_strainid(
                seq(
                    from = start_strainid,
                    length.out = nrow(section_data)
                )
            )
        } else if (is_strain_id(start_strainid)) {
            section_data$StrainID <- number_as_strainid(
                seq(
                    from = strainid_as_number(start_strainid),
                    length.out = nrow(section_data)
                )
            )
        } else {
            stop("Invalid start strain ID")
        }
    }

    s <- googlesheets::gs_new(
        title = section_title,
        input = section_data,
        trim = trim,
        ...
    )

    s$course <- 180
    s$year <- year
    s$quarter <- toupper(quarter)
    s$section <- toupper(section)

    message("** Remember to manually add data validation checks to your spreadsheet")

    s
}

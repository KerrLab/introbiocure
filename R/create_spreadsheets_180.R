#' Create all Section Spreadsheets for BIO 180 for a Given Quarter
#'
#' @inheritParams build_section_spreadsheet_title
#' @param master URL of the master spreadsheet
#' @param num_sections Total number of sections
#' @param num_groups Number of groups for each section (default: 6)
#' @param create_group_0 Whether or not to create a group 0 for controls (default: FALSE)
#' @param drugs_iso List of drugs used when creating progenitors (default: \code{c("None", "RIF", "STR")})
#' @param trim Should the resulting worksheet only include the necessary cells? (default: TRUE)
#'
#' @return A list of \code{\link[googlesheets]{googlesheet}} objects, one per section
#' @export
#'
#' @examples
#' \dontrun{
#' my_180_sheets <- create_spreadsheets_180(
#'     master = "https://docs.google.com/spreadsheets/d/1aOG8u-r1pObn2_OiiUx530eo2iACrU4nbvQ_9gmdcUw/",
#'     year = 2018,
#'     quarter = "AU",
#'     num_sections = 3
#' )
#' }
create_spreadsheets_180 <- function(master, year, quarter, num_sections,
                                     num_groups = 6, create_group_0 = FALSE,
                                     drugs_iso = c("None", "RIF", "STR"),
                                     trim = TRUE) {
    assertthat::assert_that(
        assertthat::is.string(master),
        assertthat::is.count(year),
        year >= as.numeric(format(Sys.time(), "%Y")),
        is_quarter(quarter),
        assertthat::is.count(num_sections),
        num_sections < 53,
        assertthat::is.count(num_groups)
    )

    start_strainid <- get_next_strainid(master = master, numeric = TRUE)

    # Create a data frame with data for all sections
    dAll <- expand.grid(
        Year = year,
        Quarter = quarter,
        Section = number_as_section(1:num_sections),
        Group = seq(from = ifelse(create_group_0, 0, 1), to = num_groups),
        Pro.or.Des = c("Progenitor", "Descendant"),
        Drug.at.Isolation = drugs_iso,
        Fitness = "",
        Drug1 = "",
        Drug1.MIC = "",
        Drug2 = "",
        Drug2.MIC = "",
        ProblemIdentified = "No",
        stringsAsFactors = FALSE
    ) %>%
        tibble::as.tibble() %>%
        dplyr::arrange(Year, Quarter, Section, Group) %>%
        dplyr::mutate(
            StrainID = number_as_strainid(seq(from = start_strainid, length.out = n()))
        ) %>%
        dplyr::select(Year, Quarter, Section, Group, StrainID, Pro.or.Des,
                      Drug.at.Isolation, Fitness, Drug1, Drug1.MIC, Drug2,
                      Drug2.MIC, ProblemIdentified)

    # Split the data up into sections, and create a google sheet for each section
    sheets <- dAll %>%
        split(.$Section) %>%
        purrr::map(function(x) {
            this_section <- unique(x$Section)[[1]]

            sheet_title <- build_section_spreadsheet_title(
                course = 180,
                year = year,
                quarter = quarter,
                section = this_section
            )

            s <- googlesheets::gs_new(
                title = sheet_title,
                input = x,
                trim = trim
                # Note: ... not passed on.
            )
            s
        })

    message("** Remember to manually add protected regions and data validation checks to your spreadsheets")
    sheets
}


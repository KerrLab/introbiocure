#' Create a Google Sheet for a BIO 200 section
#'
#' @inheritParams build_section_spreadsheet_title
#' @param master URL of the master spreadsheet
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
#' s4 <- create_section_spreadsheet_200(year = 2017,
#'                                      quarter = "WI",
#'                                      section = "C",
#'                                      num_groups = 5,
#'                                      create_group_0 = TRUE,
#'                                      trim = TRUE,
#'                                      ...)
#' }
create_section_spreadsheet_200 <- function(master,
                                           year,
                                           quarter,
                                           section,
                                           num_groups,
                                           create_group_0 = TRUE,
                                           trim = TRUE,
                                           ...) {
    # WHAT ABOUT SEQUENCE?
    # Year, Quarter, Section, Group, StrainID, Pro.or.Des, Drug.at.Isolation, RIF.MIC, Base.Mutations, AA.Mutations, SequenceProblemIdentified

    assertthat::assert_that(
        assertthat::is.count(year),
        year >= as.numeric(format(Sys.time(), "%Y")),
        is_quarter(quarter),
        is_section(section),
        assertthat::is.count(num_groups),
        assertthat::is.flag(create_group_0),
        assertthat::is.flag(trim)
    )

    section_title <- build_section_spreadsheet_title(
        course = 200,
        year = year,
        quarter = quarter,
        section = section
    )

    master_data <- get_master_data(master = master) %>%
        dplyr::filter(
            ProblemIdentified == "No",
            Drug.at.Isolation %in% c("None", "Rifampicin")
        ) %>%
        dplyr::mutate(
            RIF.MIC = dplyr::case_when(
                Drug1 == "Rifampicin" ~ Drug1.MIC,
                Drug2 == "Rifampicin" ~ Drug2.MIC,
                TRUE ~ NA_real_
            )
        )

    # TODO.

    section_data <- tibble::tibble(
        Year = year,
        Quarter = toupper(quarter),
        Section = toupper(section),
        Group = seq(ifelse(create_group_0, 0, 1), num_groups),
        StrainID = "TODO",
        Pro.or.Des = "TODO",
        Drug.at.Isolation = "TODO",
        Fitness = "TODO",
        RIF.MIC = "TODO",
        Sequence = "",
        Base.Mutations = "",
        AA.Mutations = "",
        SequenceProblemIdentified = "No"
    )

    # TODO: finish creating section_data

    s <- googlesheets::gs_new(
        title = section_title,
        input = section_data,
        trim = trim,
        ...
    )

    s$course <- 200
    s$year <- year
    s$quarter <- toupper(quarter)
    s$section <- toupper(section)

    message("Remember to manually add data validation checks to your spreadsheet")

    s
}

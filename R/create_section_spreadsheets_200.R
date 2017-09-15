#' Create all Section Spreadsheets for BIO 180 for a Given Quarter
#'
#'
#' @inheritParams build_section_spreadsheet_title
#' @param master URL of the master spreadsheet
#' @param num_sections Total number of sections
#' @param num_groups Number of groups for each section (default: 6)
#' @param create_group_0 Whether or not to create a group 0 for controls (default: FALSE)
#' @param trim Should the resulting worksheet only include the necessary cells? (default: TRUE)
#'
#' @return A list of \code{\link[googlesheets]{googlesheet}} objects, one per section
#' @export
#'
#' @examples
#' \dontrun{
#' create_spreadsheets_200(
#'     master = "https://docs.google.com/spreadsheets/d/1aOG8u-r1pObn2_OiiUx530eo2iACrU4nbvQ_9gmdcUw/",
#'     year = 2018,
#'     quarter = "AU",
#'     num_sections = 12
#' )
#' }
create_spreadsheets_200 <- function(master, year, quarter, num_sections,
                                    num_groups = 6, create_group_0 = TRUE,
                                    trim = TRUE) {

    assertthat::assert_that(
        assertthat::is.string(master),
        assertthat::is.count(year),
        is_quarter(quarter),
        assertthat::is.count(num_sections),
        num_sections < 53,
        assertthat::is.count(num_groups)
    )

    # How many total groups do we need strains for?
    total_num_groups <- num_sections * num_groups

    dCleanUnsequenced <- get_master_data(master, problems_as_logical = FALSE) %>%
        dplyr::mutate(
            RIF.MIC = dplyr::case_when(
                Drug1 == "RIF" ~ Drug1.MIC,
                Drug2 == "RIF" ~ Drug2.MIC,
                TRUE ~ NA_real_
            )
        ) %>%
        dplyr::filter(
            ProblemIdentified == "No",
            SequenceProblemIdentified == "No",
            !is.na(RIF.MIC),
            is.na(Base.Mutations),
            is.na(AA.Mutations)
        ) %>%
        dplyr::arrange(Year, Quarter, Section, Group, StrainID)

    # Put together the control strains for each group (RIF sensitive)
    # These will be assigned to group 0
    dControl <- tibble::tibble()

    if (create_group_0) {
        valid_controls <- dCleanUnsequenced %>%
            dplyr::filter(
                Pro.or.Des == "Progenitor",
                Drug.at.Isolation == "None"
            ) %>%
            dplyr::ungroup()

        num_controls_needed <- 2 * num_sections

        if (nrow(valid_controls) < num_controls_needed) {
            stop(sprintf("Number of valid control strains is insufficient: %d needed, %d available", num_controls_needed, nrow(valid_controls)))
        }

        dControl <- head(valid_controls, n = num_controls_needed) %>%
            dplyr::mutate(
                Section200 = number_as_section(rep(seq(1, num_sections), each = 2)),
                Group200 = 0
            )
    }

    valid_rif_strains <- dCleanUnsequenced %>%
        dplyr::filter(Drug.at.Isolation == "RIF") %>%
        dplyr::group_by(Year, Quarter, Section, Group, Drug.at.Isolation) %>%
        dplyr::filter(n() == 2L) %>%
        dplyr::ungroup()

    # Now put together the RIF resistant strains for each group
    num_strains_needed <- 2 * num_sections * num_groups

    if (nrow(valid_rif_strains) < num_strains_needed) {
        stop(sprintf("Number of valid RIF strains is insufficient: %d needed, %d available", num_strains_needed, nrow(valid_rif_strains)))
    }


    dStrains <- head(valid_rif_strains, n = num_strains_needed) %>%
        dplyr::mutate(
            Section200 = number_as_section(rep(seq(1, num_sections), each = 2 * num_groups)),
            Group200 = rep(rep(seq(1, num_groups), each = 2), times = num_sections)
        )

    dAll <- dplyr::bind_rows(dControl, dStrains) %>%
        dplyr::arrange(Section200, Group200, StrainID) %>%
        dplyr::select(Section = Section200, Group = Group200, StrainID, Pro.or.Des, Drug.at.Isolation, Fitness, RIF.MIC, Base.Mutations, AA.Mutations, SequenceProblemIdentified)

    sheets <- dAll %>%
        dplyr::mutate(
            Base.Mutations = "",
            AA.Mutations = ""
        ) %>%
        split(.$Section) %>%
        purrr::map(function(x) {
            this_section <- unique(x$Section)[[1]]

            sheet_title <- build_section_spreadsheet_title(
                course = 200,
                year = year,
                quarter = quarter,
                section = this_section
            )

            s <- googlesheets::gs_new(
                title = sheet_title,
                input = dplyr::select_(x, "-Section"),
                trim = trim
                # Note: ... not passed on.
            )
            s
        })


    message("* Remember to manually add data validation checks to your spreadsheets")
    sheets
}

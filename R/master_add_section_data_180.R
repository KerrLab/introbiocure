#' Insert Section Data from BIO 180 into Master Sheet
#'
#' @param master URL of the master spreadsheet
#' @param d Data frame containing section data (see \code{\link{get_section_data_180}})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_section_data_180(<section url>)
#' master_add_section_data_180(<master sheet url>, d)
#' }
master_add_section_data_180 <- function(master, d) {
    assertthat::assert_that(
        assertthat::is.string(master),
        is.data.frame(d),
        nrow(d) > 0
    )

    mData <- get_master_data(master = master)

    # Test for overlap.
    # We don't want to add new rows for data that already exist. Here, we're
    # just looking for records with the same StrainID. We could also look at
    # several columns, but a StrainID should only be used once in 180.

    overlap <- dplyr::semi_join(d, mData, by = c("StrainID"))
    if (nrow(overlap)) {
        stop(
            paste(
                "Not adding data. The following Strain ID(s) already exist in master spreadsheet",
                paste0(overlap$StrainID, collapse = ", "),
                sep = ": "
            )
        )
    }

    dCleaned <- d %>%
        tibble::add_column(
            Base.Mutations = "",
            AA.Mutations = "",
            SequenceProblemIdentified = "No"
        ) %>%
        dplyr::select(Year, Quarter, Section, Group, StrainID, Pro.or.Des, Drug.at.Isolation, Fitness, Drug1, Drug1.MIC, Drug2, Drug2.MIC, Base.Mutations, AA.Mutations, ProblemIdentified, SequenceProblemIdentified)

    if (is.logical(dCleaned$ProblemIdentified)) {
        dCleaned <- dCleaned %>%
            dplyr::mutate(
                ProblemIdentified = dplyr::case_when(
                    as.character(ProblemIdentified) == "FALSE" ~ "No",
                    as.character(ProblemIdentified) == "TRUE" ~ "Yes",
                    TRUE ~ ""
                )
            )
    }

    googlesheets::gs_add_row(
        ss = googlesheets::gs_url(master),
        input = dCleaned
    )
    dCleaned
}

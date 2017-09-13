#' Insert Section Data into Master Sheet
#'
#' @param title Title of the master spreadsheet (default: "Intro Bio Cure Master")
#' @param d Data frame (see \code{get_section_data})
#'
#' @return TODO
#' @export
#'
#' @examples
#' \dontrun{TODO}
master_add_section_data_180 <- function(title, d) {
    assertthat::assert_that(
        assertthat::is.string(title),
        is.data.frame(d),
        nrow(d) > 0,
        all(d$Course == 180)
    )

    mData <- get_master_data(title = title)

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

    googlesheets::gs_add_row(
        ss = googlesheets::gs_title(title),
        input = dplyr::select_(d, "Year", "Quarter", "Section", "Group", "StrainID", "Pro.or.Des", "Drug.at.Isolation", "Fitness", "Drug1", "Drug1.MIC", "Drug2", "Drug2.MIC")
    )
}

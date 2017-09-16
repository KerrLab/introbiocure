#' Insert Section Data from BIO 200 into Master Sheet
#'
#' @param master URL of the master spreadsheet
#' @param d Data frame containing section data (see \code{\link{get_section_data_180}})
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_section_data_200(<section url>)
#' master_add_section_data_200(<master sheet url>, d)
#' }
master_add_section_data_200 <- function(master, d) {
    assertthat::assert_that(
        assertthat::is.string(master),
        is.data.frame(d),
        nrow(d) > 0
    )

    dMaster <- get_master_data(master = master)

    overlap <- dplyr::semi_join(d, dMaster, by = c("StrainID"))

    if (nrow(overlap) < nrow(d)) {
        # There are StrainIDs specified in d that aren't in master.
        # TODO: stop(), printing out an error message about the odd StrainIDs
    }

    if (any(!is.na(dMaster$Base.Mutations)) || any(!is.na(dMaster$AA.Mutations))) {
        # There are strains that already have genetic information.
        # TODO: stop(), printing out an error message.
    }

    # TODO: join the data
    # TODO: update the master data sheet.
    # - How is this done? Delete the rows? Replace the rows?

}

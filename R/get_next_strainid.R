#' Title
#'
#' @param mastersheet_title Title of the master spreadsheet (default: "Intro Bio Cure Master")
#' @param numeric Whether or not the result should be numeric (default:
#' \code{FALSE}). If \code{FALSE}, a strain ID is returned.
#'
#' @return An integer specifying the number of the next available strain ID
#' @export
#'
#' @examples
#' next_strain <- get_next_strainid()
#' next_strain_id <- number_as_strainid(next_strain)
#'
get_next_strainid <- function(mastersheet_title = "Intro Bio Cure Master",
                              numeric = FALSE) {
    md <- get_master_data(title = mastersheet_title)

    nextid <- ifelse(
        nrow(md) > 0,
        max(strainid_as_number(md$StrainID)) + 1,
        1
    )

    ifelse(numeric, nextid, number_as_strainid(nextid))
}

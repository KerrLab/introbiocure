#' Title
#'
#' @param master URL of the master spreadsheet
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
get_next_strainid <- function(master, numeric = FALSE) {
    md <- get_master_data(master = master)

    nextid <- ifelse(
        nrow(md) > 0,
        max(strainid_as_number(md$StrainID)) + 1,
        1
    )

    ifelse(numeric, nextid, number_as_strainid(nextid))
}

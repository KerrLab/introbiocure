#' Retrieve Data from Master Spreadsheet
#'
#' \code{get_master_data} returns the data from the master spreadsheet as a data
#' frame ()
#'
#' @param master URL of the master spreadsheet
#' @param path File path or connection to write data to
#' @param problems_as_logical Whether or not to try to convert ProblemIdentified
#' and SequenceProblemIdentified values to logical (default: \code{FALSE})
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_read}} (\code{get_master_data}) or \code{\link[readr]{write_csv}} (\code{save_master_data}).
#'
#' @return A data frame (tibble)
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
get_master_data <- function(master, problems_as_logical = FALSE, ...) {
    dMaster <- googlesheets::gs_read(
        ss = googlesheets::gs_url(master),
        col_types = col_types_master,
        ...
    )

    if (problems_as_logical) {
        dMaster <- dMaster %>%
            dplyr::mutate(
                ProblemIdentified = tolower(trimws(ProblemIdentified)) == "yes",
                SequenceProblemIdentified = tolower(trimws(SequenceProblemIdentified)) == "yes"
            )
    }

    dMaster

}


#' @rdname get_master_data
#' @description \code{save_master_data} saves data from the master spreadsheet
#' to a csv file
#' @export
save_master_data <- function(master, path, ...) {
    readr::write_csv(
        x = get_master_data(master = master),
        path = path,
        ...
    )
}

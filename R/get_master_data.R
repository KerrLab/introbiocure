#' Retrieve Data from Master Spreadsheet
#'
#' \code{get_master_data} returns the data from the master spreadsheet as a data
#' frame ()
#'
#' @param title Title of the master spreadsheet (default: "Intro Bio Cure Master")
#' @param path File path or connection to write data to
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_read}} (\code{get_master_data}) or \code{\link[readr]{write_csv}} (\code{save_master_data}).
#'
#' @return A data frame (tibble)
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
get_master_data <- function(title, ...) {
    d <- googlesheets::gs_read(
        ss = googlesheets::gs_title(title),
        col_types = col_types_master,
        ...
    )
    d
}


#' @rdname get_master_data
#' @description \code{save_master_data} saves data from the master spreadsheet
#' to a csv file
#' @export
save_master_data <- function(title, path, ...) {
    readr::write_csv(
        x = get_master_data(title = title),
        path = path,
        ...
    )
}

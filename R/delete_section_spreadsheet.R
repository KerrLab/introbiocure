#' Delete the Google Sheet for a BIO 180/200 section
#'
#' @inheritParams build_section_spreadsheet_title
#' @param ... Additional arguments (ignored)
#'
#' @return TODO
#' @export
#'
#' @examples
#' \dontrun{
#' delete_section_spreadsheet(course = 180, year = 2017, quarter = "AU", section = "C")
#' }
delete_section_spreadsheet <- function(course, year, quarter, section, ...) {
    section_title <- build_section_spreadsheet_title(
        course = course,
        year = year,
        quarter = quarter,
        section = section
    )

    # Could use gs_vecdel for this.
    sheet <- googlesheets::gs_title(section_title, verbose = FALSE)

    response <- readline(prompt = sprintf("Are you sure you want to delete '%s'? ('yes' to delete, anything else to cancel): ", section_title))

    if (tolower(response) == "yes") {
        googlesheets::gs_delete(sheet)
    }
    else {
        cat("Did not delete section spreadsheet\n")
    }

}

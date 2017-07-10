#' Title
#'
#' @param course The course number (e.g., 180 or 200)
#' @param year The year
#' @param quarter The quarter. One of: "AU", "WI", "SP", "SU".
#' @param section The section name (e.g., "G")
#' @param ... Additional arguments (ignored)
#'
#' @return
#' @export
#'
#' @examples
delete_section_spreadsheet <- function(course, year, quarter, section, ...) {
    section_title <- build_section_spreadsheet_title(
        course = course,
        year = year,
        quarter = quarter,
        section = section
    )

    sheet <- googlesheets::gs_title(section_title, verbose = FALSE)
    response <- readline(prompt = sprintf("Are you sure you want to delete '%s'? ('yes' to delete, anything else to cancel): ", section_title))

    # Could use gs_vecdel for this.

    if (tolower(response) == "yes") {
        googlesheets::gs_delete(sheet)
    }
    else {
        cat("Did not delete section spreadsheet\n")
    }

}

#' Combine and Retrieve Data From Multiple Sections
#' @description \code{combine_section_data_key} combines data from multiple
#' sheets specified by sheet key
#' @param ... List or vector specifying one or more sheets.
#' @param remove_duplicates Whether or not to remove sheets that are given more
#' than once (default: \code{TRUE})
#'
#' @seealso \code{\link{list_course_spreadsheets}}
#'
#' @return TODO
#' @rdname combine_section_data
#' @export
#'
#' @examples
#' \dontrun{
#' my_180_sections <- list_course_spreadsheets(course = 180)
#' all_180_data <- combine_section_data_key(my_180_sections$sheet_key)
#' }
combine_section_data_key <- function(..., remove_duplicates = TRUE) {
    x <- as.list(paste0("https://docs.google.com/spreadsheets/d/", list(...)))
    x$remove_duplicates = remove_duplicates
    do.call(combine_section_data_url, x)
}


#' @description \code{combine_section_data_url} combines data from multiple
#' sheets specified by URL
#' @rdname combine_section_data
#' @export
combine_section_data_url <- function(..., remove_duplicates = TRUE) {
    urls <- list(...)

    if (length(urls) < 1) {
        stop("Must supply at least one sheet", call. = FALSE)
    }

    if (remove_duplicates) {
        urls <- unique(urls)
    }

    purrr::map_df(
        urls,
        ~ googlesheets::gs_read(
            googlesheets::gs_url(.),
            col_types = col_types_both
        )
    )
}

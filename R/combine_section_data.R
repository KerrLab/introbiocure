#' Combine and Retrieve Data From Multiple Sections
#' @description \code{combine_section_data_key} combines data from multiple
#' sheets specified by sheet key
#' @param keys List of one or more sheet keys
#' @param urls List of one or more sheet URLs
#' @param remove_duplicates Whether or not to remove sheets that are given more
#' than once (default: \code{TRUE})
#'
#' @seealso \code{\link{list_course_spreadsheets}}
#'
#' @return A data frame containing data from the given sections
#' @rdname combine_section_data
#' @export
#'
#' @examples
#' \dontrun{
#' my_180_sections <- list_course_spreadsheets(course = 180)
#' all_180_data <- combine_section_data_key(my_180_sections$sheet_key)
#' }
combine_section_data_key <- function(keys, remove_duplicates = TRUE) {
    stopifnot(length(keys) >= 1)

    if (remove_duplicates) {
        keys <- unique(keys)
    }

    dAll <- purrr::map_df(
        keys,
        ~ googlesheets::gs_read(
            googlesheets::gs_key(.),
            col_types = col_types_both
        )
    ) %>%
        dplyr::mutate(
            Section = as.factor(Section),
            Pro.or.Des = as.factor(Pro.or.Des),
            Drug.at.Isolation = as.factor(Drug.at.Isolation)
        )

    dAll
}


#' @description \code{combine_section_data_url} combines data from multiple
#' sheets specified by URL
#' @rdname combine_section_data
#' @export
combine_section_data_url <- function(urls, remove_duplicates = TRUE) {
    stopifnot(length(urls) >= 1)

    combine_section_data_key(
        keys = purrr::map_chr(urls, googlesheets::extract_key_from_url),
        remove_duplicates = remove_duplicates
    )
}

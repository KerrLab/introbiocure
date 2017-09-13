#' Retrieve Data for a Given Section
#'
#' Both \code{get_section_data_180} and \code{get_section_data_180_key} retrieve
#' data for a given section (or sections). \code{get_section_data_180} is given
#' the URL(s) of section sheets, while \code{get_section_data_180_key} is given
#' the unique keys.
#'
#' @param url One or more sheet URLs
#' @param key One or more sheet keys
#' @param remove_duplicates Whether or not to remove sheets that are given more
#' than once (default: \code{TRUE})
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame
#' @export
#'
#' @rdname get_section_data
#' @examples
#' \dontrun{
#' # TODO
#' }
get_section_data_180 <- function(url, remove_duplicates = TRUE, ...) {
    stopifnot(length(url) >= 1)

    get_section_data_180_key(
        key = purrr::map_chr(url, googlesheets::extract_key_from_url),
        remove_duplicates = remove_duplicates
    )
}


#' @rdname get_section_data
#' @export
get_section_data_180_key <- function(key, remove_duplicates = TRUE, ...) {
    stopifnot(length(key) >= 1)

    if (remove_duplicates) {
        key <- unique(key)
    }

    dAll <- purrr::map_df(
        key,
        ~ googlesheets::gs_read(
            googlesheets::gs_key(.),
            col_types = col_types_180
        )
    ) %>%
        dplyr::mutate(
            Section = as.factor(Section),
            Pro.or.Des = as.factor(Pro.or.Des),
            Drug.at.Isolation = as.factor(Drug.at.Isolation)
        )

    dAll
}


#' @rdname get_section_data
#' @description \code{save_section_data} saves data for a given section (URL) to
#' a csv file
#' @export
save_section_data_180 <- function(path, url, remove_duplicates = TRUE, ...) {
    readr::write_csv(
        x = get_section_data_180(url),
        path = path,
        ...
    )
}

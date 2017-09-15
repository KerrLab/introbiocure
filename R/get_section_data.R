#' Retrieve Data for a Given Section
#'
#' The \code{get_section_data} functions retrieve data corresponding to one or
#' more section sheets and return a data frame.
#'
#' The \code{save_section_data} retrieve the data and save it to a CSV file.
#'
#' These functions can operate either on sheet URLs or sheet keys (which are
#' embedded in the sheet URL).
#'
#' @param url One or more sheet URLs
#' @param key One or more sheet keys
#' @param remove_duplicates Whether or not to remove sheets that are given more
#' than once (default: \code{TRUE})
#' @param include_section_info Since BIO 200 sheets don't include Year, Quarter,
#' and Section information in the sheet, those values can be added to the
#' resulting data if this value is \code{TRUE} (default: \code{TRUE})
#' @param problems_as_logical Whether or not to try to convert ProblemIdentified
#' values to logical (default: \code{FALSE})
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame
#' @export
#'
#' @rdname get_section_data
#' @examples
#' \dontrun{
#' get_section_data("https://docs.google.com/spreadsheets/d/1SffJckl1oOQqE8lAfgNjNH9NKbbIXVw31rKBNIaUlQ4/")
#' }
get_section_data_180 <- function(url, remove_duplicates = TRUE,
                                 problems_as_logical = FALSE, ...) {
    stopifnot(length(url) >= 1)

    get_section_data_180_key(
        key = purrr::map_chr(url, googlesheets::extract_key_from_url),
        remove_duplicates = remove_duplicates,
        problems_as_logical = problems_as_logical
    )
}


#' @rdname get_section_data
#' @export
get_section_data_180_key <- function(key, remove_duplicates = TRUE,
                                     problems_as_logical = FALSE, ...) {
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

    if (problems_as_logical) {
        dAll$ProblemIdentified <- tolower(trimws(dAll$ProblemIdentified)) == "yes"
    }

    dAll
}


#' @rdname get_section_data
#' @export
get_section_data_200 <- function(url, remove_duplicates = TRUE,
                                 include_section_info = TRUE,
                                 problems_as_logical = FALSE, ...) {
    stopifnot(length(url) >= 1)

    get_section_data_200_key(
        key = purrr::map_chr(url, googlesheets::extract_key_from_url),
        remove_duplicates = remove_duplicates,
        include_section_info = include_section_info,
        problems_as_logical = problems_as_logical
    )

}


#' @rdname get_section_data
#' @export
get_section_data_200_key <- function(key, remove_duplicates = TRUE,
                                     include_section_info = TRUE,
                                     problems_as_logical = FALSE, ...) {
    stopifnot(length(key) >= 1)

    if (remove_duplicates) {
        key <- unique(key)
    }

    dAll <- purrr::map_df(
        key,
        function(k) {
            sheet <- googlesheets::gs_key(k)
            title_info <- parse_sheet_title(sheet$sheet_title)[[1]]

            dK <- googlesheets::gs_read(sheet, col_types = col_types_200)

            if (include_section_info) {
                dK <- dK %>%
                    tibble::add_column(
                        Year = title_info$Year,
                        Quarter = factor(title_info$Quarter, levels = c("AU", "WI", "SP", "SU")),
                        Section = title_info$Section,
                        .before = "Group"
                    )
            }

            dK
        }
    ) %>%
        dplyr::mutate(
            Pro.or.Des = as.factor(Pro.or.Des),
            Drug.at.Isolation = as.factor(Drug.at.Isolation)
    )

    if (problems_as_logical) {
        dAll$SequenceProblemIdentified <- tolower(trimws(dAll$SequenceProblemIdentified)) == "yes"
    }

    dAll
}


#' @rdname get_section_data
#' @export
save_section_data_180 <- function(path, url, remove_duplicates = TRUE,
                                  problems_as_logical = FALSE, ...) {
    readr::write_csv(
        x = get_section_data_180(
            url = url,
            remove_duplicates = remove_duplicates,
            problems_as_logical = problems_as_logical
        ),
        path = path,
        ...
    )
}


#' @rdname get_section_data
#' @export
save_section_data_200 <- function(path, url, remove_duplicates = TRUE,
                                  include_section_info = TRUE,
                                  problems_as_logical = FALSE, ...) {
    readr::write_csv(
        x = get_section_data_200(
            url = url,
            remove_duplicates = remove_duplicates,
            include_section_info = include_section_info,
            problems_as_logical = problems_as_logical
        ),
        path = path,
        ...
    )
}

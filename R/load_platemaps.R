#' Load One or More Plate Maps
#'
#' Plate map files should be CSVs that contain at least the following columns: Well, Pro.or.Des, Drug.at.Isolation, and Concentration.
#'
#' @param ... One or more pairs of drug abbreviation and file name. See example.
#' @param include_drug Whether or not to include the drug name as a column (default: \code{TRUE}).
#'
#' @return A list of data frames, one for each drug.
#' @export
#'
#' @examples
#' \dontrun{
#' maps <- load_platemaps(STR = "my_strep_pmap.csv", RIF = "my_rif_pmap.csv")
#' }
load_platemaps <- function(..., include_drug = TRUE) {
    map_files <- list(...)
    bad_drug_names <- names(map_files[nchar(names(map_files)) != 3])

    if (length(map_files) == 0) {
        stop("Must provide at least one plate map")
    }
    else if (any(names(map_files) == "")) {
        stop("All plate maps must have a name")
    }
    else if (length(bad_drug_names) > 0) {
        stop(
            paste(
                "Invalid drug name(s) ",
                paste0(bad_drug_names, collapse = ", "),
                sep = ":"
            )
        )
    }

    maps <- names(map_files) %>%
        purrr::map_df(function(x) {
            readr::read_csv(
                file = map_files[[x]],
                col_types = readr::cols(
                    Well = readr::col_character(),
                    Pro.or.Des = readr::col_character(),
                    Drug.at.Isolation = readr::col_character(),
                    Concentration = readr::col_double()
                )
            ) %>%
                dplyr::mutate(Drug = toupper(x)) %>%
                dplyr::select(Drug, Well, Pro.or.Des, Drug.at.Isolation, Concentration)
        })

    maps
}

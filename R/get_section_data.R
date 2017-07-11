#' Retrieve Section Data
#'
#' \code{get_section_data} returns the data for a given section as a data frame
#'
#' @inheritParams build_section_spreadsheet_title
#' @param path File path or connection to write data to
#' @param include_course Whether or not to include the course number as a column (default: TRUE)
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_read}} (\code{get_section_data}) or \code{\link[readr]{write_csv}} (\code{save_section_data}).
#'
#' @return TODO
#' @export
#'
#' @examples
#' \dontrun{
#' d <- get_section_data(course = 200, year = 2017, quarter = "WI", section = "R")
#' save_section_data(path = "bio200_r.csv", course = 200, year = 2017, quarter = "WI", section = "R")
#' }
get_section_data <- function(course,
                             year,
                             quarter,
                             section,
                             include_course = TRUE,
                             ...) {

    section_title <- build_section_spreadsheet_title(
        course = course,
        year = year,
        quarter = quarter,
        section = section
    )

    if (course == 180) {
        d <- googlesheets::gs_read(
            ss = googlesheets::gs_title(section_title),
            col_types = readr::cols(
                Year = readr::col_integer(),
                Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
                Section = readr::col_character(),
                Group = readr::col_integer(),
                Anc.or.Des = readr::col_factor(levels = c("A", "D")),
                Fitness = readr::col_number()
            )
        )
    }
    else if (course == 200) {
        d <- googlesheets::gs_read(
            ss = googlesheets::gs_title(section_title),
            col_types = readr::cols(
                Year = readr::col_integer(),
                Quarter = readr::col_factor(levels = c("AU", "WI", "SP", "SU")),
                Section = readr::col_character(),
                Group = readr::col_integer(),
                Anc.or.Des = readr::col_factor(levels = c("A", "D")),
                Cluster = readr::col_factor(levels = c("I", "II")),
                Direction = readr::col_factor(levels = c("F", "R"))
                #Sequence
                #Base.Mutations
                #AA.Mutations
            )
        )
    }

    if (include_course) {
        d <- dplyr::mutate(d, Course = course)
    }

    d
}

#' @rdname get_section_data
#' @description \code{save_section_data} saves data for a given section to a csv file
#' @export
save_section_data <- function(path,
                              course,
                              year,
                              quarter,
                              section,
                              include_course = TRUE,
                              ...) {
    readr::write_csv(
        x = get_section_data(course, year, quarter, section, include_course),
        path = path,
        ...
    )
}

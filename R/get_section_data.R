#' TODO
#'
#' @inheritParams build_section_spreadsheet_title
#' @param include_course Whether or not to include the course number as a column (default: TRUE)
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_read}}
#'
#' @return TODO
#' @export
#'
#' @examples
#' #TODO
get_section_data <- function(course, year, quarter, section, include_course = TRUE, ...) {

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
                Year = readr::col_integer()   ,
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
                Year = readr::col_integer()   ,
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

#' Create a Master Spreadsheet (180 + 200)
#'
#' @param title Title for the spreadsheet (default: "Intro Bio Cure Master")
#' @param trim Should the resulting worksheet only include the necessary cells? (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_new}}
#'
#' @return A \code{\link[googlesheets]{googlesheet}} object
#' @export
#'
#' @examples
#' \dontrun{
#' ms <- create_master_spreadsheet()
#' }
create_master_spreadsheet <- function(title = "Intro Bio Cure Master",
                                      trim = FALSE,
                                      ...) {
    assertthat::assert_that(assertthat::is.string(title))

    # This is nice and clean, but googlesheets requires at least one row
    # master_data <- tibble::tibble(
    #     Year = numeric(),
    #     Quarter = character(),
    #     Section = character(),
    #     Group = numeric(),
    #     StrainID = character(),
    #     Pro.or.Des = factor(c("Progenitor", "Descendant")),
    #     Drug.at.Isolation = character(),
    #     Fitness = numeric(),
    #     Drug1 = character(),
    #     Drug1.MIC = numeric(),
    #     Drug2 = character(),
    #     Drug2.MIC = numeric(),
    #     Base.Mutations = character(),
    #     AA.Mutations = character()
    # )

    master_data <- tibble::tibble(
        Year = "",
        Quarter = "",
        Section = "",
        Group = "",
        StrainID = "",
        Pro.or.Des = "",
        Drug.at.Isolation = "",
        Fitness = "",
        Drug1 = "",
        Drug1.MIC = "",
        Drug2 = "",
        Drug2.MIC = "",
        Base.Mutations = "",
        AA.Mutations = ""
    )

    master_sheet <- googlesheets::gs_new(
        title = title,
        input = master_data,
        trim = trim,
        ...
    )
    master_sheet
}

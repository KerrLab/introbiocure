#' Create a Master Spreadsheet (180 + 200)
#'
#' @param title Title for the spreadsheet
#' @param data Optional, A data frame containing data to be used (default: NULL)
#' @param trim Should the resulting worksheet only include the necessary cells? (default: FALSE)
#' @param ... Additional arguments passed to \code{\link[googlesheets]{gs_new}}
#'
#' @note An invalid first row is also created, because this is required by googlesheets.
#'
#' @return A \code{\link[googlesheets]{googlesheet}} object
#' @export
#'
#' @examples
#' \dontrun{
#' ms <- create_master_spreadsheet()
#' }
create_master_spreadsheet <- function(title, data = NULL, trim = FALSE, ...) {
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
    #     AA.Mutations = character(),
    #     ProblemIdentified = character(),
    #     SequenceProblemIdentified = character()
    # )

    if (is.null(data)) {
        # For technical reasons, we must have at least one row.
        data <- tibble::tibble(
            Year = 0,
            Quarter = "",
            Section = "ZZ",
            Group = "",
            StrainID = "AA000",
            Pro.or.Des = "",
            Drug.at.Isolation = "",
            Fitness = "",
            Drug1 = "",
            Drug1.MIC = "",
            Drug2 = "",
            Drug2.MIC = "",
            Base.Mutations = "{}",
            AA.Mutations = "{}",
            ProblemIdentified = "Yes",
            SequenceProblemIdentified = "Yes"
        )
    } else {
        if (nrow(data) < 1) {
            stop("Initial data must have at least one row")
        }
    }

    s <- googlesheets::gs_new(
        title = title,
        input = data,
        trim = trim,
        ...
    )

    message("** Remember to manually add data validation checks to your spreadsheet")
    s
}

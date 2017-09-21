#' Annotate a Data Set by Combining it with a Plate Map
#'
#' Both \code{d} and \code{platemap} should be data frames that contain columns
#' \code{Drug} (the 3-letter antibiotic name) and \code{Well} (e.g., "A4").
#'
#' @param d A data frame representing an un-annotated data set
#' @param platemap A data frame containing a plate map
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame (annotated)
#' @export
#'
#' @examples
#' \dontrun{
#' annotate_data <- function(my_raw_data, my_platemap)
#' }
annotate_data <- function(d, platemap, ...) {
    assertthat::assert_that(
        is.data.frame(d),
        all(assertthat::has_name(d, c("Drug", "Well"))),
        nrow(d) > 0,
        is.data.frame(platemap),
        all(assertthat::has_name(platemap, c("Drug", "Well"))),
        nrow(platemap) > 0
    )

    dplyr::inner_join(d, platemap, by = c("Drug", "Well"))
}

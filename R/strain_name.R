#' Create a Strain Name Based on Resistance Status and Time
#'
#' \code{strain_name} creates a strain name based on \code{Pro.or.Des} and
#' \code{Drug.at.Isolation} values. For example, (\code{"Progenitor"},
#' \code{"RIF"}) becomes \code{"RIF.R"}, while (\code{"Descendant"},
#' \code{"None"}) becomes \code{"SEN.D"}
#'
#' @param Pro.or.Des Character, either \code{"Progenitor"} or \code{"Descendant"}
#' @param Drug.at.Isolation  Character, 3-character drug name (e.g., \code{"RIF"})
#'
#' @return A string
#' @export
#'
#' @examples
#' strain_name("Progenitor", "RIF")
strain_name <- function(Pro.or.Des, Drug.at.Isolation) {
    paste0(
        ifelse(Drug.at.Isolation == "None", "SEN", sprintf("%s.R", Drug.at.Isolation)),
        ifelse(Pro.or.Des == "Descendant", ".D", "")
    )
}

#' Read Data from XML Files Exported by Spectrophotometer
#'
#' @param file The name of the file to read
#' @param ... Additional arguments (not currently used)
#'
#' @return A data frame containing all data from the reads. Column
#' \code{Absorbance.Adj} will be the absorbance data from a culture minus the
#' absorbance of that same well in the control plate (of that drug). If no
#' control plate exists for that drug, then \code{NA} is given.
#' @export
#'
#' @examples
#' \dontrun{
#' dSpec <- read_spec_data(file = "myfile.xml")
#' }
read_spec_data <- function(file, ...) {
    dSpec <- softermax::read_softmax6_xml(file = file) %>%
        tibble::as.tibble(platesAsFactors = FALSE, wellsAsFactors = FALSE) %>%
        dplyr::mutate(
            Row = as.integer(well_row(Well)),
            Column = well_column(Well)
        ) %>%
        dplyr::select(Plate, Well, Row, Column, Absorbance = Value)

    # Get the data from control plates (if any)
    dControl <- dSpec %>%
        dplyr::filter(substring(tolower(Plate), 1, 7) == "control") %>%
        dplyr::mutate(
            Drug = toupper(substring(Plate, 9, 11))
        ) %>%
        dplyr::select(Drug, Well, Row, Column, Absorbance)
    controls <- split(dControl, dControl$Drug)

    # Get the data from Group plates
    dGroups <- dSpec %>%
        dplyr::filter(substring(tolower(Plate), 1, 1) == "s")

    plate_info <- extract_plate_info(unique(dGroups$Plate))

    # Merge in Section, Group, and Drug information
    dGroups <- dGroups %>%
        dplyr::left_join(plate_info, by = "Plate") %>%
        dplyr::select(Drug, Section, Group, Well, Row, Column, Absorbance) %>%
        dplyr::arrange(Drug, Section, Group, Row, Column)

    # Combine the Group data and the Control data
    dAll <- dGroups %>%
        split(.$Drug) %>%
        purrr::map_df(function(x) {
            drug <- unique(x$Drug)

            if (drug %in% names(controls)) {
                x %>%
                    dplyr::inner_join(
                        controls[[drug]],
                        by = c("Drug", "Well", "Row", "Column"),
                        suffixes = c(".group", ".control")
                    ) %>%
                    dplyr::mutate(
                        Absorbance.Adj = Absorbance.x - Absorbance.y
                    ) %>%
                    dplyr::select(Drug, Section, Group, Well, Row, Column, Absorbance = Absorbance.x, Absorbance.Adj)
            } else {
                dplyr::mutate(x, Absorbance.Adj = NA)
            }

        }) %>%
        #dplyr::bind_rows() %>%
        dplyr::mutate(
            Drug = as.factor(Drug),
            Section = as.factor(Section),
            Well = as.factor(Well)
        )

    list(Groups = dAll, Controls = dControl)
}

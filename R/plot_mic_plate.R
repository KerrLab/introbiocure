#' Make a MIC Plate Diagram
#'
#' \code{plot_mic_plate} makes a MIC Plate Diagram using the data supplied. This
#' data frame must contain the columns described below.
#'
#' @details The following columns must be present in the data:
#'
#' \describe{
#'     \item{Drug}{3-letter code indicating the drug used for the MIC assay (e.g., 'RIF' or 'STR'). Must have one unique value.}
#'     \item{Well}{Well in the microtiter plate (e.g., 'D6')}
#'     \item{Pro.or.Des}{Strain information. Either 'Progenitor' or 'Descendant'}
#'     \item{Drug.at.Isolation}{Drug used when strain was isolated, either 'None' or 3-letter code (e.g., 'RIF')}
#'     \item{Absorbance}{Absorbance value for the well}
#'     \item{Concentration}{Concentration of antibiotic present in the well}
#' }
#'
#' Additional columns that can be used:
#'
#' \describe{
#'     \item{Section}{BIO 180 Section name (e.g., 'A'). Must have one unique value.}
#'     \item{Group}{BIO 180 Group number. Must have one unique value.}
#'     \item{Strain}{Strain name (e.g., 'RIF.R.D'). If not present, this will be created using \code{Pro.or.Des} and \code{Drug.at.Isolation}}
#' }
#'
#' @param d A data frame containing absorbance and antibiotic data
#' @param title A title for the plot. If not provided, one will be created using the \code{Section} and \code{Group}.
#' @param subtitle A subtitle for the plot. If not provided, one will be created using the \code{Drug}.
#' @param threshold Value above which values can be trusted. If a value is below the threshold, that well will be highlighted in red (default: \code{0}).
#' @param absorbance.max Value to be set as the maximum possible absorbance. Useful for having consistent greyscale across many plots (default: \code{3}, which is approx. where the spex maxes out).
#' @param show.values Whether or not to display the absorbance values for each well (default: \code{FALSE})
#' @param show.unused Whether or not to display unused wells. (default: \code{FALSE})
#' @param ... Additional arguments (not currently used)
#'
#' @return A \code{\link{ggplot2}} plot object
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' plot_mic_plate(my_mic_data)
#' }
plot_mic_plate <- function(d, title = NULL, subtitle = NULL, threshold = 0, absorbance.max = 3, show.values = FALSE, show.unused = FALSE, ...) {

    assertthat::assert_that(
        is.data.frame(d),
        nrow(d) > 0,
        all(assertthat::has_name(d, c("Drug", "Absorbance", "Well", "Concentration", "Pro.or.Des", "Drug.at.Isolation"))), # TODO: more
        length(unique(d$Drug)) == 1,
        threshold >= 0
    )

    if ("Section" %in% names(d)) {
        if (length(unique(d$Section)) > 1) {
            stop("'Section' column should contain only one value.")
        }
    }

    if ("Group" %in% names(d)) {
        if (length(unique(d$Group)) > 1) {
            stop("'Group' column should contain only one value.")
        }
    }

    if (!"Strain" %in% names(d)) {
        message("'Strain' column not present. Creating Strain names from Pro.or.Des and Drug.at.Isolation.")
        d <- dplyr::mutate(d, Strain = strain_name(Pro.or.Des, Drug.at.Isolation))
    }

    if (!"Row" %in% names(d)) {
        message("'Row' column not present. Creating row numbers from Well.")
        d <- dplyr::mutate(d, Row = well_row(Well))
    }

    if (!"Column" %in% names(d)) {
        message("'Column' column not present. Creating column numbers from Well.")
        d <- dplyr::mutate(d, Column = well_column(Well))
    }

    if ("Section" %in% names(d)) {
        section = unique(d$Section)[[1]]
    }

    if ("Group" %in% names(d)) {
        group = as.integer(unique(d$Group)[[1]])
    } else {
        group = -99
    }

    if (is.null(title)) {
        title = sprintf("Section %s, Group %d", section, group)
    }

    if (is.null(subtitle)) {
        subtitle = sprintf("%s", unique(d$Drug)[[1]])
    }


    concentration_labels <- rep("", 8)
    concentrations <- d %>%
        dplyr::group_by(Row) %>%
        dplyr::summarize(Concentration = unique(Concentration)[1]) %>%
        dplyr::arrange(Row)
    concentration_labels[concentrations$Row] <- concentrations$Concentration

    strain_labels <- rep("", 12)
    strains <- d %>%
        dplyr::group_by(Column) %>%
        dplyr::summarize(
            Strain = unique(Strain)[1]
        ) %>%
        dplyr::arrange(Column)
    strain_labels[strains$Column] <- strains$Strain

    # Zero out any negative values
    d$Absorbance <- pmax(0, d$Absorbance)

    d$AboveThreshold <- d$Absorbance >= threshold

    p <- ggplot(d, aes(x = Column, y = Row, fill = Absorbance)) +
        coord_fixed(ratio = (13/12)/(9/8), xlim = c(0.5, 12.5), ylim = c(0.6, 8.4)) +
        scale_x_continuous(
            breaks = seq(1, 12),
            position = "top",
            sec.axis = dup_axis(labels = strain_labels)) +
        scale_y_reverse(
            breaks = seq(1, 8),
            labels = LETTERS[1:8],
            sec.axis = dup_axis(labels = concentration_labels, name = "Concentration")
        ) +
        scale_fill_gradient(
            limits = c(0, absorbance.max),
            low = "#EEEEEE",
            high = "#000000",
            name = "Absorbance"
        ) +
        theme_microtiter(base_size = 16)

    p <- p + labs(title = title, subtitle = subtitle)

    # Add points for the data
    p <- p + geom_point(aes(color = AboveThreshold), size = 16, shape = 21) +
        scale_color_manual(values = c("TRUE" = "grey70", "FALSE" = "red"), guide = FALSE)

    if (show.values) {
        p <- p + geom_text(aes(label = substr(Absorbance, 0, 5)), color = "blue", size = 3)
    }

    # Show wells that are not included in the data set with an X
    if (show.unused) {
        unused <- expand.grid(Row = 1:8, Column = 1:12) %>%
            dplyr::mutate(Well = as.factor(sprintf("%s%d", LETTERS[Row], Column))) %>%
            dplyr::anti_join(d, by = "Well")

        p <- p + geom_point(data = unused, color = "grey70", fill = "white", size = 16, shape = 21)
        p <- p + geom_point(data = unused, color = "grey70", fill = "grey70", shape = "x", size = 8)
    }

    p
}

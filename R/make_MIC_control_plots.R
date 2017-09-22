#' Create MIC Plate Diagrams for Control Plates
#'
#' @param control_data Data frame containing absorbance values for control plates
#' @param path Path in which to place plots (default: \code{"."}, the current working directory)
#' @param absorbance_max Value to be set as the maximum possible absorbance. Useful for having consistent greyscale across many plots (default: \code{3}, which is approx. where the spex maxes out).
#' @param ... Additional arguments, passed to \code{\link{plot_mic_plate}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_MIC_control_plots(my_control_data)
#' }
make_MIC_control_plots <- function(control_data, path = ".", absorbance_max = 3, ...) {
    assertthat::assert_that(
        is.data.frame(control_data),
        absorbance_max >= 0
    )

    if (!dir.exists(path)) {
        message(sprintf("'%s' does not exist. Creating.", path))
        dir.create(path)
    }

    control_data %>%
        split(.$Drug) %>%
        purrr::walk(function(x) {
            drug = unique(x$Drug)
            p <- plot_mic_plate(
                x,
                title = "Control Plate",
                absorbance_max = absorbance_max,
                ...
            )
            ggplot2::ggsave(
                filename = file.path(path, sprintf("%s-Control.pdf", drug)),
                plot = p
            )
        })
}

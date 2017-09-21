#' Create MIC Plate Diagrams for Class Data
#'
#' @param plate_data Data frame containing MIC assay data
#' @param path Path in which to place plots (default: \code{"."}, the current working directory)
#' @param absorbance_max Value to be set as the maximum possible absorbance. Useful for having consistent greyscale across many plots (default: \code{3}, which is approx. where the spex maxes out).
#' @param make_section_dirs Whether or not to create subdirectories for each section (default: \code{TRUE})
#' @param ... Additional arguments, passed to \code{\link{plot_mic_plate}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_MIC_section_plots(my_plate_data)
#' }
make_MIC_section_plots <- function(plate_data, path = ".", absorbance_max = 3.0, make_section_dirs = TRUE, ...) {
    assertthat::assert_that(
        is.data.frame(plate_data),
        absorbance_max >= 0
    )

    if (!dir.exists(path)) {
        message(sprintf("'%s' does not exist. Creating.", path))
        dir.create(path)
    }

    make_group_plot <- function(x, path, make_section_dirs, amax, ...) {
        drug <- unique(x$Drug)
        section <- unique(x$Section)
        section_string <- sprintf("Section_%s", section)
        group <- unique(x$Group)

        if (make_section_dirs) {
            newpath <- file.path(path, section_string)
            if (!dir.exists(newpath)) {
                dir.create(newpath)
            }
            path <- newpath
        }

        file_name_uncorr <- file.path(
            path,
            sprintf("S%s-G%s-%s.pdf", section, group, drug)
        )
        p <- plot_mic_plate(x, absorbance_max = absorbance_max, ...)
        ggplot2::ggsave(file_name_uncorr, p)

        file_name_corr <- file.path(
            path,
            sprintf("S%s-G%s-%s-corr.pdf", section, group, drug)
        )
        if (!all(is.na(x$Absorbance.Adj))) {
            p <- plot_mic_plate(x %>% dplyr::mutate(Absorbance = Absorbance.Adj), subtitle = sprintf("%s (corrected)", drug), absorbance_max = amax, ...)
            ggplot2::ggsave(file_name_corr, p)
        }

        data.frame(P1 = file_name_uncorr, P2 = file_name_corr)
    }

    # Plot the group plates
    x <- plate_data %>%
        dplyr::group_by(Drug, Section, Group) %>%
        dplyr::do(make_group_plot(., path, make_section_dirs = make_section_dirs, amax = absorbance_max, ...))

}

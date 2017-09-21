#' Theme for displaying Microtiter plates
#'
#' \code{theme_microtiter} is a \code{\link{ggplot2}} theme that produces a
#' plot that resembles a 96-well microtiter plate. This is useful for displaying
#' either data from the plate or information about the plate in a spatial
#' context.
#'
#' @export
#' @inheritParams theme_bdc_grey
#' @return A list of ggplot theme parameters
#'
theme_microtiter <- function(base_size = 14, base_family = "") {
    t <- theme_bdc_grey(base_size = base_size, base_family = base_family,
                   grid.x = FALSE, grid.y = FALSE,
                   gridmin.x = FALSE, gridmin.y = FALSE,
                   ticks.x = FALSE, ticks.y = FALSE,
                   pmargin = base_size / 2) %+replace%
        theme(
            panel.spacing = grid::unit(0, units = "pt"),
            axis.title = element_blank(),
            axis.text = element_text(size = rel(1.0), face = "bold"),
            axis.text.y = element_text(
                margin = margin(r = 0.3 * base_size, l = 0.3 * base_size)
            ),
            axis.text.y.right = element_text(color = "grey30", face = "plain", size = rel(0.7), hjust = 0),
            axis.text.x = element_text(color = "grey30", face = "plain", size = rel(0.6), angle = 35, hjust = 1, vjust = 1, margin = margin(t = 0.3 * base_size, b = 0.3 * base_size)),
            axis.text.x.top = element_text(color = "black", face = "bold", size = base_size, angle = 0, hjust = 0.5, vjust = 0),
            axis.ticks.length = unit(0, "pt"),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vertical",
            legend.spacing = unit(6, "pt"),
            plot.title = element_text(
                size = rel(1.2),
                face = "bold",
                hjust = 0.5,
                margin = margin(b = (base_size / 2))
            ),
            plot.subtitle = element_text(
                size = rel(1.2),
                color = "grey50",
                hjust = 0.5,
                margin = margin(b = (base_size / 2) * 1.5)
            )
        )

    t
}

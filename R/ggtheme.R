#' @title Standard theme for figures
#' @param base_size Base font size (default = 14)
#' @param base_family Base font family (default = "Helvetica")
#' @return ggplot2 theme
#' @export
#' @examples
#' \dontrun{
#' import(ggplot2)
#' ggplot(data = data, aes(x = x, y = y)) +
#'     geom_point() +
#'     default_theme()
#' }
default_theme <- function(base_size = 14, base_family = "Helvetica") {
    (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
    + ggplot2::theme(
            plot.title = ggplot2::element_text(
                face = "bold",
                size = ggplot2::rel(1.2), hjust = 0.5
            ),
            text = ggplot2::element_text(),
            panel.background = ggplot2::element_rect(colour = NA),
            plot.background = ggplot2::element_rect(colour = NA),
            panel.border = ggplot2::element_rect(colour = NA),
            axis.title = ggplot2::element_text(face = "bold", size = rel(1.1)),
            axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
            axis.title.x = ggplot2::element_text(vjust = -0.2),
            axis.text = ggplot2::element_text(size = rel(1)),
            axis.line = ggplot2::element_line(colour = "black"),
            axis.ticks = ggplot2::element_line(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.key = ggplot2::element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = grid::unit(0.2, "cm"),
            legend.spacing = grid::unit(0, "cm"),
            legend.title = ggplot2::element_text(face = "plain"),
            plot.margin = grid::unit(c(10, 5, 5, 5), "mm"),
            strip.background = ggplot2::element_rect(colour = "#ffffff", fill = "#ffffff"),
            strip.text = ggplot2::element_text(face = "bold", size = rel(1.1))
        ))
}

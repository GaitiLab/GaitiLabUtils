#' @title Extract clustree legend
#' @description Extracts the 'Cluster size', 'Sample count' and 'In-proportion' legend from a clustree.
#' @param p ggplot object
#' @return ggplot2 legend
#' @export
extract_default_clustree_legend <- function(p) {
    # Extract legends
    legend <- cowplot::get_plot_component(
        p +
            ggplot2::guides(
                color = FALSE,
                size = ggplot2::guide_legend(
                    title = "Cluster size",
                    title.position = "top",
                    title.hjust = 0.5,
                    label.position = "top",
                    label.hjust = 0.5,
                    order = 1,
                    nrow = 1
                ),
                edge_colour = ggraph::guide_edge_colourbar(
                    title = "Sample count",
                    title.position = "top",
                    title.hjust = 0.5,
                    barwidth = ggplot2::unit(20, "lines"),
                    barheight = ggplot2::unit(1, "lines"),
                    draw.ulim = TRUE,
                    draw.llim = TRUE,
                    order = 3
                ),
                edge_alpha = ggplot2::guide_legend(
                    title = "In-proportion",
                    title.position = "top",
                    title.hjust = 0.5,
                    label.position = "top",
                    label.hjust = 0.5,
                    override.aes = list(size = 10),
                    order = 4
                )
            ) +
            ggplot2::theme(
                legend.position = "bottom",
                legend.justification = "center",
                legend.title = ggplot2::element_text(size = 20)
            ),
        "guide-box-bottom",
        return_all = TRUE
    )

    return(legend)
}

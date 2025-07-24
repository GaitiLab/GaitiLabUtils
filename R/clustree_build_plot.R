#' @title Build clustree plot
#' @description Combine clustrees with overlay of expression of multiple genes that share a common legend.
#' @param plist names list of clustree plots, names are the genes
#' @param legend common legend to add, containing the cluster_size, sample count and In-propotion legend
#' @return cowplot grid with plots
#' @export
build_clustree_plot <- function(plist, legend) {
    # Grid with clustree for each gene of interest
    panel <- cowplot::plot_grid(
        plotlist = plist,
        labels = names(plist),
        label_size = 30,
        hjust = -0.1,
        nrow = 1
    )

    # Grid combining the panel and the legend ('Cluster size', 'Sample count' and 'In-proportion')
    panel_legend <- cowplot::plot_grid(
        panel,
        legend,
        nrow = 2,
        rel_heights = c(1, 0.15)
    )
    return(panel_legend)
}

#' @title Perform Elbow method
#' @param seurat_object seurat object
#' @param assay assay to use in Seurat object (default = 'RNA')
#' @param ndims Number of PCs to plot (default = 50)
#' @param reduction reduction to use for ElbowPlot (default = 'pca')
#' @param change_of_variation_perc Last point where change of % of variation is more than X%
#' @returns list with plot and number of optimal npcs
#' @export
elbow_method_wrapper <- function(
    seurat_object,
    assay = "RNA",
    ndims = 50,
    reduction = "pca",
    var = 0.05
) {
    Seurat::DefaultAssay(seurat_object) <- assay
    message("Determine number of optimal PCs...")
    npcs <- get_pcs(
        seurat_object,
        reduc = reduction,
        change_of_variation_perc = var,
        method = "elbow"
    )
    p <- Seurat::ElbowPlot(
        seurat_object,
        ndims = ndims,
        reduction = reduction
    ) +
        ggplot2::geom_vline(xintercept = npcs, lty = "dashed", color = "red") +
        ggplot2::annotate(
            "text",
            x = npcs + 3.5,
            y = 6,
            label = glue::glue("Optimal PCs={npcs}")
        ) +
        default_theme()
    message(glue::glue("Number of optimal PCs: {npcs}"))
    return(list(plot = p, optimal_npcs = npcs))
}

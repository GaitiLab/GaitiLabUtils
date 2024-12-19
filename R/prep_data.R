#' @title Perform Elbow method
#' @param seurat_object seurat object
#' @param assay assay to use in Seurat object (default = 'RNA')
#' @param ndims Number of PCs to plot (default = 50)
#' @param reduction reduction to use for ElbowPlot (default = 'pca')
#' @param var Last point where change of % of variation is more than X%
#' @returns list with plot and number of optimal npcs
#' @export
elbow_method_wrapper <- function(
    seurat_object, assay = "RNA",
    ndims = 50, reduction = "pca", var = 0.05) {
    Seurat::DefaultAssay(seurat_object) <- assay
    message("Determine number of optimal PCs...")
    npcs <- min(get_pcs(seurat_object, reduc = reduction, var = var))
    p <- Seurat::ElbowPlot(seurat_object,
        ndims = ndims, reduction = reduction
    ) +
        ggplot2::geom_vline(xintercept = npcs, lty = "dashed", color = "red") +
        ggplot2::annotate("text",
            x = npcs + 3.5, y = 6, label =
                glue::glue("Optimal PCs={npcs}")
        ) +
        default_theme()
    message(glue::glue("Number of optimal PCs: {npcs}"))
    return(list(plot = p, optimal_npcs = npcs))
}

#' @title Wrapper for normalization
#' @description takes a seurat object and performs 1) Log-normalization and 2) SCTransform
#' @param seurat_object seurat object
#' @param assay RNA assay, either 'RNA.008um' (when created with Load10X_Spatial) or 'RNA' (default = "RNA")
#' @param apply_sct_transform logical indicating whether to apply sctransform (default = 0)
#' @return seurat object
#' @export
normalizedata_wrapper <- function(seurat_object, assay = "RNA", apply_sct_transform = FALSE) {
    message("Approach 1. Normalize data (log-normalization)...")
    Seurat::DefaultAssay(seurat_object) <- assay
    seurat_object <- Seurat::NormalizeData(seurat_object)
    message("Find variable features..")
    seurat_object <- Seurat::FindVariableFeatures(seurat_object)
    message("Scale data...")
    seurat_object <- Seurat::ScaleData(seurat_object)
    if (apply_sct_transform) {
        message("Approach 2. Use SCT transform...")
        seurat_object <- Seurat::SCTransform(seurat_object)
    }
    return(seurat_object)
}

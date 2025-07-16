#' @title Wrapper for normalization
#' @description takes a seurat object and performs 1) Log-normalization and 2) SCTransform
#' @param seurat_object seurat object
#' @param assay RNA assay, either 'RNA.008um' (when created with Load10X_Spatial) or 'RNA' (default = "RNA")
#' @param apply_sct_transform logical indicating whether to apply sctransform (default = 0)
#' @return seurat object
#' @export
normalizedata_wrapper <- function(
    seurat_object,
    assay = "RNA",
    apply_sct_transform = FALSE
) {
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

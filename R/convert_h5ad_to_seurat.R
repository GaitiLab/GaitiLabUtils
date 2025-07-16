#' @title Convert H5ad Anndata object to Seurat object
#' @description Converts a h5ad file containing an AnnData object (Python) to Seurat object. Due to compatibility issues, only the assay data is converted, metadata needs to be added manually using the `metadata_df`. Metadata from AnnData can be exported in Python using the snippet below
#' """
#' import scanpy as sc
#' h5ad = "path-to-h5ad-file"
#' anndata = sc.read_h5ad(h5ad)
#' anndata.obs.to_csv("path-for-csv")
#' """
#' @param h5ad_path path to h5ad data
#' @param metadata_df dataframe where rownames are the barcodes/cell_ids that are present in the anndata object
#' @return Seurat object
#' @export
convert_h5ad_to_seurat <- function(h5ad_path, metadata_df = NULL) {
    SeuratDisk::Convert(
        h5ad_path,
        dest = "h5seurat",
        overwrite = TRUE
    )

    seurat_obj <- SeuratDisk::LoadH5Seurat(
        paste0(fs::path_ext_remove(h5ad_path), ".h5seurat"),
        # exporting metadata manually in Python to prevent error here due to format of metadata/misc slots
        meta.data = FALSE,
        misc = FALSE
    )
    if (!is.null(metadata_df)) {
        # Rownames in metadata_df should be the barcodes/cell_ids in the Seurat object
        if (intersect(rownames(metadata_df), colnames(seurat_obj))) {
            seurat_obj <- Seurat::AddMetaData(
                seurat_obj,
                metadata = metadata_df
            )
        }
    }
    return(seurat_obj)
}

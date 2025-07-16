#' @title Create annotation lookup table
#' @description Creates a look up table for annotating seurat object (Xenium) based on manually annotated clusters. Especially useful when multiple clusters have the same label
#' @param label label
#' @param annot dataframe containing the required columns: 'Cluster' and 'Label'
#' @param label_name name for column with labels, e.g. Label
#' @export
create_lookup_table <- function(label, annot, label_name) {
    annot_df <- data.frame(
        cluster = factor(
            annot |>
                dplyr::filter(Label == label) |>
                dplyr::pull(Clusters) |>
                stringr::str_split(",", simplify = TRUE) |>
                as.numeric()
        )
    )
    annot_df[, label_name] <- label
    return(annot_df)
}

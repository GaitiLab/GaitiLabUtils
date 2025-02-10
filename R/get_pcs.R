#' @title Function for getting get_pcs
#' @description Getting the number of optimal PCs to use, either determined using the 'elbow' (faster) or 'jackstraw' method (slower).
#' Method 1: 'elbow'
#' 1. The point where the PCs only contribute 5% of standard deviation and the principal components cumulatively contribute 90% of the standard deviation.
#' 2. The point where the percent change in variation between the consecutive PCs < 0.05%.
#' @param seurat_obj seurat object
#' @param reduc layer to use for 'reduction' (default='pca')
#' @param method method to use 'elbow' or 'jackstraw'
#' Parameters associated with 'elbow' method
#' @param cumu_filter PC exhibits cumulative percent greater than `cumu_filter`% (default=90)
#' @param perc_filter PC exhibits associated with %-variation < `perc_filter` (default=5)
#' @param n_dim number of dimensions to use for 'jackstraw' method (default=30)
#' @param change_of_variation_perc Last point where change of % of variation is more than X% used in 'elbow method' (default=0.05;0.05%)
#' Parameters associated with 'jackstraw' method
#' @param dim_filter filter for JackStraw score and getting the optimal number of PCs used in the 'jackstraw' method (default=5^(-15))
#' @export
#' @references https://github.com/tiroshlab/Spatial_Glioma/blob/main/Module1_MP.R (jackstraw implementation)
#' @references https://hbctraining.github.io/scRNA-seq/lessons/elbow_plot_metric.html (elbow implementation)
get_pcs <- function(
    seurat_obj,
    reduc = "pca",
    method = c("elbow", "jackstraw")[1],
    # Elbow method
    cumu_filter = 90,
    perc_filter = 5,
    change_of_variation_perc = 0.05,
    # Jackstraw
    n_dim = 30,
    dim_filter = 5^(-15)) {
    if (method == "elbow") {
        # Determine percent of variation associated with each PC
        pct <- seurat_obj[[reduc]]@stdev / sum(seurat_obj[[reduc]]@stdev) * 100

        # Calculate cumulative percents for each PC
        cumu <- cumsum(pct)
        # Determine which PC exhibits cumulative percent greater than 90% and %
        # variation associated with the PC as less than 5
        co1 <- which(cumu > cumu_filter & pct < perc_filter)[1]
        # Determine the difference between variation of PC and subsequent PC
        # Last point where change of % of variation is more than 0.05%
        diff_var <- (pct[seq(1, length(pct) - 1)] - pct[seq(2, length(pct))])
        co2 <- sort(which(diff_var > change_of_variation_perc),
            decreasing = TRUE
        )[1] + 1
        return(min(c(co1, co2)))
    } else if (method == "jackstraw") {
        seurat_obj <- Seurat::JackStraw(seurat_obj, num.replicate = 100)
        seurat_obj <- Seurat::ScoreJackStraw(seurat_obj, dims = seq_len(n_dim))
        js_scores <- seurat_obj[[reduc]]@jackstraw$overall.p.values
        return(max(js_scores[, "PC"][js_scores[, "Score"] < dim_filter]))
    } else {
        stop("Choose valid method: 'elbow' or 'jackstraw'...")
    }
}

#' @title Order stacked barplot
#' @param df_wide dataframe in wide format, containing at least the columns `order_by` and `id`, in addition with columns containing the measurements/values for different levels (categorical) of a variable.
#' @param order_by vector containing the names in `df_wide` to order by
#' @param id name of column in `df_wide` containing the ids, these are the bars in barplot
#' @param pc principal component to use for ordering
#' @return vector with order for `id`
#' @export
order_stacked_barplot <- function(df_wide, order_by, id, pc = 1) {
    pca_result <- prcomp(
        df_wide |> dplyr::select(dplyr::where(is.numeric)),
        scale. = TRUE
    )
    df_wide$PC <- pca_result$x[, pc]

    # Step 4: Reorder ROI factor levels by PC1
    roi_order <- df_wide |>
        dplyr::arrange(!!!dplyr::syms(order_by), PC) |>
        dplyr::pull(!!dplyr::sym(id))

    return(roi_order)
}

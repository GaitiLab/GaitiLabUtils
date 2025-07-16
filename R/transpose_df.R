#' @title Transpose dataframe
#' @param df dataframe
#' @param set_index set 'col1' as index by default FALSE,
#' @return transposed dataframe
#' @export
transpose_df <- function(df, set_index = FALSE) {
    t_df <- df |>
        tibble::rownames_to_column("rowname") |>
        tidyr::pivot_longer(
            cols = !rowname,
            names_to = "col1",
            values_to = "col2"
        ) |>
        tidyr::pivot_wider(names_from = "rowname", values_from = "col2")

    if (set_index) {
        t_df <- t_df |> tibble::column_to_rownames("col1")
    }
    return(t_df)
}

#' @title Merge list of dataframes
#' @param list_of_dfs list of dataframes for merging
#' @param by merge on key (Default = i)
#' @param all.x merge on x (left dataframe)
#' @export
merge_dfs <- function(list_of_dfs, by, all.x = TRUE) {
    Reduce(
        function(dtf1, dtf2) merge(dtf1, dtf2, by = by, all.x = all.x),
        list_of_dfs
    )
}

#' @title Transpose dataframe
#' @param df dataframe
#' @param set_index set 'col1' as index by default FALSE,
#' @return transposed dataframe
#' @export
#' @importFrom dplyr %>%
transpose_df <- function(df, set_index = FALSE) {
    t_df <- df %>%
        tibble::rownames_to_column() %>%
        tidyr::pivot_longer(!rowname, names_to = "col1", values_to = "col2") %>%
        tidyr::pivot_wider(names_from = "rowname", values_from = "col2")

    if (set_index) {
        t_df <- t_df %>% tibble::column_to_rownames("col1")
    }
    return(t_df)
}

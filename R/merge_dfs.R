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

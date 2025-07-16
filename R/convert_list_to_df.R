#' @title Convert list to dataframe
#' @description Convert list of vectors with unequal lengths to dataframe
#' @param ls_vec list of vectors
#' @return dataframe
#' @export
convert_list_to_df <- function(ls_vec) {
    n_lengths <- lapply(ls_vec, length) |> unlist()
    df_nrows <- max(n_lengths)
    n_pad <- lapply(ls_vec, function(vec) {
        df_nrows - length(vec)
    })
    df <- lapply(names(ls_vec), function(name) {
        c(sort(ls_vec[[name]]), rep(NA, n_pad[[name]]))
    }) |>
        as.data.frame()
    colnames(df) <- names(ls_vec)
    return(df)
}

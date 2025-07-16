#' @title Dynamically set bracket heights
#' @description Compute bracket heights for plots with statistical tests, with brackets to show p-values. Here, the heights of the brackets is computed in a dynamic way, where `height` should be the value of the largest value in the plot, then `offset` is added to this height and offset controls the space between brackets, to prevent overlap.
#' @param height base height, e.g. max value in plot
#' @param n number of brackets, i.e. number of statistical tests performed
#' @param offset space between base height and first bracket
#' @param vert_spacing_fct factor to use for spacing between brackets
#' @return numerical vector of length `n`
#' @export
get_bracket_height_rolling <- function(
    height,
    n,
    offset = 3,
    vert_spacing_fct = 6
) {
    base_height <- rep(height, n)
    return(
        base_height +
            offset +
            ave(base_height, base_height, FUN = function(z) {
                seq_along(z) - 1
            }) *
                vert_spacing_fct
    )
}


#' @title Set bracket height for p-values
#' @param df dataframe
#' @param col name of column with groups (x-axis)
#' @param metric variable that is measured (y-axis)
#' @param fct factor to determine height
#' @return dataframe with bracket heights for each level in `col`
#' @export
get_bracket_height <- function(df, col, metric, fct = 1.05) {
    col_levels <- df |>
        dplyr::pull(!!dplyr::sym(col)) |>
        unique()
    bracket_height <- setNames(
        sapply(
            col_levels,
            function(col_level, metric) {
                p <- boxplot(
                    df |>
                        dplyr::filter(!!dplyr::sym(col) == col_level) |>
                        dplyr::pull(!!dplyr::sym(metric))
                )
                return(max(c(p$out, p$stats), na.rm = TRUE))
            },
            metric = metric
        ),
        col_levels
    )
    return(
        data.frame(bracket_height) |>
            tibble::rownames_to_column(col) |>
            dplyr::mutate(bracket_height = bracket_height * fct)
    )
}

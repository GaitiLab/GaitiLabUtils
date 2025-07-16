#' @title Determine no. rows in facet grid
#' @param n_plots number of total facets (plot)
#' @param n_plots_per_row number of plots per row, i.e. number of columns
#' @return numerical value
#' @export
compute_nrows_in_facetgrid <- function(n_plots, n_plots_per_row = 5) {
    return(ceiling(n_plots / n_plots_per_row))
}

#' @title Compute total facetgrid dimensions
#' @param n_plots totla number of facets (plots)
#' @param width width of single facet plot (inches)
#' @param height width of single facet plot (inches)
#' @param n_plots_per_row number of plots per row, i.e. number of columns
#' @return list with `width`, `height` & `nrows`
#' @export
compute_total_facetgrid_dimensions <- function(
    n_plots,
    width = 5,
    height = 5,
    n_plots_per_row = 5
) {
    nrows <- compute_nrows_in_facetgrid(n_plots, n_plots_per_row)
    return(list(
        width = n_plots_per_row * width,
        height = nrows * height,
        nrows = nrows
    ))
}

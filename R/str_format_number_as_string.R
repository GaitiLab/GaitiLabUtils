#' @title Format number as string
#' @param x number/float
#' @param n_dec number of decimals to use
#' @export
format_number_as_string <- function(x, n_dec = 2) {
    sprintf(glue::glue("%.{n_dec}f"), x)
}

#' @title Pretty limits
#' @description Set 'pretty' limits based on vector of values. Can be used for `limits=` argument in ggplot2() functions #nolint
#' @param x vector of numeric values
#' @param n number of breaks (default=5)
#' @export
#' @references https://stackoverflow.com/a/67865447/19429268
pretty_limits <- function(x, n = 5) {
    # pretty_lim
    r2 <- ifelse(x < 0, 0, x)
    pr <- pretty(r2, n)
    r_out <- range(pr)
    return(r_out)
}

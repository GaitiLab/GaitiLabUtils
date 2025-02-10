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

#' @title Pretty unexpanded limits
#' @description Set 'pretty' breaks based on vector of values. Can be used for `breaks=` argument in ggplot2() functions. Alternative for scales::pretty_breaks() #nolint
#' @param x vector of numeric values
#' @param n number of breaks (default=5)
#' @export
#' @references https://stackoverflow.com/a/67865447/19429268
pretty_breaks_unexpanded <- function(x, n = 5) {
    # expand_limit
    if (x[1] <= 0) {
        r2 <- x + c(-x[1], x[1])
    } else {
        r2 <- x + c((x[2] - x[1]) * 0.04545455, -(x[2] - x[1]) * 0.04545455)
    }
    pout <- pretty(r2, n)
    return(pout)
}

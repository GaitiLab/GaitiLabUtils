#' @title Cut-off values by percentile
#' @param vec vector with numerical values
#' @param prob probability to use for cut-off
#' @return vector
#' @export
cutoff_values <- function(vec, prob = .95) {
    cutoff <- quantile(vec, prob = prob, na.rm = TRUE)
    vec[vec > cutoff] <- cutoff
    return(vec)
}

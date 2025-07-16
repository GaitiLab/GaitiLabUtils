#' @title Min-max scaling
#' @param x numerical vector
#' @return single numerical value
#' @export
min_max_scaling <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

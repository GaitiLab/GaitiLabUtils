#' @title Compute overlap coefficient
#' @param vec1 character vector
#' @param vec2 character vector
#' @return single numerical value
#' @export
overlap_coefficient <- function(vec1, vec2) {
    return(length(intersect(vec1, vec2)) / min(c(length(vec1), length(vec2))))
}

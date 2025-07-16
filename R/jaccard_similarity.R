#' @title Compute Jaccard similarity
#' @param vec1 vector
#' @param vec2 vector
#' @return jaccard similarity
#' @export
jaccard_similarity <- function(vec1, vec2) {
    intersection <- length(intersect(vec1, vec2))
    union <- length(unique(c(vec1, vec2)))
    return(intersection / union)
}

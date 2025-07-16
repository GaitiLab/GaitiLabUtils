#' @title Compute Sorenson-Dice index
#' @param vec1 character vector
#' @param vec2 character vector
#' @return single numerical value
#' @export
sorensen_dice_index <- function(vec1, vec2) {
    return(2 * length(intersect(vec1, vec2)) / (length(vec1) + length(vec2)))
}

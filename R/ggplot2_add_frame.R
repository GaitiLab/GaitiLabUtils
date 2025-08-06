#' @title Add frame to ggplot2
#' @description adds border to all sides of the plot
#' @param color color to use for borders
#' @param linewidth widht of borders
#' @return ggplot2 element/object
#' @export
add_frame <- function(color = "black", linewidth = 1) {
    return(ggplot2::theme(
        panel.border = ggplot2::element_rect(
            color = color,
            fill = NA,
            linewidth = linewidth
        )
    ))
}

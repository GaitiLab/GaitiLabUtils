#' @title Add scalebar
#' @description Adds scalebar to ggplot2 object
#' @param xpos x-position to start the scalebar
#' @param size size (width) of scalebar
#' @param ypos y-position to start the scalebar
#' @param height height of scalebar
#' @param unit character string for the unit to add to the scalebar
#' @param color color of scalebar
#' @param fontsize fontsize of text used for scalebar
#' @return ggplot2 annotation
#' @export
add_scalebar <- function(
    xpos = 200,
    size = 2000,
    ypos = 200,
    height = 50,
    unit = "µm",
    color = "white",
    fontsize = 3.88
) {
    return(
        list(
            ggplot2::annotate(
                "rect",
                xmin = xpos,
                xmax = xpos + size,
                ymin = ypos,
                ymax = ypos + height,
                fill = color
            ),
            ggplot2::annotate(
                "text",
                x = xpos + size * .5,
                y = ypos,
                label = paste0(size, unit),
                vjust = -.5,
                hjust = .5,
                color = color,
                size = fontsize
            )
        )
    )
}

#' @title Remove axes from ggplot object
#' @return list with ggplot2 theme
#' @export
remove_axes <- function() {
    return(list(ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
    )))
}

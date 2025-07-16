#' @title Explode p-value
#' @param p p-value
#' @description Dissects p-value to be used in `format_p_value`
#' @return named list with each item a single value
explode_p <- function(p) {
    p <- as.numeric(p)
    p_sci <- format(p, scientific = TRUE, digits = 2)
    parts <- strsplit(p_sci, "e")[[1]]
    coef <- parts[1]
    exponent <- as.integer(parts[2])
    return(list(coef = coef, exponent = exponent))
}

#' @title Format p-value
#' @description Format p-value for plots, reformat to 'A x 10^exp'
#' @param p p-value
#' @param smallest_p  smallest p value to display
#' @return float
#' @export
format_p_value <- function(p, smallest_p = 2.2e-16) {
    p <- as.numeric(p)

    p_smallest_exploded <- explode_p(smallest_p)
    p_exploded <- explode_p(p)

    if (p_exploded[["exponent"]] < p_smallest_exploded[["exponent"]]) {
        return(bquote(
            "<" ~ .(p_smallest_exploded[["coef"]]) ~ "x" ~
                10^.(p_smallest_exploded[["exponent"]])
        ))
    } else if (identical(p, 0)) {
        return(bquote(
            "<" ~ .(p_smallest_exploded[["coef"]]) ~ "x" ~
                10^.(p_smallest_exploded[["exponent"]])
        ))
    } else if (p_exploded[["exponent"]] < -3) {
        return(bquote(
            .(p_exploded[["coef"]]) ~ "x" ~ 10^.(p_exploded[["exponent"]])
        ))
    } else {
        return(format(p, scientific = FALSE, digits = 3))
    }
}

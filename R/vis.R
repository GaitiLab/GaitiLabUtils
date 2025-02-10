#' @title Autocrop plot
#' @description Crop plot to remove white space
#' @param filename filename
#' @return NA
#' @export
auto_crop <- function(filename) {
    knitr::plot_crop(filename)
}

#' geom signif with lmm test. See geom_signif from ggsignif for more information
#'
#' @param data_df data frame containing data
#' @param response name of column in data_df that corresponds to the response variable
#' @param condition name of column in data_df that corresponds to condition of interest
#' @param latent_vars vector containing columns in data_df that correspond to variables that describe the nested structure of the data e.g. Patient
#' @param comparisons list of vectors of length two that define the comparisons to make
#' @param y_position position of brackets (default=NULL)
#' @param tip_length length of bracket tips (default = 0.03)
#' @param size size of brackets (default = 0.5)
#' @param step_increase distance between brackets (default = 0)
#' @return geom signif
#' @examples
#' \dontrun{
#' comparisons <- list(c("condition1", "condition2"), c("condition2", "condition3"))
#' p <- ggplot(data = data_df, aes(x = condition, y = response, fill = condition)) +
#'     geom_boxplot() +
#'     geom_signif_lmm(data_df = data_df, response = "response", condition = "condition", latent_vars = "Patient", comparisons = comps, steps_increase = c(0, 0.1, 0.1))
#' }
#' @export
#' @importFrom dplyr %>%
geom_signif_lmm <- function(
    data_df, response, condition, latent_vars,
    comparisons, y_position = NULL,
    tip_length = 0.03, size = 0.5, step_increase = 0) {
    p_vals <- sapply(seq_along(comparisons), function(comp) {
        curr_comparison <- comparisons[[comp]]
        curr_df <- data_df %>%
            filter(!!rlang::sym(condition) %in% curr_comparison)

        return(round(LMM_test(curr_df, response, condition, latent_vars),
            digits = 5
        ))
    })

    return(
        ggsignif::geom_signif(
            comparisons = comparisons,
            map_signif_level = FALSE,
            annotations = p_vals,
            y_position = y_position,
            tip_length = tip_length,
            size = size,
            step_increase = step_increase
        )
    )
}

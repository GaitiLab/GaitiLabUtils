#' geom signif with lmm test. See geom_signif from ggsignif for more information
#'
#' @param data_df data frame containing data
#' @param response name of column in data_df that corresponds to the response variable
#' @param condition name of column in data_df that corresponds to condition of interest
#' @param method one of the following 'nested_t_test' or 'mixed_effects_anova' (default="nested_t_test")
#' @param latent_vars vector containing columns in data_df that correspond to variables that describe the nested structure of the data e.g. Patient
#' @param comparisons list of vectors of length two that define the comparisons to make (default=NULL)
#' @param y_position position of brackets (default=NULL)
#' @param tip_length length of bracket tips (default=0.03)
#' @param size size of brackets (default=0.5)
#' @param step_increase distance between brackets (default=0)
#' @param verbose (default=TRUE)
#' @return geom signif
#' @export
geom_signif_lmm <- function(
    data_df,
    response,
    condition,
    latent_vars,
    method = "nested_t_test",
    comparisons = NULL,
    y_position = NULL,
    tip_length = 0.03,
    size = 0.5,
    step_increase = 0,
    verbose = TRUE
) {
    if (!method %in% c("nested_t_test", "mixed_effects_anova")) {
        stop("Method needs to be nested_t_test or mixed_effects_anova")
    }

    if (method == "nested_t_test") {
        p_vals <- sapply(seq_along(comparisons), function(comp) {
            curr_comparison <- comparisons[[comp]]
            curr_df <- data_df |>
                filter(!!dplyr::sym(condition) %in% curr_comparison)

            pval <- format(
                LMM_test(curr_df, response, condition, latent_vars),
                scientific = TRUE,
                digits = 5
            )
            return(pval)
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
    } else if (method == "mixed_effects_anova") {
        if (!is.null(comparisons)) {
            stop("Comparisons needs to be NULL for ANOVA")
        }

        if (class(data_df[[condition]]) != "factor") {
            stop("X-axis variable needs to be factor")
        }

        middle_category <- levels(data_df[[condition]])[ceiling(
            length(levels(data_df[[condition]])) / 2
        )]

        anova_pval <- format(
            LMM_test(
                data_df,
                response,
                condition,
                latent_vars,
                verbose = verbose
            ),
            scientific = TRUE,
            digits = 5
        )
        return(
            ggplot2::annotate(
                "text",
                x = middle_category,
                y = Inf,
                label = paste("ANOVA:", anova_pval),
                hjust = 0.5,
                vjust = 1.5
            )
        )
    }
}

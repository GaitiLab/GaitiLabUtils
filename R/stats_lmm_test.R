#' perform a nested t-test using LMMs.
#'
#' @param data_df data frame containing data
#' @param response name of column in data_df that corresponds to the response variable
#' @param condition name of column in data_df that corresponds to condition of interest
#' @param latent_vars vector containing columns in data_df that correspond to variables that describe the nested structure of the data e.g. Patient
#' @param vebose (default=TRUE)
#' @return p-value derived from likelihood ratio test of model incorporating condition vs null model
#' @export
LMM_test <- function(
    data_df,
    response,
    condition,
    latent_vars,
    verbose = TRUE
) {
    full_formula <- as.formula(
        paste(
            response,
            "~",
            condition,
            "+",
            paste("(1 |", latent_vars, ")", collapse = "+")
        )
    )

    null_formula <- as.formula(
        paste(
            response,
            "~ 1 +",
            paste("(1 |", latent_vars, ")", collapse = "+")
        )
    )

    # Fit the models
    model <- lme4::lmer(full_formula, data = data_df, REML = FALSE)
    model_null <- lme4::lmer(null_formula, data = data_df, REML = FALSE)

    # Perform the likelihood ratio test
    res <- anova(model_null, model)

    if (verbose) {
        print(res)
    }

    # Return the p-value from the Chi-square test
    return(res$`Pr(>Chisq)`[2])
}

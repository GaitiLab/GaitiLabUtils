#' @title Compute p-val for one-sample permutation test
#' @description Compute p-value from null distribution and observed measurement (single value)
#' @param null_dist vector with the null distribution obtained by shuffling/randomizing (without replacement)
#' @param true_val observed value
#' @param alternative alternative hypothesis, 'less', 'greater' or 'two.sided' (default = 'less')
#' @references adapted from http://users.stat.umn.edu/~helwig/notes/perm-Notes.pdf (slide 18)
#' @export
one_sample_perm_test_pval <- function(null_dist, true_val, alternative = c("less", "greater", "two.sided")[1]) {
    if (alternative == "less") {
        message("Testing hypothesis: H0 > H1")
        # Adding '+1' to prevent pval = 0 -> smallest p 1/ number of permutations a.k.a. length of null_dist
        pval <- (sum(null_dist <= true_val) + 1) / length(null_dist)
    } else if (alternative == "greater") {
        message("Testing hypothesis: H0 < H1")
        pval <- (sum(null_dist >= true_val) + 1) / length(null_dist)
    } else if (alternative == "two.sided") {
        message("Testing hypothesis: H0 != H1")
        pval <- (sum(abs(null_dist) >= abs(true_val)) + 1) / length(null_dist)
    } else {
        stop("Invalid alternative hypothesis, please choose: 'less', 'greater' or 'two.sided'")
    }
    return(pval)
}
#' perform a nested t-test using LMMs.
#'
#' @param data_df data frame containing data
#' @param response name of column in data_df that corresponds to the response variable
#' @param condition name of column in data_df that corresponds to condition of interest
#' @param latent_vars vector containing columns in data_df that correspond to variables that describe the nested structure of the data e.g. Patient
#' @param vebose (default=TRUE)
#' @return p-value derived from likelihood ratio test of model incorporating condition vs null model
#' @export
LMM_test <- function(data_df, response, condition, latent_vars, verbose = TRUE) {
    full_formula <- as.formula(
        paste(response, "~", condition, "+", paste("(1 |", latent_vars, ")", collapse = "+"))
    )

    null_formula <- as.formula(
        paste(response, "~ 1 +", paste("(1 |", latent_vars, ")", collapse = "+"))
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

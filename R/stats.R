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

#' @title Extract name from filepath
#' @param filepath filepath
#' @return name of file without extension
#'
#' @examples 
#' \dontrun{
#' get_name("R/my_script.R") returns 'my_script'
#' }
#' @export
#'
get_name <- function(filepath) {
    return(tools::file_path_sans_ext(fs::path_file(filepath)))
}

#' @title Create directory
#'
#' @description Create directory if it does not exist
#'
#' @param dir_path Path to directory to be created
#'
#' @examples 
#' \dontrun{
#' create_dir("/Users/johndoe/my_new_dir")
#' }
#' @export
#' @importFrom glue glue
create_dir <- function(dir_path) {
    if (!dir.exists(dir_path)) {
        message(glue::glue("Creating directory {dir_path}"))
        dir.create(dir_path, recursive = TRUE)
    } else {
        message(glue::glue("Directory {dir_path} already exists."))
    }
}

#' @title Obtain current date
#' @description Obtain current date in format YYYYMMDD
#' @return current date
#' @export
get_current_date <- function() {
    return(format(Sys.time(), "%Y%m%d"))
}


#' @title Transpose dataframe
#' @param df dataframe
#' @param set_index set 'col1' as index by default FALSE,
#' @return transposed dataframe
#' @export
#' @importFrom dplyr %>%
transpose_df <- function(df, set_index = FALSE) {
    t_df <- df %>%
        tibble::rownames_to_column() %>%
        tidyr::pivot_longer(!rowname, names_to = "col1", values_to = "col2") %>%
        tidyr::pivot_wider(names_from = "rowname", values_from = "col2")

    if (set_index) {
        t_df <- t_df %>% tibble::column_to_rownames("col1")
    }
    return(t_df)
}

#' @title Function for getting get_pcs
#' @param seurat_obj seurat object
#' @param reduc layer to use for 'reduction' (default: pca)
#' @param var Last point where change of % of variation is more than X%
#' @export
get_pcs <- function(seurat_obj, reduc = "pca", var = 0.05) {
    # Determine percent of variation associated with each PC
    pct <- seurat_obj[[reduc]]@stdev / sum(seurat_obj[[reduc]]@stdev) * 100
    # Calculate cumulative percents for each PC
    cumu <- cumsum(pct)
    # Determine which PC exhibits cumulative percent greater than 90% and %
    # variation associated with the PC as less than 5
    co1 <- which(cumu > 90 & pct < 5)[1]
    # Determine the difference between variation of PC and subsequent PC
    # Last point where change of % of variation is more than 0.05%
    co2 <- sort(which((pct[seq(1, length(pct) - 1)] - pct[seq(2, length(pct))]) > var),
        decreasing = TRUE
    )[1] + 1
    c(co1, co2)
}

#' @title Generate pairs,
#' @param v vector with labels
#' @param remove_self remove self-pairs
#' @param return_undirected remove directed pairs, i.e. A-B == B-A
#' @param collapse delimiter
#' @export
generate_pairs <- function(v, collapse = "__", remove_self = TRUE, return_undirected = TRUE) {
    pairs <- expand.grid(v, v)
    if (remove_self) {
        pairs <- pairs[pairs[, 1] != pairs[, 2], ]
    }
    if (return_undirected) {
        return(unique(apply(pairs, 1, function(x) paste0(sort(x), collapse = collapse))))
    } else {
        return(apply(pairs, 1, paste0, collapse = collapse))
    }
}

#' @title Merge list of dataframes 
#' @param list_of_dfs list of dataframes for merging
#' @param by merge on key (Default = i)
#' @param all.x merge on x (left dataframe)
#' @export
merge_dfs <- function(list_of_dfs, by, all.x = TRUE) {
    Reduce(
        function(dtf1, dtf2) merge(dtf1, dtf2, by = by, all.x = all.x),
        list_of_dfs
    )
}

#' @title Format large numbers with scientific notation 
#' @param value the string or number to be formatted
#' @references https://stackoverflow.com/a/24241954
#' @export
format_scientific <- function(value) {
    # turn in to character string in scientific notation
    value <- format(value, scientific = TRUE)
    value <- gsub("0e\\+00", "0", value)
    # quote the part before the exponent to keep all the digits
    value <- gsub("^(.*)e", "'\\1'e", value)
    # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2)
    value <- gsub("e\\+", "e", value)
    # convert 1x10^ or 1.000x10^ -> 10^
    value <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", value)

    # turn the 'e+' into plotmath format
    value <- gsub("e", "%*%10^", value)
    # return this as an expression
    parse(text = value)
}
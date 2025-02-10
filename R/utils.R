#' @title Create directory
#' @description Create directory if it does not exist
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

#' @title Generate pairs,
#' @param v vector with labels
#' @param remove_self remove self-pairs
#' @param return_undirected remove directed pairs, i.e. A-B == B-A
#' @param collapse delimiter
#' @export
generate_pairs <- function(
    v, collapse = "__",
    remove_self = TRUE, return_undirected = TRUE) {
    pairs <- expand.grid(v, v)
    if (remove_self) {
        pairs <- pairs[pairs[, 1] != pairs[, 2], ]
    }
    if (return_undirected) {
        return(unique(apply(
            pairs, 1,
            function(x) paste0(sort(x), collapse = collapse)
        )))
    } else {
        return(apply(pairs, 1, paste0, collapse = collapse))
    }
}

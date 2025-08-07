#' @title Create directory
#' @description Create directory if it does not exist
#' @param dir_path Path to directory to be created
#' @param verbose log messages
#' @examples
#' \dontrun{
#' create_dir("/Users/johndoe/my_new_dir")
#' }
#' @export
#' @importFrom glue glue
create_dir <- function(dir_path, verbose = TRUE) {
    if (!dir.exists(dir_path)) {
        if (verbose) {
            message(glue::glue("Creating directory {dir_path}"))
        }
        dir.create(dir_path, recursive = TRUE)
    } else {
        if (verbose) {
            message(glue::glue("Directory {dir_path} already exists."))
        }
    }
}

#' @title Generate pairs,
#' @param v vector with labels
#' @param remove_self remove self-pairs
#' @param return_undirected remove directed pairs, i.e. A-B == B-A
#' @param collapse delimiter
#' @export
generate_pairs <- function(
    v,
    collapse = "__",
    remove_self = TRUE,
    return_undirected = TRUE
) {
    pairs <- expand.grid(v, v)
    if (remove_self) {
        pairs <- pairs[pairs[, 1] != pairs[, 2], ]
    }
    if (return_undirected) {
        return(unique(apply(
            pairs,
            1,
            function(x) paste0(sort(x), collapse = collapse)
        )))
    } else {
        return(apply(pairs, 1, paste0, collapse = collapse))
    }
}

#' @title Check whether a path is valid
#' @param path character string to be checked
#' @param expected_type 'file' or 'dir'
#' @param required_file_extension in case expected_type == "file", you can also check for a specific file extension (case insensitive)
#' @return logical
#' @export
is_valid_path <- function(
    path,
    expected_type = c("file", "dir")[1],
    required_file_extension = NULL
) {
    match.arg(expected_type, c("file", "dir"))
    if (is.null(path) || !is.character(path) || is.na(path)) {
        return(FALSE)
    }

    if (expected_type == "file") {
        if (is.null(required_file_extension)) {
            return(fs::file_exists(path))
        } else {
            return(
                fs::file_exists(path) &&
                    (stringr::str_to_lower(fs::path_ext(path)) ==
                        stringr::str_to_lower(required_file_extension))
            )
        }
    } else if (expected_type == "dir") {
        return(fs::dir_exists(path))
    }
    return(FALSE)
}

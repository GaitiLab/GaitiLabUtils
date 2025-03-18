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

#' @title Get color palette from Excel file
#' @param name name of color palette
#' @param excel_filepath path to Excel file containing the color palette(s). A color palette is defined by two columns: (1) <name>, (2) <name>_color.
#' @param sheet_name the name of the sheet in the Excel file containing the color palette(s) (default='color_palettes')
#' @return named list with the colors (<name>_color) and names as <name>
#' @importFrom dplyr %>%
get_color_palette <- function(name, excel_filepath, sheet_name = "color_palettes") {
    cols <- c(name, paste(name, "color", sep = "_"))
    color_palette <- readxl::read_excel(
        excel_filepath,
        sheet = sheet_name
    ) %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        tidyr::drop_na() %>%
        as.list()

    return(setNames(color_palette[[cols[2]]], color_palette[[1]]))
}

#' @title Get color palette from Excel file
#' @param name name of color palette
#' @param excel_filepath path to Excel file containing the color palette(s). A color palette is defined by two columns: (1) <name>, (2) <name>_color.
#' @param sheet_name the name of the sheet in the Excel file containing the color palette(s) (default='color_palettes')
#' @return named list with the colors (<name>_color) and names as <name>
#' @export
get_color_palette <- function(
    name,
    excel_filepath,
    sheet_name = "color_palettes"
) {
    cols <- c(name, paste(name, "color", sep = "_"))
    color_palette <- readxl::read_excel(
        excel_filepath,
        sheet = sheet_name
    ) |>
        dplyr::select(dplyr::all_of(cols)) |>
        tidyr::drop_na() |>
        as.list()

    return(setNames(color_palette[[cols[2]]], color_palette[[1]]))
}

#' @title Determine optimal ComplexHeatmap size
#' @description Obtain optimal size of heatmap or list of heatmaps
#' @param hm heatmap or list of heatmaps
#' @param m margin (default = 4)
#' @return list of height and width
#' @export
get_optimal_output_size <- function(hm, m = 4) {
    # First draw the heatmap to obtain the size of the plot
    hm_obj <- ComplexHeatmap::draw(hm)

    # Obtain height and width and convert to inches
    ht_height <- sum(ComplexHeatmap::component_height(hm_obj)) +
        grid::unit(m, "mm")
    ht_height <- grid::convertHeight(ht_height, "inch", valueOnly = TRUE)

    ht_width <- sum(ComplexHeatmap::component_width(hm_obj)) +
        grid::unit(m, "mm")
    ht_width <- grid::convertHeight(ht_width, "inch", valueOnly = TRUE)
    return(list(height = ht_height, width = ht_width))
}


#' @title Setup cell function for ComplexHeatmap
#' @param matrix matrix to use
#' @param is_upper_tri default=FALSE,
#' @param add_annot default= TRUE
#' @references https://jokergoo.github.io/2021/07/22/make-triangle-heatmap/
#' @export
get_cell_function <- function(matrix, is_upper_tri = FALSE, add_annot = TRUE) {
    # Full matrix
    cell_fun_annot <- function(j, i, x, y, width, height, fill) {
        grid::grid.rect(
            x = x,
            y = y,
            width = width,
            height = height,
            gp = grid::gpar(col = "grey", fill = fill, lwd = 0.2)
        )
        grid::grid.text(matrix[i, j], x, y, gp = grid::gpar(fontsize = 5))
    }

    cell_fun_no_annot <- function(j, i, x, y, width, height, fill) {
        grid::grid.rect(
            x = x,
            y = y,
            width = width,
            height = height,
            gp = grid::gpar(col = "grey", fill = fill, lwd = 0.2)
        )
    }

    # Triangular
    cell_fun_tri_annot <- function(j, i, x, y, width, height, fill) {
        if (i <= j) {
            grid::grid.rect(
                x = x,
                y = y,
                width = width,
                height = height,
                gp = grid::gpar(col = "grey", fill = fill, lwd = 0.2)
            )
            grid::grid.text(matrix[i, j], x, y, gp = grid::gpar(fontsize = 5))
        }
    }

    cell_fun_tri_no_annot <- function(j, i, x, y, width, height, fill) {
        if (i <= j) {
            grid::grid.rect(
                x = x,
                y = y,
                width = width,
                height = height,
                gp = grid::gpar(col = "grey", fill = fill, lwd = 0.2)
            )
        }
    }

    if (is_upper_tri) {
        if (add_annot) {
            return(cell_fun_tri_annot)
        } else {
            return(cell_fun_tri_no_annot)
        }
    } else {
        if (add_annot) {
            return(cell_fun_annot)
        } else {
            return(cell_fun_no_annot)
        }
    }
}

#' @title Get size of heatmap (in mm)
#' @description use desired cell_width and cell_height to compute size of matrix
#' @param matrix matrix
#' @param cell_width height of cell in mm, default=0.1 (square)
#' @param cell_height width of cell in mm, default=0.1 (square)
#' @return list with height and width
get_cell_dims <- function(matrix, cell_width = 0.1, cell_height = 0.1) {
    height <- nrow(matrix) * grid::unit(cell_height, "mm")
    width <- ncol(matrix) * grid::unit(cell_width, "mm")
    return(list(height = height, width = width))
}

#' @title Save heatmap
#' @description Save heatmap with optimal output size (removing redundant whitespace)
#' @param hm_obj heatmap created with ComplexHeatmap or create_hm()
#' @param annotation_legend_side position of annotation legend by default = 'right'
#' @param heatmap_legend_side position of annotation legend by default = 'right'
#' @param merge_legend merge legends, by default = TRUE
#' @param annotation_legend_list list of custom legends, by default = NULL
#' @param heatmap_legend_list list of custom legends, by default = NULL
#' @param output_file path for output file by default = "heatmap.pdf"
#' @importFrom ComplexHeatmap draw
#' @export
save_hm <- function(
    hm_obj,
    annotation_legend_side = "right",
    heatmap_legend_side = "right",
    merge_legend = TRUE,
    heatmap_legend_list = NULL,
    annotation_legend_list = NULL,
    output_file = "heatmap.pdf"
) {
    # Only heatmap legends
    if (!is.null(heatmap_legend_list) && is.null(annotation_legend_list)) {
        hm_obj <- ComplexHeatmap::draw(
            hm_obj,
            merge_legend = merge_legend,
            heatmap_legend_list = heatmap_legend_list,
            heatmap_legend_side = heatmap_legend_side,
        )
        # Only annotation legend
    } else if (
        (is.null(heatmap_legend_list) && !is.null(annotation_legend_list))
    ) {
        hm_obj <- ComplexHeatmap::draw(
            hm_obj,
            merge_legend = merge_legend,
            annotation_legend_side = annotation_legend_side,
            annotation_legend_list = annotation_legend_list
        )
        # Both heatmap and annotation legend
    } else if (
        !is.null(heatmap_legend_list) && !is.null(annotation_legend_list)
    ) {
        hm_obj <- ComplexHeatmap::draw(
            hm_obj,
            merge_legend = merge_legend,
            annotation_legend_side = annotation_legend_side,
            annotation_legend_list = annotation_legend_list,
            heatmap_legend_side = heatmap_legend_side,
            heatmap_legend_list = heatmap_legend_list
        )
    } else {
        # No legends provided
        hm_obj <- ComplexHeatmap::draw(
            hm_obj,
            heatmap_legend_side = heatmap_legend_side,
            annotation_legend_side = annotation_legend_side,
            merge_legend = merge_legend
        )
    }
    hm_size <- get_optimal_output_size(hm_obj)
    pdf(output_file, width = hm_size$width, height = hm_size$height)
    ComplexHeatmap::draw(hm_obj)
    dev.off()
}

#' Plot heatmap + save
#' @param matrix matrix
#' @param cell_width height of cell in mm, default=0.1 (square)
#' @param cell_height width of cell in mm, default=0.1 (square)
#' @param is_full plot full matrix (default = TRUE)
#' @return hm
#' @export
create_hm <- function(
    matrix,
    cell_width = 4,
    cell_height = 4,
    is_full = TRUE,
    ...
) {
    cell_dims <- get_cell_dims(
        matrix = matrix,
        cell_width = cell_width,
        cell_height = cell_height
    )
    if (is_full) {
        hm <- ComplexHeatmap::Heatmap(
            matrix = matrix,
            # Size of cells (use square)
            height = cell_dims$height,
            width = cell_dims$width,
            ...
        )
    } else {
        hm <- ComplexHeatmap::Heatmap(
            matrix = matrix,
            # Size of cells (use square)
            height = cell_dims$height,
            width = cell_dims$width,
            rect_gp = grid::gpar(type = "none"),
            cluster_columns = FALSE,
            cluster_rows = FALSE,
            ...
        )
    }
    return(hm)
}

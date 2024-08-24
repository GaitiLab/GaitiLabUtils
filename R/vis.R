#' @title Autocrop plot
#' @description Crop plot to remove white space
#' @param filename filename
#' @return NA
#' @export
auto_crop <- function(filename) {
    knitr::plot_crop(filename)
}

#' @title Determine optimal ComplexHeatmap size
#' @description Obtain optimal size of heatmap or list of heatmaps
#' @param hm heatmap or list of heatmaps
#' @param m margin
#' @return list of height and width
#' @export
#' @importFrom ComplexHeatmap draw component_height component_width
#' @importFrom grid convertHeight
get_optimal_output_size <- function(hm, m = 4) {
    # First draw the heatmap to obtain the size of the plot
    hm_obj <- draw(hm)

    # Obtain height and width and convert to inches
    ht_height <- sum(component_height(hm_obj)) + unit(m, "mm")
    ht_height <- convertHeight(ht_height, "inch", valueOnly = TRUE)

    ht_width <- sum(component_width(hm_obj)) + unit(m, "mm")
    ht_width <- convertHeight(ht_width, "inch", valueOnly = TRUE)
    return(list(height = ht_height, width = ht_width))
}


#' @title Setup cell function for ComplexHeatmap 
#' @param matrix matrix to use
#' @param is default=FALSE,
#' @param add_annot default= TRUE
#' @references https://jokergoo.github.io/2021/07/22/make-triangle-heatmap/
#' @export
get_cell_function <- function(matrix, is_upper_tri = FALSE, add_annot = TRUE) {
    # Full matrix
    cell_fun_annot <- function(j, i, x, y, width, height, fill) {
        grid.rect(
            x = x, y = y, width = width, height = height,
            gp = gpar(col = "grey", fill = fill, lwd = 0.2)
        )
        grid.text(matrix[i, j], x, y, gp = gpar(fontsize = 5))
    }

    cell_fun_no_annot <- function(j, i, x, y, width, height, fill) {
        grid.rect(
            x = x, y = y, width = width, height = height,
            gp = gpar(col = "grey", fill = fill, lwd = 0.2)
        )
    }

    # Triangular
    cell_fun_tri_annot <- function(j, i, x, y, width, height, fill) {
        if (i <= j) {
        grid.rect(
            x = x, y = y, width = width, height = height,
            gp = gpar(col = "grey", fill = fill, lwd = 0.2)
        )            
        grid.text(matrix[i, j], x, y, gp = gpar(fontsize = 5))
        } 
    }

    cell_fun_tri_no_annot <- function(j, i, x, y, width, height, fill) {
        if (i <= j) {
            grid.rect(
                x = x, y = y, width = width, height = height,
                gp = gpar(col = "grey", fill = fill, lwd = 0.2)
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
#' @param cell_width width of cell in mm, default=0.1 (square)
#' @return list with height and width
#' @importFrom grid unit
get_cell_dims <- function(matrix, cell_width = 0.1, cell_height = 0.1) {
    height <- nrow(matrix) * unit(cell_height, "mm")
    width <- ncol(matrix) * unit(cell_width, "mm")
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
    annotation_legend_list = NULL, output_file = "heatmap.pdf") {
    # Only heatmap legends
    if (!is.null(heatmap_legend_list) & is.null(annotation_legend_list)) {
        hm_obj <- draw(hm_obj,
            merge_legend = merge_legend,
            annotation_legend_side = annotation_legend_side,
            annotation_legend_list = annotation_legend_list,
        )
        # Only annotation legend
    } else if ((is.null(heatmap_legend_list) & !is.null(annotation_legend_list))) {
        hm_obj <- draw(hm_obj,
            merge_legend = merge_legend,
            annotation_legend_side = annotation_legend_side,
            annotation_legend_list = annotation_legend_list
        )
        # Both heatmap and annotation legend
    } else if (!is.null(heatmap_legend_list) & !is.null(annotation_legend_list)) {
        hm_obj <- draw(hm_obj,
            merge_legend = merge_legend,
            annotation_legend_side = annotation_legend_side,
            annotation_legend_list = annotation_legend_list,
            heatmap_legend_side = heatmap_legend_side,
            heatmap_legend_list = heatmap_legend_list
        )
    } else {
        # No legends provided
        hm_obj <- draw(hm_obj, heatmap_legend_side = heatmap_legend_side, annotation_legend_side = annotation_legend_side, merge_legend = merge_legend)
    }
    hm_size <- get_optimal_output_size(hm_obj)
    pdf(output_file, width = hm_size$width, height = hm_size$height)
    draw(hm_obj)
    dev.off()
}

#' Plot heatmap + save
#' @param mat matrix
#' @param col_fun color function
#' @param output_file output file name
#' @param legend_title legend title
#' @param save_plot save plot
#' @param is_full plot full matrix (default = TRUE)
#' @return hm
#' @export
#' @inheritParams ComplexHeatmap::Heatmap
create_hm <- function(
    matrix,
    cell_width = 4,
    cell_height = 4,
    is_full = TRUE,
    ...) {
    cell_dims <- get_cell_dims(matrix = matrix, cell_width = cell_width, cell_height = cell_height)
    if (is_full) {
    hm <- ComplexHeatmap::Heatmap(
        matrix = matrix,
        # Size of cells (use square)
        height = cell_dims$height,
        width = cell_dims$width,
        ...
    )} else { 
        hm <- ComplexHeatmap::Heatmap(
        matrix = matrix,
        # Size of cells (use square)
        height = cell_dims$height,
        width = cell_dims$width,
        rect_gp = gpar(type = "none"),
        cluster_columns = FALSE, 
        cluster_rows = FALSE,
        ...)
    }
    return(hm)
}


#' geom signif with lmm test. See geom_signif from ggsignif for more information
#' 
#' @param data_df data frame containing data
#' @param response name of column in data_df that corresponds to the response variable
#' @param condition name of column in data_df that corresponds to condition of interest
#' @param latent_vars vector containing columns in data_df that correspond to variables that describe the nested structure of the data e.g. Patient
#' @param comparisons list of vectors of length two that define the comparisons to make
#' @param y_position position of brackets 
#' @param tip_length length of bracket tips
#' @param size size of brackets
#' @param step_increase distance between brackets
#' @return geom signif
#' @examples 
#' \dontrun {
#' comparisons <- list(c("condition1", "condition2"), c("condition2", "condition3"))
#' p <- ggplot(data = data_df, aes(x = condition, y = response, fill = condition)) + geom_boxplot() + geom_signif_lmm(data_df = data_df, response = "response", condition = "condition", latent_vars = "Patient", comparisons = comps, steps_increase = c(0, 0.1, 0.1))
#' }
#' @export
geom_signif_lmm <- function(data_df, response, condition, latent_vars,
    comparisons, y_position = NULL, tip_length = 0.03, size = 0.5, step_increase = 0) {

    p_vals <- sapply(seq_along(comparisons), function(comp) {
        
        curr_comparison <- comparisons[[comp]]
        curr_df <- data_df %>% 
            filter(!!rlang::sym(condition) %in% curr_comparison)
        
        return(round(LMM_test(curr_df, response, condition, latent_vars), digits = 5))
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
}
#' @title Get density of points in 2 dimensions.
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param n Create a square n by n grid to compute density.
#' @return The density within each square.
#' @export
get_density <- function(x, y, ...) {
    dens <- MASS::kde2d(x, y, ...)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
}

#' @title Visualize transcripts
#' @description visualize transcripts of a gene and color by density estimates
#' @param gene gene name
#' @param transcripts dataframe with the following required columns x_location, y_location and feature_name
#' @param n_bins number of bins to use for KDE2D (default = 100)
#' @param output_dir output directory for saving (default = ".")
#' @param save_plot whether to save the plot (default = TRUE)
#' @param transcript_size Size of transcript (dot) on plot (default = 0.1)
#' @importFrom dplyr %>%
#' @export
visualize_transcripts <- function(gene = NULL, transcripts, n_bins = 100, output_dir = ".", save_plot = TRUE, transcript_size = 0.1) {
    if (is.null(gene)) {
        transcripts_subset <- transcripts
    } else {
        transcripts_subset <- transcripts %>% filter(feature_name %in% gene)
    }

    transcripts_subset <- transcripts_subset %>%
        tidyr::drop_na(dplyr::any_of(c("x_location", "y_location"))) %>%
        dplyr::mutate(x_location = as.numeric(x_location), y_location = as.numeric(y_location)) %>%
        tidyr::drop_na(dplyr::any_of(c("x_location", "y_location")))

    transcripts_subset$density <- get_density(
        transcripts_subset$x_location,
        transcripts_subset$y_location,
        n = n_bins
    )

    if (nrow(transcripts_subset) > 50e3) {
        transcript_size <- 0.02
    } else if (nrow(transcripts_subset) > 10e3) {
        transcript_size <- 0.05
    } else {
        transcript_size <- 0.1
    }

    p <- ggplot2::ggplot(transcripts_subset) +
        ggrastr::geom_point_rast(ggplot2::aes(x_location, y_location, color = density), size = transcript_size) +
        ggplot2::scale_color_continuous(
            type = "viridis",
            breaks = scales::pretty_breaks(),
            limits = c(0, NA),
            guide = ggplot2::guide_colorbar(
                title = "Density",
                barwidth = ggplot2::unit(15, "lines"), barheight = unit(0.7, "lines"),
            )
        ) +
        default_theme() +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "black"), aspect.ratio = 1,
            legend.title.position = "top",
            legend.direction = "horizontal",
        ) +
        ggplot2::labs(
            x = "X (µm)", y = "Y (µm)",
            subtitle = glue::glue("N={nrow(transcripts_subset)}")
        ) +
        ggplot2::coord_equal(ratio = 1) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::expand_limits(x = 0, y = 0) +
        ggplot2::scale_y_reverse()
    if (save_plot) {
        if (is.null(gene)) {
            gene <- "all_genes"
        } else if (length(gene == 1)) {
            ggplot2::ggsave(
                plot = p,
                filename = glue::glue("{output_dir}/transcript_distribution_{gene}.pdf"), width = 10, height = 10
            )
        } else {
            ggplot2::ggsave(
                plot = p,
                filename = glue::glue("{output_dir}/transcript_distribution_multi_genes.pdf"), width = 10, height = 10
            )
        }
    }
    return(p)
}


#' @title create polygons
#' @param x cell_id
#' @param cell_boundaries dataframe with at least the required columns: 'vertex_x', 'vertex_y' and 'cell_id'
#' @return dataframe with 'vertex_x', 'vertex_y' and 'cell_id'
#' @importFrom dplyr %>%
#' @export
create_polygon <- function(x, cell_boundaries) {
    coords <- cell_boundaries %>%
        filter(cell_id == x) %>%
        dplyr::select(vertex_x, vertex_y) %>%
        dplyr::distinct() %>%
        tidyr::drop_na()
    # Polygon needs at least 3 points
    if (nrow(coords) > 2) {
        # Add first coordinates for closing polygon and add cell_id
        return(rbind(coords, coords[1, ]) %>% dplyr::mutate(cell_id = x))
    }
}

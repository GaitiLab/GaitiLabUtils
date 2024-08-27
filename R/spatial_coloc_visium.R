#' @title Find neighbours in window
#' @param adj_table  adjacency table
#' @param radius radius to use
#' @references Adapted from Greenwald 2024 (https://github.com/tiroshlab/Spatial_Glioma)
#' @export
#' @return matrix
find_neighbours_in_window <- function(adj_table, radius) {
    return(sapply(rownames(adj_table), find_neighbours_in_window_around_spot, adj_table = adj_table, radius = radius))
}

#' @title Find neighbours in window around spot
#' @param adj_table  adjacency table
#' @param radius radius to use
#' @param spot_barcode barcode of spot to look at
#' @export
#' @references Adapted from Greenwald 2024 (https://github.com/tiroshlab/Spatial_Glioma)
#' @return vector with barcodes of neighbors
find_neighbours_in_window_around_spot <- function(adj_table, radius, spot_barcode) {
    neighbours <- c(find_neighbouring_spots(adj_table, spot_barcode))
    if (radius == 1) {
        return(neighbours)
    } else {
        neighbours <- c(neighbours, unlist(lapply(neighbours, find_neighbours_in_window_around_spot, adj_table = adj_table, radius = radius - 1)))
        return(unique(neighbours))
    }
}

#' @title Find neighbouring spots
#' @param adj_table  adjacency table
#' @param spot_barcode barcode of spot to select
#' @references Adapted from Greenwald 2024 (https://github.com/tiroshlab/Spatial_Glioma)
#' @export
#' @return vector with barcodes of neighbors
find_neighbouring_spots <- function(adj_table, spot_barcode) {
    return(na.omit(adj_table[spot_barcode, ]))
}

#' @title Build adjacency table
#' @param spot_pos_type dataframe with spot positions + info
#' @references Adapted from Greenwald 2024 (https://github.com/tiroshlab/Spatial_Glioma)
#' @export
#' @return adjacency matrix
build_adjacency_table <- function(spot_pos_type) {
    adj_table <- sapply(spot_pos_type$barcodes, function(spot) {
        spots_row <- spot_pos_type[spot_pos_type$barcodes == spot, "array_row"]
        spots_col <- spot_pos_type[spot_pos_type$barcodes == spot, "array_col"]

        # bottom left
        if (spots_col == 0 | spots_row == 0) {
            c1 <- NA
        } else {
            curr_spot <- spot_pos_type[spot_pos_type$array_row == (spots_row - 1) & spot_pos_type$array_col == (spots_col - 1), ]
            spot_exists <- length(rownames(curr_spot)) == 1
            if (spot_exists) {
                n1 <- spot_pos_type$barcodes[spot_pos_type$array_row == spots_row - 1 & spot_pos_type$array_col == spots_col - 1]
                c1 <- n1
            } else {
                c1 <- NA
            }
        }

        # bottom right
        if (spots_col == 127 | spots_row == 0) {
            c2 <- NA
        } else {
            curr_spot <- spot_pos_type[spot_pos_type$array_row == (spots_row - 1) & spot_pos_type$array_col == (spots_col + 1), ]
            spot_exists <- length(rownames(curr_spot)) == 1
            if (spot_exists) {
                n2 <- spot_pos_type$barcodes[spot_pos_type$array_row == spots_row - 1 & spot_pos_type$array_col == spots_col + 1]
                c2 <- n2
            } else {
                c2 <- NA
            }
        }

        # left
        if (spots_col == 0 | spots_col == 1) {
            c3 <- NA
        } else {
            curr_spot <- spot_pos_type[spot_pos_type$array_row == (spots_row) & spot_pos_type$array_col == (spots_col - 2), ]
            spot_exists <- length(rownames(curr_spot)) == 1
            if (spot_exists) {
                n3 <- spot_pos_type$barcodes[spot_pos_type$array_row == spots_row & spot_pos_type$array_col == spots_col - 2]
                c3 <- n3
            } else {
                c3 <- NA
            }
        }

        # right
        if (spots_col == 126 | spots_col == 127) {
            c4 <- NA
        } else {
            curr_spot <- spot_pos_type[spot_pos_type$array_row == (spots_row) & spot_pos_type$array_col == (spots_col + 2), ]
            spot_exists <- length(rownames(curr_spot)) == 1
            if (spot_exists) {
                n4 <- spot_pos_type$barcodes[spot_pos_type$array_row == spots_row & spot_pos_type$array_col == spots_col + 2]
                c4 <- n4
            } else {
                c4 <- NA
            }
        }

        # top left
        if (spots_col == 0 | spots_row == 77) {
            c5 <- NA
        } else {
            curr_spot <- spot_pos_type[spot_pos_type$array_row == (spots_row + 1) & spot_pos_type$array_col == (spots_col - 1), ]
            spot_exists <- length(rownames(curr_spot)) == 1
            if (spot_exists) {
                n5 <- spot_pos_type$barcodes[spot_pos_type$array_row == spots_row + 1 & spot_pos_type$array_col == spots_col - 1]
                c5 <- n5
            } else {
                c5 <- NA
            }
        }

        # top right
        if (spots_col == 127 | spots_row == 77) {
            c6 <- NA
        } else {
            curr_spot <- spot_pos_type[spot_pos_type$array_row == (spots_row + 1) & spot_pos_type$array_col == (spots_col + 1), ]
            spot_exists <- length(rownames(curr_spot)) == 1
            if (spot_exists) {
                n6 <- spot_pos_type$barcodes[spot_pos_type$array_row == spots_row + 1 & spot_pos_type$array_col == spots_col + 1]
                c6 <- n6
            } else {
                c6 <- NA
            }
        }


        return(c(c1, c2, c3, c4, c5, c6))
    })

    adj_table <- t(adj_table)
    row.names(adj_table) <- spot_pos_type$barcodes

    return(adj_table)
}

#' @title Inspect Neighbors
#' @description Determine the number of neighbors per cell type for a cell of interest
#' @param cell_id cell_id for which you want to assess its neighbors
#' @param nn_id_list output from dbscan::frNN()
#' @param group_varname variable containing your labels for the cell_ids, e.g. cell type
#' @param df_with_labels dataframe with 'cell_ids' and your 'group_varname'
#' @return return dataframe with the columns: labels_of_neighbors, cell_id, n_total, and n_cells
#' @export
inspect_neighbors <- function(
    cell_id,
    nn_id_list,
    group_varname,
    df_with_labels
) {
    if (length(nn_id_list[[cell_id]]) > 0) {
        labels_of_neighbors <- df_with_labels[
            nn_id_list[[cell_id]],
            group_varname
        ]

        n_total <- length(labels_of_neighbors)

        # For genes, cells can express both ligand/receptor, incl. both. Consequence, fraction doesn't sum to 1
        labels_of_neighbors <- stringr::str_split(labels_of_neighbors, ",") |>
            unlist()

        # Adding 'total_neighbor_counts' as column to track whether there are cells expressing both genes, only relevant when using genes
        return(
            cbind(
                data.frame(table(labels_of_neighbors)),
                data.frame(
                    cell_id = cell_id,
                    n_total = n_total,
                    total_neighbor_counts = length(labels_of_neighbors)
                )
            ) |>
                dplyr::rename(n_cells = Freq)
        )
    }
}


#' @title Inspect k nearest Neighbors
#' @description Determine the number of neighbors per cell type for a cell of interest
#' @param cell_id cell_id for which you want to assess its neighbors
#' @param nn_id_mat output from dbscan::frNN()
#' @param group_varname variable containing your labels for the cell_ids, e.g. cell type
#' @param df_with_labels dataframe with 'cell_ids' and your 'group_varname'
#' @return return dataframe with the columns: labels_of_neighbors, cell_id, n_total, and n_cells
#' @export
inspect_k_nearest_neighbors <- function(
    cell_id,
    nn_id_mat,
    group_varname,
    df_with_labels
) {
    labels_of_neighbors <- df_with_labels[nn_id_mat[cell_id, ], group_varname]

    # For genes, cells can express both ligand/receptor, incl. both. Consequence, fraction doesn't sum to 1
    labels_of_neighbors <- stringr::str_split(labels_of_neighbors, ",") |>
        unlist()

    # Adding 'total_neighbor_counts' as column to track whether there are cells expressing both genes, only relevant when using genes
    return(
        cbind(
            data.frame(
                table(labels_of_neighbors)
            ),
            data.frame(
                cell_id = cell_id,
                total_neighbor_counts = length(labels_of_neighbors)
            )
        ) |>
            dplyr::rename(
                n_cells = Freq,
            )
    )
}

#' @title Compute neighbor metrics
#' @description Determine the number of neighbors per cell type for a cell of interest
#' @param approach 'knn' or 'radius'
#' @param dbscan_res results of dbscan
#' @param cell_id cell_id for which you want to assess its neighbors
#' @param group_varname variable containing your labels for the cell_ids, e.g. cell type
#' @param df_with_labels dataframe with 'cell_ids' and your 'group_varname'
#' @param n_cores number of cores to use
#' @param k_neighbors number of neighbors when approach = 'knn'
#' @return return dataframe with the columns: labels_of_neighbors, cell_id, n_total, and n_cells
#' @export
compute_neighbor_metrics <- function(
    approach,
    dbscan_res,
    group_varname,
    df_with_labels,
    n_cores = 1,
    k_neighbors = NA
) {
    valid_approaches <- c("knn", "radius")
    if (!approach %in% valid_approaches) {
        stop("no valid approach chosen, please choose 'knn' or 'radius'")
    }

    if (approach == "knn") {
        message("Inspect k-nearest neighbors for each cell...")
        neighbors_df <- do.call(
            rbind,
            parallel::mclapply(
                rownames(dbscan_res$id),
                inspect_k_nearest_neighbors,
                nn_id_mat = dbscan_res$id,
                group_varname = group_varname,
                df_with_labels = df_with_labels,
                mc.cores = n_cores
            )
        ) |>
            dplyr::mutate(n_total = k_neighbors)
    } else if (approach == "radius") {
        message("Inspect neighbors within radius for each cell...")
        neighbors_df <- do.call(
            rbind,
            parallel::mclapply(
                names(dbscan_res$id),
                inspect_neighbors,
                nn_id_list = dbscan_res$id,
                group_varname = group_varname,
                df_with_labels = df_with_labels,
                mc.cores = n_cores
            )
        )
    }
    message(
        "Assign cell type to each cell for which the neighbors were inspected..."
    )
    cell_id_with_label <- df_with_labels |>
        dplyr::select(cell_id, !!dplyr::sym(group_varname)) |>
        # Source/sender cells
        dplyr::rename(source = !!dplyr::sym(group_varname))

    message("Merge dataframes...")
    cell_oi_with_neighbors_df <- neighbors_df |>
        # Label source/sender cells ('cell_id')
        dplyr::left_join(cell_id_with_label, by = "cell_id") |>
        dplyr::mutate(
            # Compute fraction
            frac = n_cells / n_total
        ) |>
        # Neighbors are receivers/targets
        dplyr::rename(target = labels_of_neighbors)

    message("Compute averages per source-target cell type...")
    cell_oi_with_neighbors_df <- cell_oi_with_neighbors_df |>
        dplyr::group_by(source, target) |>
        dplyr::summarise(
            mean_frac = mean(frac),
            mean_n_cells = mean(n_cells),
            # Note for 'knn' approach this is 'k-neighbors'
            mean_n_total = mean(n_total),
            mean_total_neighbor_counts = mean(total_neighbor_counts)
        ) |>
        tidyr::unite(
            source_target,
            c(source, target),
            sep = "__",
            remove = FALSE
        ) |>
        # Removing all 'group_by' groups
        data.frame()
    return(cell_oi_with_neighbors_df)
}

#' @title Compute p-val permutation testing
#' @param celltype_pair_oi string with cell type pair of interest existing in shuffled_df and obs_df
#' @param shuffled_df dataframe with 'source_target' and 'metric' for all iterations (permutations)
#' @param obs_df dataframe with 'source_target' and 'metric' (observed)
#' @param metric string with metric of interest, e.g. 'mean_frac'
#' @return dataframe of single row with 'source_target', 'mean_obs', 'overall_mean_shuffled' and 'pval'
#' @export
compute_perm_pval <- function(celltype_pair_oi, shuffled_df, obs_df, metric) {
    # select data for cell type pair
    means_shuffled <- shuffled_df |>
        dplyr::filter(source_target == celltype_pair_oi) |>
        dplyr::pull(!!dplyr::sym(metric))
    mean_obs <- obs_df |>
        dplyr::filter(source_target == celltype_pair_oi) |>
        dplyr::pull(!!dplyr::sym(metric))

    # Compute p-value
    mean_null <- mean(means_shuffled)

    # Test for co-localization, expected if co-localized that means_shuffled < mean_obs
    pval <- GaitiLabUtils::one_sample_perm_test_pval(
        null_dist = means_shuffled,
        true_val = mean_obs,
        alternative = "greater"
    )

    return(
        data.frame(
            source_target = celltype_pair_oi,
            mean_obs = mean_obs,
            mean_null = mean_null,
            pval = pval,
            mean_diff = mean_obs - mean_null
        )
    )
}

#' @title spatialCrossCorTest
#' @description Adaptation of spatialCrossCorTest from MERINGUE.
#' @param i iteration (numeric)
#' @param x vector with expression of gene
#' @param y vector with expression of gene
#' @param w adjacency matrix
#' @export
#' @return dataframe with iteration and SCC
compute_shuffled_autocorr <- function(i, x, y, w) {
    message(glue::glue("Iteration: {i}..."))
    set.seed(i)
    xbg <- sample(x)
    names(xbg) <- names(x)
    return(data.frame(ix = i, I = MERINGUE::spatialCrossCor(xbg, y, w)))
}

#' Title
#'
#' @param Combined_dataset
#' @param group
#' @param n_cells_per_group
#'
#' @return
#' @export
#'
#' @examples
Calculate_Density <- function(Combined_dataset, group = c("sample.visitName"), n_cells_per_group = 100000) {
  Cord_and_meta_data <- cbind(Combined_dataset[[]], Combined_dataset@reductions$umap@cell.embeddings)
  split_group <- list()

  for (i in 1:length(group)) {
    split_group[[i]] <- Cord_and_meta_data[, group[i]]
  }
  subset <- split(Cord_and_meta_data, split_group, drop = TRUE)
  group_name <- NULL

  Density_list <- list()
  for (z in 1:length(subset)) {
    subset[[z]] <- subset[[z]][sample(nrow(subset[[z]]), n_cells_per_group, replace = T), ]
    group_name <- rbind(group_name, unique(subset[[z]][group]))
    rownames(group_name) <- NULL
    Density_list[[z]] <- kde2d(subset[[z]]$UMAP_1, subset[[z]]$UMAP_2, n = 100)
  }

  Density_list[[z + 1]] <- group_name
  return(Density_list)
}

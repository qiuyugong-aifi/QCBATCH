#' Title
#'
#' @param combined_dataset
#' @param point_size
#' @param cell_type
#'
#' @return
#' @export
#'
#' @examples
Batch_UMAP_Plot <- function(combined_dataset, point_size = 0.001, cell_type = "seurat_pbmc_type") {
  umap_cord <- cbind(combined_dataset[[]], combined_dataset@reductions$umap@cell.embeddings)
  p <- ggplot(umap_cord[sample(nrow(umap_cord), nrow(umap_cord)), ], aes(x = UMAP_1, y = UMAP_2, color = batch_id)) +
    geom_point(size = point_size) +
    facet_wrap(as.formula(paste("~", cell_type))) +
    guides(colour = guide_legend(override.aes = list(size = 10))) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.key.size = unit(1, "cm"),
      legend.text = element_text(size = 15),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15)
    )
  return(p)
}

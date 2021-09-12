#' Title
#'
#' @param combined_dataset
#' @param group
#' @param dot_alpha
#' @param dot_size
#' @param plot_width
#' @param plot_height
#'
#' @return
#' @export
#'
#' @examples
UMAP_Scatter_Plot <- function(combined_dataset, group = "seurat_pbmc_type", dot_alpha = 0.3, dot_size = 0.001, plot_width = 1000, plot_height = 800) {
  data_test <- cbind(combined_dataset[[]], combined_dataset@reductions$umap@cell.embeddings)
  p <- ggplot(data = data_test[sample(nrow(data_test), nrow(data_test)), ], aes(x = UMAP_1, y = UMAP_2, color = (!!as.name(group)))) +
    geom_point(alpha = dot_alpha, size = dot_size)
  fig <- ggplotly(p, width = plot_width, height = plot_height)
  fig <- fig %>% toWebGL()
  return(fig)
}

#' Title
#'
#' @param combined_dataset
#' @param n_pca_umap
#'
#' @return
#' @export
#'
#' @examples
Process_Dataset <- function(combined_dataset, n_pca_umap = 30) {
  combined_dataset <- NormalizeData(combined_dataset, normalization.method = "LogNormalize", scale.factor = 10000)
  combined_dataset <- FindVariableFeatures(combined_dataset, selection.method = "vst", nfeatures = 3000)
  combined_dataset <- ScaleData(combined_dataset)
  combined_dataset <- RunPCA(combined_dataset, features = VariableFeatures(object = combined_dataset))
  combined_dataset <- FindNeighbors(combined_dataset, dims = 1:n_pca_umap)
  combined_dataset <- FindClusters(combined_dataset, resolution = 0.5)
  combined_dataset <- RunUMAP(combined_dataset, dims = 1:n_pca_umap)

  return(combined_dataset)
}

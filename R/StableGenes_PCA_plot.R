#' Title
#'
#' @param Combined_dataset
#' @param n_PCs
#'
#' @return
#' @export
#'
#' @examples
StableGenes_PCA_plot <- function(Combined_dataset, n_PCs = 30) {
  var_gene <- intersect(Stable_gene$gene, rownames(Combined_dataset@assays$RNA@data))
  Combined_dataset <- NormalizeData(Combined_dataset, normalization.method = "LogNormalize", scale.factor = 10000)
  Combined_dataset <- FindVariableFeatures(Combined_dataset)
  Combined_dataset_Stable <- ScaleData(Combined_dataset, features = var_gene)
  Combined_dataset_Stable <- RunPCA(Combined_dataset_Stable, features = var_gene, npcs = 50, approx = TRUE)
  Combined_dataset_Stable <- RunUMAP(Combined_dataset_Stable, reduction = "pca", dims = 1:n_PCs)
  p <- DimPlot(Combined_dataset_Stable, pt.size = 0.001, reduction = "umap", raster = FALSE, group.by = "batch_id", repel = T, shuffle = TRUE) + guides(color = guide_legend(override.aes = list(size = 4), ncol = 1))
  return(p)
}

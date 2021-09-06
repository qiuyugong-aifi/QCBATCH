#' Title
#'
#' @param combined_dataset
#' @param cell_type
#' @param group
#'
#' @return
#' @export
#'
#' @examples
CellType_Composition_Heatmap <- function(combined_dataset, cell_type = "seurat_pbmc_type", group = "pbmc_sample_id") {
  cell_composition <- combined_dataset[[]] %>%
    dplyr::select((!!as.name(cell_type)), (!!as.name(group))) %>%
    dplyr::group_by((!!as.name(group)), (!!as.name(cell_type))) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = n / sum(n)) %>%
    dplyr::select((!!as.name(group)), (!!as.name(cell_type)), percent)

  cell_composition <- data.frame(spread(cell_composition, key = (!!as.name(cell_type)), value = percent))

  rownames(cell_composition) <- cell_composition %>%
    dplyr::select((!!as.name(group))) %>%
    pull()
  cell_composition <- cell_composition[, 2:ncol(cell_composition)]


  fig <- plot_ly(
    x=colnames(cell_composition),y=rownames(cell_composition),
    z = as.matrix(cell_composition), type = "heatmap",width=800, height=length(rownames(cell_composition))*20
  )

  return(fig)
}

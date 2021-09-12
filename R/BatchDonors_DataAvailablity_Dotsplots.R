#' Title
#'
#' @param BR1_rna_file
#' @param meta_data
#'
#' @return
#' @export
#'
#' @examples
BatchDonors_DataAvailablity_Dotsplots <- function(rna_dataframe, meta_data = "sample.visitName", width = 1000, height = 500) {
  plot_info <- rna_dataframe %>% dplyr::select(
    (!!as.name(meta_data)),
    subject.subjectGuid, file.batchID
  )
  dot_shapes <- factor(plot_info[, meta_data])
  p <- ggplot(plot_info, aes(x = subject.subjectGuid, y = file.batchID, group = (!!as.name(meta_data)))) +
    geom_point(alpha = 1, size = 2.5, aes(shape = dot_shapes, colour = factor((!!as.name(meta_data))))) +
    scale_shape_manual(values = c(3, 4, 5, 6, 0, 1, 2, 22, 23, 24, 25)) +
    theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  p <- ggplotly(p, width = width, height = height)
  p <- p %>% toWebGL()
  return(p)
}

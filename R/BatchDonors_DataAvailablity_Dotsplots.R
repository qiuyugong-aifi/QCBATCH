#' Title
#'
#' @param BR1_rna_file
#' @param meta_data
#'
#' @return
#' @export
#'
#' @examples
BatchDonors_DataAvailablity_Dotsplots <- function(rna_dataframe, meta_data = "sample.visitName") {
  plot_info <- rna_dataframe %>%
    dplyr::select((!!as.name(meta_data)), subject.subjectGuid, file.batchID)

  dot_shapes <- factor(plot_info[, meta_data])

  p <- ggplot(plot_info, aes(x = subject.subjectGuid, y = file.batchID, group = (!!as.name(meta_data)))) +
    geom_point(alpha = 8, aes(shape = dot_shapes, size = 120)) +
    scale_shape_manual(values = c(3, 4, 5, 6, 0, 1, 2)) +
    theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  return(p)
}

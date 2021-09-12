#' Title
#'
#' @param combined_dataset
#' @param group
#' @param sample_id
#' @param plotly
#' @param width
#'
#' @return
#' @export
#'
#' @examples
CellNumber_Batch_Boxplot <- function(combined_dataset,width=1000, group = "batch_id", sample_id = "pbmc_sample_id", plotly = "False") {
  if (typeof(combined_dataset) == "S4") {
    combined_dataframe <- combined_dataset[[]]
  } else {
    combined_dataframe <- combined_dataset
  }

  batch_number <- combined_dataframe %>% dplyr::select(
    (!!as.name(group)),
    (!!as.name(sample_id))
  )
  batch_number <- as.data.frame(table(batch_number))
  batch_number <- batch_number %>% dplyr::filter(!Freq == 0)
  if (plotly == "False") {
    p <- ggplot(batch_number, aes(
      x = (!!as.name(group)),
      y = Freq
    )) +
      geom_boxplot() +
      geom_dotplot(
        binaxis = "y",
        stackdir = "center", dotsize = 1
      ) +
      theme(
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20), axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 20)
      )
  } else if (plotly == "True") {
    p <- plot_ly(x = batch_number %>% dplyr::select((!!as.name(group))) %>%
      dplyr::pull(), y = batch_number %>% dplyr::select(Freq) %>%
      pull(), type = "box") %>% plotly::add_trace(
      text = batch_number %>%
        dplyr::select((!!as.name(sample_id))) %>% dplyr::pull(),
      type = "scatter", mode = "markers", marker = list(size = 8),
      showlegend = F,width=width
    )
  }
  p <- p %>% toWebGL()
  return(p)
}

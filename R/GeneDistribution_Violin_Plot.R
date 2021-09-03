#' Title
#'
#' @param combined_dataset
#' @param plotly
#'
#' @return
#' @export
#'
#' @examples
GeneDistribution_Violin_Plot<-function (combined_dataset,plotly = "False") {
  if (plotly == "False") {
    p <- ggplot(combined_dataset[[]], aes(x = batch_id, y = n_genes,
                                          color = batch_id)) + geom_violin(trim = FALSE) + geom_boxplot(width = 0.1)
  }
  else if (plotly == "True") {
    p <- combined_dataset[[]]%>%
      plot_ly(
        x = ~batch_id,
        y =  ~n_genes,
        split = ~batch_id,
        type = 'violin',box = list(visible = T),meanline = list(visible = T))
  }
  return(p)
}

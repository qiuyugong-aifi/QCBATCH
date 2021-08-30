#' Title
#'
#' @param combined_dataset
#'
#' @return
#' @export
#'
#' @examples
GeneDistribution_Violin_Plot<- function(combined_dataset) {



  p<-ggplot(combined_dataset[[]], aes(x=batch_id, y=n_genes, color=batch_id)) +
    geom_violin(trim=FALSE)+geom_boxplot(width=0.1)


  return (p)


}

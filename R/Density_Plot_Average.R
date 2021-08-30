#' Title
#'
#' @param input_density_list
#' @param combined_dataset
#' @param cell_type_label
#'
#' @return
#' @export
#'
#' @examples
Density_Plot_Average <- function(input_density_list, combined_dataset, cell_type_label = "seurat_pbmc_type") {
  myplots <- list()
  Density_list <- input_density_list[1:(length(input_density_list) - 1)]
  for (i in 1:length(Density_list)) {
    Cord_and_meta_data <- cbind(combined_dataset[[]], combined_dataset@reductions$umap@cell.embeddings)

    labels <- Cord_and_meta_data %>%
      dplyr::select(UMAP_1, UMAP_2, (!!as.name(cell_type_label))) %>%
      group_by((!!as.name(cell_type_label))) %>%
      summarise_each(funs(median), UMAP_1, UMAP_2)







    # esitmate lvls range for density plots
    all_range <- NULL
    for (ii in Density_list) {
      all_range <- c(all_range, range(ii$z)[1], range(ii$z)[2])
    }
    lvls <- pretty(range(all_range), 30)

    all_average <- list()
    for (iii in 1:length(Density_list)) {
      density_1 <- Density_list[[1]]

      density <- Density_list
      X <- list()
      for (z in 1:length(density)) {
        X[[z]] <- density[[z]]$z
      }

      Y <- do.call(cbind, X)
      Y <- array(Y, dim = c(dim(X[[1]]), length(X)))
      density_1$z <- apply(Y, c(1, 2), mean, na.rm = TRUE)
    }

    all_average <- density_1
    volcano3d <- reshape2::melt(all_average$z)
    # get coord
    x_cord <- rep(all_average$x, 100)
    y_cord <- NULL

    for (z in all_average$y) {
      y_cord <- c(y_cord, rep(z, 100))
    }


    names(volcano3d) <- c("x", "y", "z")
    volcano3d$UMAP_1 <- x_cord
    volcano3d$UMAP_2 <- y_cord

    myplots <- ggplot(volcano3d, aes(UMAP_1, UMAP_2, z = z)) +
      geom_contour_filled(aes(fill = ..level..), breaks = lvls) +
      scale_fill_gradientn(colours = c("white", "lightblue2", "lightblue4", "blue", "blue1", "blue2", "yellow1", "yellow2", "yellow3", "red1", "red2", "red3", "red4", "darkred"), super = metR::ScaleDiscretised, ) +
      theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), legend.key.size = unit(3, "cm"), legend.text = element_text(size = 15), axis.text = element_text(size = 15), axis.title = element_text(size = 15)) +
      ggtitle("Averaged") +
      annotate("text", x = labels$UMAP_1, y = labels$UMAP_2, label = labels %>% dplyr::select((!!as.name(cell_type_label))) %>% dplyr::pull(), color = "black", size = 6)

    return(myplots)
  }
}

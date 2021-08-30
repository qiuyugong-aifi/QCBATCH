
#' Title
#'
#' @param input_density_list
#'
#' @return
#' @export
#'
#' @examples
Density_Plot_by_Group <- function(input_density_list) {
  myplots <- list()
  Density_list <- input_density_list[1:(length(input_density_list) - 1)]
  for (i in 1:length(Density_list)) {

    # esitmate lvls range for density plots
    all_range <- NULL
    for (ii in Density_list) {
      all_range <- c(all_range, range(ii$z)[1], range(ii$z)[2])
    }
    lvls <- pretty(range(all_range), 30)

    volcano3d <- reshape2::melt(Density_list[[i]]$z)
    # get coord
    x_cord <- rep(Density_list[[i]]$x, 100)
    y_cord <- NULL

    for (z in Density_list[[i]]$y) {
      y_cord <- c(y_cord, rep(z, 100))
    }


    names(volcano3d) <- c("x", "y", "z")
    volcano3d$UMAP_1 <- x_cord
    volcano3d$UMAP_2 <- y_cord

    name_group <- as.character(input_density_list[length(input_density_list)][[1]][i, ])


    myplots[[i]] <- ggplot(volcano3d, aes(UMAP_1, UMAP_2, z = z)) +
      geom_contour_filled(aes(fill = ..level..), breaks = lvls) +
      scale_fill_gradientn(colours = c("white", "lightblue2", "lightblue4", "blue", "blue1", "blue2", "yellow1", "yellow2", "yellow3", "red1", "red2", "red3", "red4", "darkred"), super = metR::ScaleDiscretised, ) +
      theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.key.size = unit(3, "cm"), legend.text = element_text(size = 15), axis.text = element_text(size = 15), axis.title = element_text(size = 15)) +
      ggtitle(paste(name_group, collapse = " "))
  }
  return(myplots)
}

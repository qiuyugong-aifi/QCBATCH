#' Title
#'
#' @param input_density_list
#'
#' @return
#' @export
#'
#' @examples
MeanSubtracted_Density_Plot_by_Group <- function(input_density_list) {
  myplots <- list()
  Density_list <- input_density_list[1:(length(input_density_list) - 1)]

  estimate_lvls <- list()
  for (i in 1:length(Density_list)) {
    estimate_lvls[[i]] <- Density_list[[i]]$z
  }

  lvls <- pretty(range(estimate_lvls), 20)

  if ((length(lvls) - which(lvls == 0, arr.ind = T)) < which(lvls == 0, arr.ind = T)) {
    lvls <- sort(c(lvls[1:(which(lvls == 0, arr.ind = T) - 1)], lvls[which(lvls == 0, arr.ind = T)], -lvls[1:(which(lvls == 0, arr.ind = T) - 1)]))
  } else {
    lvls <- sort(c(-lvls[(which(lvls == 0, arr.ind = T) - 1):length(lvls)], lvls[which(lvls == 0, arr.ind = T)], lvls[(which(lvls == 0, arr.ind = T) - 1):length(lvls)]))
  }
  lvls <- unique(lvls)




  for (i in 1:length(Density_list)) {
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
      scale_fill_gradientn(colours = c("blue3", "blue3", "blue2", "blue1", "lightblue", "white", "white", "pink", "red1", "red2", "red3", "red4"), super = metR::ScaleDiscretised, ) +
      theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_blank(), legend.key.size = unit(3, "cm"), legend.text = element_text(size = 15), axis.text = element_text(size = 15), axis.title = element_text(size = 15)) +
      ggtitle(paste(name_group, collapse = " "))
  }
  return(myplots)
}

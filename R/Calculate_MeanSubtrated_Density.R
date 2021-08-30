#' Title
#'
#' @param input_density_list
#' @param subset_group
#' @param Bridging_Control
#'
#' @return
#' @export
#'
#' @examples
Calculate_MeanSubtrated_Density <- function(input_density_list, subset_group = "sample.visitName", Bridging_Control = "False") {
  info_data_frame <- input_density_list[[length(input_density_list)]]
  average_denisty_by_group <- list()
  average_denisty_by_group_mean_sub <- list()
  new_order_index <- NULL
  if (Bridging_Control == "True") {
    subset_group <- NULL
    Density_list <- input_density_list[1:(length(input_density_list) - 1)]


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
    average_denisty_by_group_mean_sub <- Density_list

    for (ii in 1:length(Density_list)) {
      average_denisty_by_group_mean_sub[[ii]]$z <- Density_list[[ii]]$z - all_average$z
    }

    average_denisty_by_group_mean_sub[[length(average_denisty_by_group_mean_sub) + 1]] <- info_data_frame
  } else if (subset_group == "NULL") {
    Density_list <- input_density_list[1:(length(input_density_list) - 1)]

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
    average_denisty_by_group_mean_sub <- Density_list

    for (ii in 1:length(Density_list)) {
      average_denisty_by_group_mean_sub[[ii]]$z <- Density_list[[ii]]$z - all_average$z
    }

    average_denisty_by_group_mean_sub[[length(average_denisty_by_group_mean_sub) + 1]] <- info_data_frame
  } else {
    for (i in 1:dim(unique(info_data_frame[subset_group]))[1])
    {
      row_index <- strtoi(rownames(split(info_data_frame, info_data_frame[subset_group] %>% pull())[[i]]))
      subset_density_list <- input_density_list[row_index]

      for (iii in 1:length(subset_density_list)) {
        density_1 <- subset_density_list[[1]]

        density <- subset_density_list
        X <- list()

        for (z in 1:length(subset_density_list)) {
          X[[z]] <- subset_density_list[[z]]$z
        }

        Y <- do.call(cbind, X)
        Y <- array(Y, dim = c(dim(X[[1]]), length(X)))
        density_1$z <- apply(Y, c(1, 2), mean, na.rm = TRUE)
        average_denisty_by_group[[i]] <- density_1
        subset_density_list_mean_sub <- list()
        subset_density_list_mean_sub <- subset_density_list

        for (ii in 1:length(subset_density_list)) {
          subset_density_list_mean_sub[[ii]]$z <- subset_density_list[[ii]]$z - density_1$z
        }
      }


      average_denisty_by_group_mean_sub <- c(average_denisty_by_group_mean_sub, subset_density_list_mean_sub)
    }
    average_denisty_by_group_mean_sub[[length(average_denisty_by_group_mean_sub) + 1]] <- bind_rows(split(info_data_frame, info_data_frame[subset_group] %>% pull()), )
  }
  return(average_denisty_by_group_mean_sub)
}

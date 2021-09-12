#' Title
#'
#' @param BR1_rna_file
#' @param filePath
#' @param Bridging_Control
#'
#' @return
#' @export
#'
#' @examples
Merge_Dataset <- function(rna_dataframe, filePath = "filePath", Bridging_Control = "False") {
  file_path <- NULL
  file_pool <- NULL
  for (i in 1:dim(rna_dataframe)[1]) {
    file_path <- c(file_path, paste0("cache/", rna_dataframe$file.id[i], "/", str_split(rna_dataframe$file.name[i], "/")[1][[1]][6]))
    file_pool <- c(file_pool, substr(str_split(rna_dataframe$file.name[i], "/")[[1]][length(str_split(rna_dataframe$file.name[i], "/")[[1]])], 6, 7))
  }
  rna_dataframe$file_pool <- file_pool
  rna_dataframe$file_keep <- paste0(rna_dataframe$file.batchID, "-", rna_dataframe$file_pool)




  if (Bridging_Control == "False") {
    for (i in 1:dim(rna_dataframe)[1]) {
      single_object <- read_h5_seurat(rna_dataframe$filePath[i],
        feature_names = "name"
      )

      if (i == 1) {
        combined <- single_object
      } else {
        combined <- merge(combined, y = single_object, project = "combined")
        rm(single_object)
      }
    }

    if (!grepl("IMM", rna_dataframe$filePath[1], fixed = FALSE)) {
      sample_name <- NULL
      for (i in str_split(rna_dataframe$filePath, "_")) {
        sample_name <- c(sample_name, i[(startsWith(i, "P"))])
      }

      rna_dataframe$pbmc_sample_id <- sample_name

      temp_meta <- left_join(combined[[]], rna_dataframe, by = c("pbmc_sample_id"))

      for (i in colnames(temp_meta)[!c(colnames(temp_meta) %in% colnames(combined[[]]))]) {
        combined@meta.data[, i] <- temp_meta[, i]
      }
    }
  }

  if (Bridging_Control == "True") {
    rna_desc_IMM <- fetch_bridging_control()


    rna_desc_IMM_SUBSET <- rna_desc_IMM %>% filter(file_keep %in% rna_dataframe$file_keep)
    for (i in 1:dim(rna_desc_IMM_SUBSET)[1]) {
      single_object <- read_h5_seurat(rna_desc_IMM_SUBSET$filePath[i],
        feature_names = "name"
      )

      if (i == 1) {
        combined <- single_object
      } else {
        combined <- merge(combined, y = single_object, project = "combined")
        rm(single_object)
      }
    }

    rna_desc_IMM_SUBSET$pool_id <- gsub("_", "-", rna_desc_IMM_SUBSET$file_keep)

    temp_meta <- left_join(combined[[]], rna_desc_IMM_SUBSET, by = c("pool_id"))

    for (i in colnames(temp_meta)[!c(colnames(temp_meta) %in% colnames(combined[[]]))]) {
      combined@meta.data[, i] <- temp_meta[, i]
    }
  }



  return(combined)
}

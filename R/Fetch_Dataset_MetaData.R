#' Title
#'
#' @param rna_dataframe
#' @param filePath
#'
#' @return
#' @export
#'
#' @examples
Fetch_Dataset_MetaData <- function(rna_dataframe, filePath = "filePath") {
  for (i in 1:length(rna_dataframe$sample.visitName)) {
    if (!rna_dataframe[i, ]$sample.visitDetails == "N/A - Flu-Series Timepoint Only") {
      rna_dataframe[i, ]$sample.visitName <- rna_dataframe[i, ]$sample.visitDetails
    }
  }
  rna_dataframe$Donor_Visit_Detail <- paste0(rna_dataframe$subject.subjectGuid, "-", rna_dataframe$sample.visitName)

  for (i in 1:dim(rna_dataframe)[1]) {
    single_object <- read_h5_seurat(rna_dataframe$filePath[i],
      feature_names = "name"
    )
    if (i == 1) {
      combined_meta_data <- single_object[[]]
    } else {
      combined_meta_data <- rbind(combined_meta_data, single_object[[]])
    }
    rm(single_object)
  }
  sample_name <- NULL
  for (i in str_split(rna_dataframe$filePath, "_")) {
    sample_name <- c(sample_name, i[(startsWith(i, "P"))])
  }
  rna_dataframe$pbmc_sample_id <- sample_name
  temp_meta <- left_join(combined_meta_data, rna_dataframe,
    by = c("pbmc_sample_id")
  )
  for (i in colnames(temp_meta)[!c(colnames(temp_meta) %in%
    colnames(combined_meta_data))]) {
    combined_meta_data[, i] <- temp_meta[, i]
  }
  return(combined_meta_data)
}

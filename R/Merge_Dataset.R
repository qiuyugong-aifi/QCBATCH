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
Merge_Dataset<- function(BR1_rna_file,filePath='filePath',Bridging_Control="FALSE") {

  if (Bridging_Control=="FALSE"){
    for (i in 1:dim(BR1_rna_file)[1]){
      single_object <- read_h5_seurat(BR1_rna_file$filePath[i],
                                      feature_names = "name")

      if (i==1){

        combined=single_object
      } else{

        combined<-merge(combined, y =single_object,  project = "combined")
        rm(single_object)}
    }

    if (!grepl( "IMM",rna_desc_IMM$filePath[1],fixed=FALSE)){

      sample_name<-NULL
      for (i in str_split(BR1_rna_file$filePath,'_')){

        sample_name<- c(sample_name,i[(startsWith(i, "P"))])

      }

      BR1_rna_file$pbmc_sample_id<-sample_name

      temp_meta<-left_join(combined[[]],BR1_rna_file,by=c("pbmc_sample_id"))

      for (i in colnames(temp_meta)[!c(colnames(temp_meta) %in% colnames(combined[[]]))]){
        combined@meta.data[,i]<-temp_meta[,i]
      }}


  }

  if (Bridging_Control=="TRUE"){
    rna_desc_IMM_SUBSET<-rna_desc_IMM %>% filter (file.batchID %in% BR1_rna_file$file.batchID)
    for (i in 1:dim(rna_desc_IMM_SUBSET)[1]){

      single_object <- read_h5_seurat(rna_desc_IMM_SUBSET$filePath[i],
                                      feature_names = "name")

      if (i==1){

        combined=single_object
      } else{

        combined<-merge(combined, y =single_object,  project = "combined")
        rm(single_object)}

    }

    rna_desc_IMM_SUBSET$pool_id<-gsub("_", "-", rna_desc_IMM_SUBSET$file_keep)

    temp_meta<-left_join(combined[[]],rna_desc_IMM_SUBSET,by=c("pool_id"))

    for (i in colnames(temp_meta)[!c(colnames(temp_meta) %in% colnames(combined[[]]))]){
      combined@meta.data[,i]<-temp_meta[,i]
    }



  }



  return(combined)
}

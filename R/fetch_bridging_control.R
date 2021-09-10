#' Title
#'
#' @return
#' @export
#'
#' @examples
fetch_bridging_control<-function() {
  filter_list <- list(
    sample.bridgingControl= 'true'
  )

  rna_desc_IMM <- getFileDescriptors(
    fileType = "scRNA-seq-labeled",
    filter = filter_list)
  rna_desc_IMM <- fileDescToDataframe_beta(rna_desc_IMM)

  selected_IMM <- cacheFiles(list(rna_desc_IMM$file.id))
  rna_desc_IMM <- fileDescToDataframe(selected_IMM)


  #fetch info for bridging control samples
  #some bridging control dataset missing some pattern,so cannot be fetched through HISE instance, have to do it manually
  fres2 <- hise::readFiles(list("d96cfdea-1017-4ec0-b737-8fe87a24d486", "13f76644-0ba2-41e3-9075-d1035e6e5b98", "fe3393eb-c6f0-46a7-b70e-f42bd1e13611",
                                "edc63d4e-6feb-4169-ae69-37d6bdb1691d", "8a5a1463-721f-4618-87f5-cc2c4185a311", "1290ac85-40a1-479e-bf1d-f2eacf941aa5",
                                "6c2dea7e-870b-46a1-a356-99821567fdf3", "0d2b8c64-70d7-4990-8f03-05330c98d006", "50cec494-7deb-475a-b97e-cac27d38df4b",
                                "76c91c6a-fa94-4c93-a70a-b3e326756b8a", "e4bab562-4ab5-4df8-8491-3603ac8587e4", "816495b0-73a6-4419-8314-2e704d696be1",
                                "69d367f3-ae46-4e0c-b2ba-556ecc9af770"))
  #get meta data info for this spefific batch
  meta_data_control<-NULL
  for (i in 1:length(fres2)){
    desc<-unlist(fres2[[i]]$descriptors)
    desc <- desc[!grepl("scheme", names(desc))]
    names(desc) <- sub("^descriptors.","",names(desc))

    desc <- as.list(desc)
    desc$labDisplay.version<-NULL
    df <- as.data.frame(desc)

    meta_data_control<-rbind(meta_data_control,df)


  }
  file_path<-NULL
  file_pool<-NULL
  for (i in 1:length(fres2)){
    file_path<-c(file_path,paste0("cache/",meta_data_control$file.id[i],"/",tail(str_split(meta_data_control$file.name[i],'/')[[1]],n=1)[1]))
    file_pool<-c(file_pool,substr(str_split(rna_desc_IMM$file.name[i],'/')[[1]][length(str_split(rna_desc_IMM$file.name[i],'/')[[1]])],6,7))
  }

  meta_data_control$filePath<-file_path
  meta_data_control$file_pool<-file_pool
  meta_data_control$file_keep<-paste0(meta_data_control$file.batchID,"-",meta_data_control$file_pool)


  #fetch info for bridging control samples
  #add additional info into data frame
  file_path<-NULL
  file_pool<-NULL
  for (i in 1:dim(rna_desc_IMM)[1]){
    file_path<-c(file_path,paste0("cache/",rna_desc_IMM$file.id[i],"/",str_split(rna_desc_IMM$file.name[i],'/')[1][[1]][6]))
    file_pool<-c(file_pool,substr(str_split(rna_desc_IMM$file.name[i],'/')[[1]][length(str_split(rna_desc_IMM$file.name[i],'/')[[1]])],6,7))
  }
  rna_desc_IMM$file_pool<-file_pool
  rna_desc_IMM$file_keep<-paste0(rna_desc_IMM$file.batchID,"-",rna_desc_IMM$file_pool)


  rna_desc_IMM<-rbind(rna_desc_IMM,meta_data_control[c(colnames(rna_desc_IMM))])

  duplicated<-rna_desc_IMM%>%  find_duplicates(file_keep)
  duplicated<-duplicated[order(duplicated$file_keep),]

  filepath_to_delete<-c()
  for (i in 1:dim(duplicated)[1]){


    single_sample<-duplicated %>% filter (file_keep==duplicated[i,]$file_keep)
    filepath<-single_sample$filePath
    time<-c()
    for (z in filepath){

      time<-c(time,str_split(str_split(z,"_")[[1]][4],"T")[[1]][1])

    }
    single_sample$time<-time
    filepath_to_delete<-c(filepath_to_delete,single_sample[single_sample$time==min(single_sample$time),]$filePath)




  }
  filepath_to_delete<-unique(filepath_to_delete)
  rna_desc_IMM<-rna_desc_IMM %>% dplyr::filter (!filePath %in% filepath_to_delete)

  return (rna_desc_IMM)}

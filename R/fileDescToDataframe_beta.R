#function to get file meta data for new HISE system
#' Title
#'
#' @param descriptors
#' @param keep_labs
#'
#' @return
#' @export
#'
#' @examples
fileDescToDataframe_beta <- function(descriptors,
                                     keep_labs = FALSE) {
  descriptors<-descriptors[[1]]
  assertthat::assert_that(typeof(descriptors) == "list")
  assertthat::assert_that(typeof(keep_labs) == "logical")

  do.call(
    rbind,
    lapply(
      descriptors,
      function(desc) {
        desc <- unlist(desc)
        desc <- desc[!grepl("scheme", names(desc))]
        names(desc) <- sub("^descriptors.","",names(desc))
        if(!keep_labs) {
          desc <- desc[!grepl("^lab", names(desc))]
        }
        desc <- as.list(desc)
        df <- as.data.frame(desc)
        df
      }
    )
  )
}

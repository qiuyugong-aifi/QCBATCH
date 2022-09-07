#' Title
#'
#' @param descriptors
#' @param keep_labs
#'
#' @return
#' @export
#'
#' @examples

fileDescToDataframe <- function(descriptors,
                                keep_labs = FALSE) {

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
        desc <- desc[!grepl("^specimens", names(desc))]
        desc <- desc[!grepl("^lab", names(desc))]
        desc <- desc[!grepl("^emr", names(desc))]
        desc <- desc[!grepl("^survey", names(desc))]
        desc <- desc[!grepl("^revision", names(desc))]
        desc <- desc[!grepl("^file.userTags", names(desc))]

        desc <- as.list(desc)
        df <- as.data.frame(desc)
        df
      }
    )
  )
}

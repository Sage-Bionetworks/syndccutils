#' Generate manifest template
#' 
#' @param keys Character vector of annotation keys
#' @return A 0-row data frame with column names corresponding to those of a
#'   Synapse manifest
#' @export
#'
#' @examples
#' generate_manifest()
#' 
#' my_keys <- c("study", "assay", "tissue")
#' generate_manifest(my_keys)
generate_manifest <- function(keys = NULL) {
  standard_cols <- c("path", "parent", "name", "used", "executed")
  columns <- unique(c(standard_cols, keys))
  schema <- data.frame(matrix(ncol = length(columns), nrow = 0))
  colnames(schema) <- columns
  schema
}

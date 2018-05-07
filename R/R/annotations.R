#' Get Synapse annotations
#'
#' Download current annotation values from Synapse and provide them as a data
#' frame.
#' 
#' @return A data frame containing all annotation keys, descriptions, column
#'   types, maximum sizes, values, value descriptions, sources, and the name of
#'   the annotation's parent module
#' @export
#' 
#' @examples
#' \dontrun{
#' synLogin()
#' dat <- get_synapse_annotations()
#' }
get_synapse_annotations <- function() {
  queryResult <- synapser::synTableQuery('select * from syn10242922')
  dat <- synapser::as.data.frame(queryResult)
  dat <- dat[, !names(dat) %in% c("ROW_ID", "ROW_VERSION")]
  dat
}


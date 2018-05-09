#' Get Synapse annotations
#'
#' Download current annotation values from Synapse and provide them as a data
#' frame.
#' 
#' @return A data frame containing all annotation keys, descriptions, column
#'   types, maximum sizes, values, value descriptions, sources, and the name of
#'   the annotation's parent module.
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

#' Get all keys associated with a module or set of modules
#'
#' @param module A module or modules to retrieve keys from. If missing, all keys
#'   will be returned.
#' @param source A data frame (such as that returned by
#'   [get_synapse_annotations()]) containing information about annotations.
#'   Should have a `module` column and `key` column.
#' @return A character vector of annotation keys from the specified module(s).
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' get_keys_from_module("sageCommunity")
#' get_keys_from_module(c("sageCommunity", "neuro"))
#' }
get_annotation_keys <- function(module, source) {
  if (missing(source)) {
    source <- get_synapse_annotations()
  }

  if (missing(module)) {
    return(unique(source$key))
  }

  unique(source[source$module %in% module, "key"])
}

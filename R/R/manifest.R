#' Generate manifest template
#' 
#' @param keys A character vector of annotation keys.
#' @return A 0-row data frame with column names corresponding to those of a
#'   Synapse manifest.
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

#' Generate key description table
#'
#' @param table A table of annotation key/value descriptions (e.g. one produced
#'   by [get_synapse_annotations()]).
#' @return A dataframe containing the description of each annotation key in the
#'   table.
#' @export
#' @md
#' 
#' @examples
#' \dontrun{
#' dat <- get_synapse_annotations()
#' generate_key_description(dat)
#' }
generate_key_description <- function(table) {
  table <- unique(table[, c("key", "description", "columnType", "module")])
  table[order(table$module), ]
}

#' Generate value description table
#'
#' Generates a table that describes all of the values an annotation key may take
#' 
#' @inheritParams generate_key_description
#' @return A dataframe containing the description of each annotation value in
#'   the table.
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' dat <- get_synapse_annotations()
#' generate_value_description(dat)
#' }
generate_value_description <- function(table) {
  table <- table[, c("key", "value", "valueDescription", "source", "module")]
  table[order(table$module), ]
}

#' Write manifest to file
#' 
#' @param sheets Sheets to be written into the xlsx file. Can be one data frame,
#'   or a list of data frames.
#' @param file The file to be written to.
#' @export
write_manifest <- function(sheets, file = "annotations_manifest.xlsx") {
  if (!is.data.frame(sheets)) {
    if (!is.list(sheets) | !all(vapply(sheets, is.data.frame, logical(1)))) {
      stop("`sheets` must be a data frame or list of data frames", call. = FALSE)
    }
  }
  openxlsx::write.xlsx(sheets, file)
  invisible(sheets)
}

#' Add new annotations to a file view
#'
#' File annotations can be updated by querying a file view and editing the
#' annotation columns, however adding *new* annotations is less straightforward.
#' `add_fileview_annotations()` streamlines this process. Users can query a file
#' view and add any columns they desire; `add_fileview_annotations()` will
#' detect the column type, create a new column on Synapse, add the column to the
#' file view schema, and then store the user-supplied annotations.
#'
#' @param data Data frame of a file view with additional columns of annotations
#' @param fileview_id Synapse ID of the file view
#' @export
#' @examples
#' \dontrun{
#' library("synapser")
#' synLogin()
#'
#' fv_id <- "syn17020234"
#' fv <- synTableQuery(paste0("SELECT * FROM ", fv_id))
#' fv_data <- as.data.frame(fv)
#'
#' fv_data$new_annotation_1 <- "value1"
#' fv_data$new_annotation_2 <- 5
#' fv_data$new_annotation_3 <- factor("x", "x")
#' fv_data$new_annotation_4 <- c(TRUE, FALSE)
#'
#' add_fileview_annotations(fv_data, fv_id)
#' }
add_fileview_annotations <- function(data, fileview_id) {
  schema <- synapser::synGet(fileview_id)
  fileview <- synapser::synTableQuery(paste0("SELECT * FROM ", fileview_id))
  fileview_df <- synapser::as.data.frame(fileview)
  cols_to_add <- names(data)[!names(data) %in% names(fileview_df)]
  purrr::walk(cols_to_add, function(x) {
    schema$addColumn(
      synapser::Column(
        name = x,
        columnType = detect_column_type(data[[x]])
      )
    )
  })
  synapser::synStore(schema)
  synapser::synStore(synapser::Table(fileview_id, data))
}

detect_column_type <- function(column) {
  switch(
    class(column),
    "character" = "STRING",
    "logical" = "BOOLEAN",
    "integer" = "INTEGER",
    "numeric" = "DOUBLE",
    "factor" = "STRING"
  )
}

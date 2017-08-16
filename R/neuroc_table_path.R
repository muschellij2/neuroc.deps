#' Path to Neuroconductor Remote Commits
#'
#' Gets the correct table if none are specified (supposed to be robust for
#' OSLER)
#' @param table_path Path to the table.  If unspecified, uses defaults
#' @param dev Whether development should be used or not
#'
#' @return Character Vector
#' @export
#'
#' @examples
#' neuroc_table_path(dev = TRUE)
#' neuroc_table_path(dev = FALSE)
#' neuroc_table_path(table_path = "blah")
#' neuroc_table_path(table_path = "blah", dev = TRUE)
neuroc_table_path = function(table_path = NULL, dev = FALSE) {
  if (is.null(table_path)) {
    table_path = "https://neuroconductor.org/neurocPackages"
    if (dev) {
      table_path = "http://neuroconductor.org:8080/neurocPackages"
    }
  }
  return(table_path)
}
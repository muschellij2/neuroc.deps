#' Path to Neuroconductor Remote Commits
#'
#' Gets the correct table if none are specified (supposed to be robust for
#' OSLER)
#' @param table_path Path to the table.  If unspecified, uses defaults
#' @param dev Whether development should be used or not
#' @param user User for the repositories
#'
#' @return Character Vector
#' @export
#'
#' @examples
#' neuroc_table_path(dev = TRUE)
#' neuroc_table_path(dev = FALSE)
#' neuroc_table_path(table_path = "blah")
#' neuroc_table_path(table_path = "blah", dev = TRUE)
#' neuroc_table_path(dev = TRUE, user = "osler")
#' neuroc_table_path(dev = FALSE, user = "osler")
neuroc_table_path = function(
  table_path = NULL,
  dev = FALSE,
  user = c("neuroconductor",
           "osler")) {
  user = match.arg(user)
  if (is.null(table_path)) {
    table_path = switch(
      user,
      neuroconductor = "https://neuroconductor.org/neurocPackages",
      osler = "https://oslerinhealth.org/oslerPackages"
    )
    if (dev) {
      table_path = sub("[.]org/", ".org:8080/", table_path)
      table_path = sub("https://", "http://", table_path)
    }
  }
  return(table_path)
}


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
#' neuroc_table_path(dev = TRUE, user = "oslerinhealth")
#' neuroc_table_path(dev = FALSE, user = "oslerinhealth")
neuroc_table_path = function(
  table_path = NULL,
  dev = FALSE,
  user = NULL) {

  user = neuroc_user(user = user, dev = dev)
  if (is.null(table_path)) {
    table_path = switch(
      user,
      neuroconductor = "https://neuroconductor.org/neurocPackages",
      "neuroconductor-devel" = "http://neuroconductor.org:8080/neurocPackages",
      oslerinhealth = "http://oslerinhealth.org/oslerPackages"
    )
    if (dev) {
      table_path = sub("https://", "http://", table_path)
    }
  }
  return(table_path)
}


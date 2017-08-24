#' Neuroconductor GitHub Username
#'
#' Gets the correct user if none are specified (supposed to be robust for
#' OSLER)
#' @param user GitHub username
#' @param dev Whether development should be used or not
#'
#' @return Character Vector of username
#' @export
#'
#' @examples
#' neuroc_user(user = "neuroconductor")
#' neuroc_user(user = "neuroconductor", dev = TRUE)
#' neuroc_user(user = "neuroconductor-devel")
#' neuroc_user(user = "oslerinhealth")
neuroc_user = function(user = NULL, dev = FALSE) {
  if (is.null(user)) {
    user = Sys.getenv("PACKAGE_NAME")
    if (user == "") {
      user = NULL
    }
  }
  if (is.null(user)) {
    user = "neuroconductor"
    if (dev) {
      user = paste0(user, "-devel")
    }
  }
  return(user)
}
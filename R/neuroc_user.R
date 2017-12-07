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
#' neuroc_user(user = "neuroconductor-devel", dev = TRUE)
#' neuroc_user(dev = TRUE)
#' neuroc_user(user = "oslerinhealth")
#' neuroc_user(user = "oslerinhealth", dev = TRUE)
neuroc_user = function(user = NULL, dev = FALSE) {
  if (is.null(user)) {
    user = Sys.getenv("PACKAGE_NAME")
    if (user == "") {
      user = NULL
    }
  }
  # if (is.null(user)) {
  choices = c("neuroconductor", "oslerinhealth")
  if (dev) {
    choices = c("neuroconductor-devel",  "oslerinhealth")
  }
  user = match.arg(user, choices = choices)
  # }
  return(user)
}

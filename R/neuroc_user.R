#' Neuroconductor GitHub Username
#'
#' Gets the correct user if none are specified (supposed to be robust for
#' OSLER)
#' @param user GitHub username
#' @param dev Whether development should be used or not
#' @param deployment indicator if this is a release, not standard running.
#' Just deployment.
#'
#' @return Character Vector of username
#' @export
#'
#' @examples
#' neuroc_user(user = "neuroconductor")
#' neuroc_user(user = "neuroconductor-releases", deployment = TRUE)
#' neuroc_user(user = "neuroconductor-devel", dev = TRUE)
#' neuroc_user(user = "neuroconductor-devel-releases", dev = TRUE,
#' deployment = TRUE)
#' neuroc_user(dev = TRUE)
#' neuroc_user(user = "oslerinhealth")
#' neuroc_user(user = "oslerinhealth", dev = TRUE)
neuroc_user = function(user = NULL, dev = FALSE, deployment = FALSE) {
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
  if (deployment) {
    choices = paste0(choices, "-releases")
  }
  user = match.arg(user, choices = choices)
  # }
  return(user)
}

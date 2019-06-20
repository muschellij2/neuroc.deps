#' Set PACKAGE_NAME Environment Variable
#'
#' @param dev Development Site vs. not?
#' @param x character of vector
#' @param user User for the repositories
#' @param deployment indicator if this is a release, not standard running.
#' Just deployment.
#'
#'
#' @return Change the PROJECT_NAME to the user
#' @export
set_env_package_name = function(
  x,
  dev = FALSE,
  user = NULL,
  deployment = FALSE) {

  user = neuroc_user(user = user, dev = dev, deployment = deployment)
  x = gsub(
    "PROJECT_NAME=(.*)",
    paste0("PROJECT_NAME=", user),
    x)
  return(x)
}
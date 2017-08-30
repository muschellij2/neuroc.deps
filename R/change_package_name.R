#' Change PACKAGE_NAME
#'
#' @param dev Development Site vs. not?
#' @param x character of vector
#' @param user User for the repositories
#'
#' @return Chnage the PROJECT_NAME to the user
#' @export
change_package_name = function(
  x,
  dev = FALSE,
  user = NULL) {

  user = neuroc_user(user = user, dev = dev)
  x = gsub(
    "PROJECT_NAME=(.*)",
    paste0("PROJECT_NAME=", user),
    x)
  return(x)
}
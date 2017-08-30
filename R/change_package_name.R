#' @rdname use_neuroc_template
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
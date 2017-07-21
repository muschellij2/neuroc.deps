#' Neuroconductor CI Template
#'
#' @param path path to DESCRIPTION file
#' @param ci Which continuous integration system
#' @param ... Additional arguments to pass to
#' \code{\link{neuroc_ci_template_path}}
#'
#' @return File path of YAML file
#' @export
#'
#' @examples
#'
neuroc_ci_template = function(
  path = "DESCRIPTION",
  ci = c("travis", "appveyor"),
  ...) {
  package = read.dcf(file = path, fields = "Package")[1,, drop = TRUE]
  ants = package %in% c("ITKR", "ANTsRCore", "ANTsR")

  neuroc_ci_template_path(ci = ci, ants = ants, ...)
}

#' @rdname neuroc_ci_template
#' @export
neuroc_travis_template = function(...){
  neuroc_ci_template(ci = "travis", ...)
}

#' @rdname neuroc_ci_template
#' @export
neuroc_appveyor_template = function(...){
  neuroc_ci_template(ci = "appveyor", ...)
}
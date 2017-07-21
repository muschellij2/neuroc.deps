#' Neuroconductor CI Template Paths
#'
#' @param ci Which continuous integration system
#' @param ants Does the package depend on ANTs or ANTsR
#' @param ... not used
#'
#' @return File path of YAML file
#' @export
#'
#' @examples
#' neuroc_ci_template()
#' neuroc_appveyor_template()
#' neuroc_ci_template(ants = TRUE)
#' neuroc_travis_template()
#' neuroc_travis_template(ants = TRUE)
#' neuroc_appveyor_template()
#' neuroc_appveyor_template(ants = TRUE)
neuroc_ci_template_path = function(
  ci = c("travis", "appveyor"),
  ants = FALSE) {
  ci = match.arg(ci)

  file = paste0("neuroc_", ci, ifelse(ants, "_ants", ""), ".yml")
  file = system.file(file, package = "neuroc.deps", mustWork = TRUE)

}

#' @rdname neuroc_ci_template_path
#' @export
neuroc_travis_template_path = function(...){
  neuroc_ci_template_path(ci = "travis", ...)
}

#' @rdname neuroc_ci_template_path
#' @export
neuroc_appveyor_template_path = function(...){
  neuroc_ci_template_path(ci = "appveyor", ...)
}


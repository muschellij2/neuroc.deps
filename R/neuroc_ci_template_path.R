#' Neuroconductor CI Template Paths
#'
#' @param ci Which continuous integration system
#' @param ants Does the package depend on ANTs or ANTsR
#' @param user GitHub username for repos
#' @param dev Development Site vs. not?
#' @param deployment indicator if this is a release, not standard running.
#' Just deployment.
#' @param ... not used
#'
#' @return File path of YAML file
#' @export
#'
#' @examples
#' neuroc_ci_template_path()
#' neuroc_appveyor_template_path()
#' neuroc_ci_template_path(ants = TRUE)
#' neuroc_travis_template_path()
#' neuroc_travis_template_path(ants = TRUE)
#' neuroc_appveyor_template_path()
#' neuroc_appveyor_template_path(ants = TRUE)
neuroc_ci_template_path = function(
  ci = c("travis", "appveyor", "travis_pkgdown"),
  ants = FALSE,
  dev = FALSE,
  user = NULL,
  deployment = FALSE,
  ...) {
  ci = match.arg(ci)

  user = neuroc_user(user = user, dev = dev, deployment = deployment)
  user = switch(user,
    "neuroconductor" = "neuroconductor",
    "neuroconductor-devel" = "neuroconductor",
    "neuroconductor-releases" = "neuroconductor",
    "neuroconductor-devel-releases" = "neuroconductor",
    "oslerinhealth" = "oslerinhealth",
    "oslerinhealth-releases" = "oslerinhealth"
  )

  if ((user == "neuroconductor" || user == "oslerinhealth") & ci == "travis_pkgdown") {
    ants = TRUE
  }
  file = paste0(user, "_", ci, ifelse(ants, "_ants", ""), ".yml")
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


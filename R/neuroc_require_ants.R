#' @title ANTsR Dependency
#' @description Returns a logical if binary packages are necessary
#' @param path path to DESCRIPTION file
#' @param verbose print diagnostic messages
#' @inheritParams neuroc_dep_ants
#' @export
neuroc_require_ants = function(
  path = "DESCRIPTION",
  dev = FALSE,
  table_path = NULL,
  release = c("stable", "current"),
  bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
  verbose = TRUE,
  user = NULL,
  deployment = FALSE
  ) {

  desc = desc::description$new(file = path)
  deps = desc$get_deps()
  deps = deps[
    deps$type %in% c("Imports", "Depends", "Suggests"), ,
    drop = FALSE]
  deps = deps$package
  pack_name = desc$get("Package")

  ants_dep = neuroc_dep_ants(
    release = release,
    dev = dev,
    bin_packages = bin_packages,
    table_path = table_path,
    user = user,
    deployment = deployment
  )
  ants = any(deps %in% ants_dep) || any(deps %in% bin_packages)
  ants = ants || any(pack_name %in% bin_packages)
  return(ants)
}

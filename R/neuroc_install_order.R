#' @title Neuroconductor Package Install Order
#' @description Returns the order of Neuroconductor packages installation
#' @return \code{list} of package install order, the dependency matrix,
#' and repos parsed
#' @param release Stable or development version
#' @param dev Development version or not?
#' @param table_path path to table of packages
#' @param user GitHub username for repos
#' @param deployment indicator if this is a release, not standard running.
#' Just deployment
#' @param force should this stop (\code{FALSE}) on missing DESCRIPTION files?
#' Passed to \code{\link{get_repo_dep_mat}}.
#' @export
#'
#' @examples \dontrun{
#' neuroc_install_order()
#' }
#' @importFrom neurocInstall neuro_package_table
neuroc_install_order = function(
  release = c("stable", "current"),
  dev = FALSE,
  deployment = FALSE,
  table_path = NULL,
  user = NULL,
  force = FALSE
){

  dep_mat = neuroc_dep_mat(
    release = release,
    dev = dev,
    table_path = table_path,
    user = user,
    deployment = deployment,
    force = force)
  repos = names(colnames(dep_mat))
  install_ord = install_order(dep_mat)
  L = list(install_order_list = install_ord,
           install_order = unlist(install_ord),
           dep_mat = dep_mat,
           remotes = repos)
  return(L)
}

#' @export
#' @rdname neuroc_install_order
neuroc_dep_mat = function(
  release = c("stable", "current"),
  dev = FALSE,
  table_path = NULL,
  deployment = FALSE,
  user = NULL,
  force = FALSE) {
  #############################
  # Match release
  #############################
  release = match.arg(release)

  user = neuroc_user(user = user, dev = dev, deployment = deployment)
  table_path = neuroc_table_path(
    table_path = table_path,
    dev = dev, user = user,
    deployment = deployment)


  args = list(path = table_path, long = TRUE)
  if ("deployment" %in%
      methods::formalArgs(neurocInstall::neuro_package_table)) {
    args$deployment = deployment
  }
  neuro_deps = do.call(neurocInstall::neuro_package_table, args = args)
  all_neuro_deps = neuro_deps[ neuro_deps$release %in% release, ]
  if (nrow(all_neuro_deps) > 0) {
    all_neuro_deps$remote = paste0(
      user, "/",
      all_neuro_deps$repo)
    run = !is.na(all_neuro_deps$commit) & all_neuro_deps$commit != ""
    all_neuro_deps$remote[run] = paste0(
      all_neuro_deps$remote[run],
      "@", all_neuro_deps$commit[run])
    repos = all_neuro_deps$remote
    dep_mat = get_repo_dep_mat(repos, force = force)
  } else {
    dep_mat = matrix(NA, nrow = 0, ncol = 0)
  }
  return(dep_mat)
}

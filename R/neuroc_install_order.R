#' @title Neuroconductor Package Install Order
#' @description Returns the order of Neuroconductor packages installation
#' @return \code{list} of package install order, the dependency matrix,
#' and repos parsed
#' @param release Stable or development version
#' @param dev Development version or not?
#' @param table_path path to table of packages
#' @param user GitHub username for repos
#' @export
#'
#' @examples \dontrun{
#' neuroc_install_order()
#' }
#' @importFrom neurocInstall neuro_package_table
neuroc_install_order = function(
  release = c("stable", "current"),
  dev = FALSE,
  table_path = NULL,
  user = NULL
){

  dep_mat = neuroc_dep_mat(
    release = release,
    dev = dev,
    table_path = table_path,
    user = user)
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
  user = NULL) {
  #############################
  # Match release
  #############################
  release = match.arg(release)


  table_path = neuroc_table_path(
    table_path = table_path,
    dev = dev, user = user)

  user = neuroc_user(user = user, dev = dev)

  neuro_deps = neuro_package_table(path = table_path, long = TRUE)
  all_neuro_deps = neuro_deps[ neuro_deps$release %in% release, ]
  if (nrow(all_neuro_deps) > 0 ) {
    all_neuro_deps$remote = paste0(user, "/",
                                   all_neuro_deps$repo,
                                   "@", all_neuro_deps$commit)
    repos = all_neuro_deps$remote
    dep_mat = get_repo_dep_mat(repos)
  } else {
    dep_mat = matrix(NA, nrow = 0, ncol = 0)
  }
  return(dep_mat)
}
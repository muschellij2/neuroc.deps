#' @title Neuroconductor Package Install Order
#' @description Returns the order of Neuroconductor packages installation
#' @return \code{list} of package install order, the dependency matrix,
#' and repos parsed
#' @param release Stable or development version
#' @param dev Development version or not?
#' @param table_path path to table of packages
#' @export
#'
#' @examples \dontrun{
#' neuroc_install_order()
#' }
#' @importFrom neurocInstall neuro_package_table
neuroc_install_order = function(
  release = c("stable", "current"),
  dev = FALSE,
  table_path = NULL
){

  dep_mat = neuroc_dep_mat(release = release, dev = dev, table_path = table_path)
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
  table_path = NULL) {
  #############################
  # Match release
  #############################
  release = match.arg(release)


  if (is.null(table_path)) {
    table_path = "https://neuroconductor.org/neurocPackages"
    if (dev) {
      table_path = "http://neuroconductor.org:8080/neurocPackages"
    }
  }
  user = "neuroconductor"
  if (dev) {
    user = paste0(user, "-devel")
  }

  neuro_deps = neuro_package_table(path = table_path, long = TRUE)
  all_neuro_deps = neuro_deps[ neuro_deps$release %in% release, ]

  all_neuro_deps$remote = paste0(user, "/",
                                 all_neuro_deps$repo,
                                 "@", all_neuro_deps$commit
  )
  repos = all_neuro_deps$remote
  dep_mat = get_repo_dep_mat(repos)
  return(dep_mat)
}
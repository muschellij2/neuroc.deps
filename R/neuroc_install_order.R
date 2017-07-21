#' @title Neuroconductor Package Install Order
#' @description Returns the order of Neuroconductor packages installation
#' @return \code{list} of package install order, the dependency matrix,
#' and repos parsed
#' @param path Path to the table of package
#' @param release Stable or development version
#' @export
#'
#' @examples \dontrun{
#' neuroc_install_order()
#' }
#' @importFrom neurocInstall neuro_package_table
neuroc_install_order = function(
  path = "https://neuroconductor.org/neurocPackages",
  release = c("stable", "current")
){
  #############################
  # Match release
  #############################
  release = match.arg(release)

  neuro_deps = neuro_package_table(path = path, long = TRUE)
  all_neuro_deps = neuro_deps[ neuro_deps$release %in% release, ]
  all_neuro_deps$remote = paste0("neuroconductor/",
                                 all_neuro_deps$repo,
                                 "@", all_neuro_deps$commit
  )
  repos = all_neuro_deps$remote

  dep_mat = get_repo_dep_mat(repos)
  install_ord = install_order(dep_mat)
  L = list(install_order_list = install_ord,
           install_order = unlist(install_ord),
           dep_mat = dep_mat,
           remotes = repos)
  return(L)
}
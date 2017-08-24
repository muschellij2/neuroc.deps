#' @title Neuroconductor ANTsR Dependency
#' @description Returns the packages in Neuroconductor that require a
#' set of binary installers
#' @return Character vector of packages
#' @param release Stable or development version
#' @param dev Development version or not?
#' @param bin_packages Packages that require binaries
#' @param table_path path to table of packages
#' @param user GitHub username for repos
#'
#' @export
#'
#' @examples \dontrun{
#' neuroc_dep_ants()
#' }
neuroc_dep_ants = function(
  release = c("stable", "current"),
  dev = FALSE,
  bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
  table_path = NULL,
  user = NULL
){

  dep_mat = neuroc_dep_mat(
    release = release,
    dev = dev,
    table_path = table_path,
    user = user)
  # dep_mat = cbind(dep_mat, fake = FALSE)
  # dep_mat = rbind(dep_mat, fake = FALSE)
  # dep_mat["extrantsr", "fake"] = TRUE


  get_a_dep = function(packs) {
    if (is.null(packs)) {
      return(NULL)
    }
    packs = intersect(packs, rownames(dep_mat))
    if (length(packs) == 0 ) {
      return(NULL)
    }
    names(which(colSums(dep_mat[packs,, drop = FALSE]) > 0))
  }
  # diag(dep_mat) = TRUE
  require_ants = get_a_dep(bin_packages)
  require_ants = sort(unique(c(require_ants, bin_packages)))

  # xreq = require_ants
  dreq = c("test")
  max_iter = 1000
  iter = 1
  # does downstream
  while (length(dreq) > 0) {
    iter = iter + 1
    old_req = require_ants
    require_ants = get_a_dep(require_ants)
    require_ants = sort(unique(c(old_req, require_ants)))
    dreq = setdiff(require_ants, old_req)
    if (iter > max_iter) {
      stop("Infinite loop probably occurring in neuroc_require_ants!")
    }
  }

  return(require_ants)
}

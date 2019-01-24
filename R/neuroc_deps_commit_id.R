#' Get \code{neuroc.deps} commit id
#'
#' @return A character string
#' @export
#'
#' @examples
#' neuroc_deps_commit_id()
neuroc_deps_commit_id = function() {
  df = sessioninfo::session_info("neuroc.deps")
  df = df$packages
  record = df[ df$package == "neuroc.deps", ]
  if (nrow(record) > 1) {
    return(NA)
  }
  src = record$source
  if (!grepl("Git", src)) {
    return(NA)
  }
  ss = strsplit(src, " ")[[1]]
  ss = ss[ grepl("@", ss)]
  ss = strsplit(ss, "@")[[1]]
  ss = ss[length(ss)]
  return(ss)
}
#' Fix DESCRIPTION Remotes field
#'
#' @param file path to DESCRIPTION file
#' @param outfile path to DESCRIPTION output file
#'
#' @return Character path of output file
#' @export
#'
#' @examples
#' tfile = tempfile()
#' path = system.file("bad_remotes_DESCRIPTION",
#' package = "neuroc.deps")
#' fixed = fix_desc_remotes(path, tfile)
fix_desc_remotes = function(file, outfile = file) {
  res = read.dcf(file, all = TRUE, keep.white = FALSE)
  xx = res$Remotes[[1]]
  xx = paste(xx, collapse = ",\n")
  res = read.dcf(file, all = TRUE)
  res$Remotes = xx
  write.dcf(res, file = outfile)
  return(outfile)
}

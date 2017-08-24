#' Make Example Package for testing
#'
#' Returns a path with a DESCRIPTOIN file in there
#' @param package packages in \code{neuroc.deps} that have an example
#'
#' @return List of a directory and a DESCRIPTION file
#' @export
#'
#' @examples
#' example_package(package = "cifti")
example_package = function(
  package = c("extrantsr", "cifti", "spotgear")
  ) {
  tfile = tempfile()
  dir.create(tfile)
  outfile = file.path(tfile, "DESCRIPTION")

  package = match.arg(package)
  file = paste0(package, "_DESCRIPTION")
  file = system.file(
    file,
    package = "neuroc.deps",
    mustWork = TRUE)
  file.copy(file, outfile)

  L = list(
    directory = tfile,
    description_file = outfile
  )
  return(L)
}
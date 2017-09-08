#' Make Example DESCRIPTION file for testing
#'
#' Returns a path with a DESCRIPTION file in there
#' @param package packages in \code{neuroc.deps} that have an example
#'
#' @return List of a directory and a DESCRIPTION file
#' @export
#'
#' @examples
#' neuroc_example_description(package = "cifti")
neuroc_example_description = function(
  package = c("extrantsr", "cifti", "spotgear", "ITKR", "mimosa")
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



#' Make Example travis file
#'
#' Returns a path with a travis file in there
#' @param package packages in \code{neuroc.deps} that have an example
#'
#' @return List of a directory and a travis file
#' @export
#'
#' @examples
#' neuroc_example_travis(package = "mimosa")
neuroc_example_travis = function(
  package = c("mimosa")
) {
  tfile = tempfile()
  dir.create(tfile)
  outfile = file.path(tfile, ".travis.yml")

  package = match.arg(package)
  file = paste0(package, "_travis.yml")
  file = system.file(
    file,
    package = "neuroc.deps",
    mustWork = TRUE)
  file.copy(file, outfile)

  L = list(
    directory = tfile,
    travis_file = outfile
  )
  return(L)
}
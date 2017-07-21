#' Use Neuroconductor CI Template
#'
#' @param path path to DESCRIPTION file
#' @param ci Which continuous integration system
#' @param dev Development Site vs. not?
#' @param ... Additional arguments to pass to
#' \code{\link{neuroc_ci_template_path}}
#'
#' @return Copy the template to the current directory
#' @export
use_neuroc_template = function(
  path = "DESCRIPTION",
  ci = c("travis", "appveyor"),
  dev = FALSE,
  ...) {


  ci = match.arg(ci)



  template_file = neuroc_ci_template(path = path, ci = ci, ...)

  outfile = switch(
    ci,
    travis = ".travis.yml",
    appveyor = "appveyor.yml")
  if (!file.exists(outfile)) {
    bak = paste0(outfile, ".bak")
    file.copy(outfile, bak)
  }
  file.copy(infile, outfile, overwrite = TRUE)
}

#' @rdname use_neuroc_template
#' @export
use_neuroc_travis_template = function(...){
  use_neuroc_template(ci = "travis", ...)
}

#' @rdname use_neuroc_template
#' @export
use_neuroc_appveyor_template = function(...){
  use_neuroc_template(ci = "appveyor", ...)
}

#' @rdname use_neuroc_template
#' @export
neuroc_key = function(
  ci = c("travis", "appveyor"),
  dev = FALSE) {

  ci = match.arg(ci)
  outdev = ""
  if (dev) {
    outdev = "DEVEL_"
  }
  key_val = paste0("NEUROC_", outdev, toupper(ci), "_KEY")
  key = Sys.getenv(key_val)
  if (is.na(key) || key == "") {
    stop(paste0(key_val, " not set!"))
  }

  return(key)
}

#' @rdname use_neuroc_template
#' @export
add_neuroc_keys = function(
  template_file,
  ci = c("travis", "appveyor"),
  dev = FALSE) {

  key = neuroc_key(
    ci = ci,
    dev = dev
    )

  tfile = tempfile()
  file.copy(template_file, tfile)
  x = readLines(tfile)
  if (dev) {
    x = gsub(
      "ants_user=neuroconductor",
      "ants_user=neuroconductor-devel",
      x)
  }
  x = gsub("${NEUROCKEY}", key, x, fixed = TRUE)
  return(x)
}
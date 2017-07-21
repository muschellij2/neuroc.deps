#' Use Neuroconductor Template
#'
#' @param path path to DESCRIPTION file
#' @param ci Which continuous integration system
#' @param dev Development Site vs. not?
#' @param ... Additional arguments to pass to
#' \code{\link{neuroc_ci_template_path}}
#' @param template_file file to template
#' @param table_path Path to the table of packages for neuroconductor
#' @param release Stable or development version
#'
#' @return Copy the template to the current directory
#' @export
use_neuroc_template = function(
  path = "DESCRIPTION",
  ci = c("travis", "appveyor"),
  dev = FALSE,
  table_path = "https://neuroconductor.org/neurocPackages",
  release = c("stable", "current"),
  ...) {


  if (dev) {
    table_path = sub("^https", "http", table_path)
    table_path = sub("[.]org/", ".org:8080/", table_path)
  }

  # overwriting description file
  new_desc = neuroc_desc(path = path,
                         table_path = table_path,
                         release = release)
  bak = paste0(path, ".bak")
  file.copy(path, bak)
  file.copy(new_desc, path, overwrite = TRUE)


  outfiles = rep(NA, length = length(ci))
  names(outfiles) = ci
  for (ici in ci) {
    template_file = neuroc_ci_template(path = path, ci = ici, ...)
    template = add_neuroc_keys(template_file,
                               ci = ici,
                               dev = dev)
    outfile = switch(
      ici,
      travis = ".travis.yml",
      appveyor = "appveyor.yml")
    if (!file.exists(outfile)) {
      bak = paste0(outfile, ".bak")
      file.copy(outfile, bak)
    }
    writeLines(text = template, con = outfile)
    outfiles[ici] = outfile
  }
  return(outfiles)
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
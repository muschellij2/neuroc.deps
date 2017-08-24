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
#' @param bin_packages Binaries packages required
#' @param verbose print diagnostic messages
#' @param user User for the repositories
#'
#' @return Copy the template to the current directory
#' @export
#' @importFrom utils download.file
use_neuroc_template = function(
  path = "DESCRIPTION",
  ci = c("travis", "appveyor"),
  dev = FALSE,
  table_path = NULL,
  release = c("stable", "current"),
  bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
  verbose = TRUE,
  user = c("neuroconductor",
           "oslerinhealth"),
  ...) {

  if (!file.exists(path)) {
    stop(paste0("File passed into path argument: ", path,
                "does not exist!"))
  }
  pkg_directory = dirname(path)

  table_path = neuroc_table_path(
    table_path = table_path,
    dev = dev,
    user = user)

  if (grepl("^http", table_path)) {
    if (verbose) {
      message("Downloading Table of packages")
    }
    destfile = tempfile(fileext = ".txt")
    download.file(url = table_path, destfile = destfile,
                  quiet = !verbose)
    table_path = destfile
  }

  if (verbose) {
    message("Rewriting DESCRIPTION")
  }
  # overwriting description file
  new_desc = neuroc_desc(
    path = path,
    table_path = table_path,
    release = release,
    dev = dev,
    verbose = verbose,
    user = user)
  bak = paste0(path, ".bak")
  file.copy(path, bak, overwrite = TRUE)
  file.copy(new_desc, path, overwrite = TRUE)

  if (verbose) {
    message(paste0("Checking Dependencies against ",
                   paste(bin_packages, collapse = ", "), " required binaries")
    )
  }
  desc = description$new(file = path)
  # Look at ANTsR
  deps = desc$get_deps()
  deps = deps[
    deps$type %in% c("Imports", "Depends", "Suggests"), ,
    drop = FALSE]
  deps = deps$package

  ants_dep = neuroc_dep_ants(
    release = release,
    dev = dev,
    bin_packages = bin_packages,
    table_path = table_path,
    user = user
  )
  ants = any(deps %in% ants_dep) || any(deps %in% bin_packages)

  if (verbose) {
    message(paste0("Writing CI files")
    )
  }
  outfiles = rep(NA, length = length(ci))
  names(outfiles) = ci
  for (ici in ci) {
    # template_file = neuroc_ci_template(path = path, ci = ici, ...)

    template_file = neuroc_ci_template_path(
      ci = ici,
      ants = ants,
      user = user)
    template = add_neuroc_keys(
      template_file,
      ci = ici,
      dev = dev,
      user = user)
    outfile = switch(
      ici,
      travis = ".travis.yml",
      appveyor = "appveyor.yml")
    outfile = file.path(pkg_directory, outfile)
    if (file.exists(outfile)) {
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
  dev = FALSE,
  user = c("neuroconductor", "oslerinhealth")) {

  ci = match.arg(ci)
  outdev = ""
  if (dev) {
    outdev = "DEVEL_"
  }
  user = match.arg(user)
  prefix = switch(
    user,
    neuroconductor = "NEUROC",
    oslerinhealth = "OSLER")
  key_val = paste0(prefix, "_", outdev, toupper(ci), "_KEY")
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
  dev = FALSE,
  user = c("neuroconductor",
           "oslerinhealth")) {

  key = neuroc_key(
    ci = ci,
    dev = dev
  )

  tfile = tempfile()
  file.copy(template_file, tfile)
  x = readLines(tfile)
  user = match.arg(user)

  if (dev) {
    x = gsub(
      paste0("ants_user=", user),
      paste0("ants_user=", user, "-devel"),
      x)
  }
  x = gsub("${NEUROCKEY}", key, x, fixed = TRUE)
  return(x)
}


#' @rdname use_neuroc_template
#' @export
#' @param keep_packs Packages to keep when removing all neuroc packages
#' @importFrom utils installed.packages read.csv remove.packages
remove_neuroc_packages = function(
  ...,
  keep_packs = c("neuroc.deps", "ghtravis", "neurocInstall")) {

  table_path = neuroc_table_path(...)
  tab = utils::read.csv(table_path, as.is = TRUE)
  rm_packs = tab[,1]
  rm_packs = setdiff(rm_packs, keep_packs)
  check = rm_packs %in% utils::installed.packages()
  if (any(check)) {
    rm_packs = rm_packs[check]
    utils::remove.packages(rm_packs)
  }
  return(invisible(NULL))
}

#' Use Neuroconductor Template
#'
#' @param path path to DESCRIPTION file
#' @param ci Which continuous integration system
#' @param dev Development Site vs. not?
#' @param ... Additional arguments to pass to
#' \code{\link{extract_ci_fields}}
#' @param table_path Path to the table of packages for neuroconductor
#' @param release Stable or development version
#' @param bin_packages Binaries packages required
#' @param verbose print diagnostic messages
#' @param user User for the repositories
#' @param merge_ci Should CI fields be merged?  If so,
#' \code{\link{extract_ci_fields}} is called to grab the relevant fields.
#' Fields cannot currently be a list
#'
#' @return Copy the template to the current directory
#' @export
#' @importFrom utils download.file
#' @importFrom yaml yaml.load as.yaml
#' @importFrom usethis use_build_ignore
use_neuroc_template = function(
  path = "DESCRIPTION",
  ci = c("travis", "appveyor"),
  dev = FALSE,
  table_path = NULL,
  release = c("stable", "current"),
  bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
  verbose = TRUE,
  user = NULL,
  merge_ci = FALSE,
  ...) {

  if (!file.exists(path)) {
    stop(paste0("File passed into path argument: ", path,
                "does not exist!"))
  }
  pkg_directory = dirname(path)
  user = neuroc_user(user = user, dev = dev)

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
  ####################
  # Adding build ignoring
  ####################
  usethis::use_build_ignore(
    basename(bak),
    base_path = pkg_directory
  )
  file.copy(new_desc, path, overwrite = TRUE)

  if (verbose) {
    message(
      paste0("Checking Dependencies against ",
             paste(bin_packages, collapse = ", "),
             " required binaries")
    )
  }
  ants = neuroc_require_ants(
    path = path,
    dev = dev,
    table_path = table_path,
    release = release,
    bin_packages = bin_packages,
    verbose = verbose,
    user = user)

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
    template = set_env_package_name(
      template,
      dev = dev,
      user = user)
    outfile = switch(
      ici,
      travis = ".travis.yml",
      appveyor = "appveyor.yml")
    ####################
    # Adding build ignoring
    ####################
    usethis::use_build_ignore(outfile, base_path = pkg_directory)
    outfile = file.path(pkg_directory, outfile)
    if (file.exists(outfile)) {
      bak = paste0(outfile, ".bak")
      file.copy(outfile, bak, overwrite = TRUE)

      ####################
      # Adding build ignoring
      ####################
      usethis::use_build_ignore(bak, base_path = pkg_directory)

      ############################
      # Merging CI fields
      ############################
      if (merge_ci) {
        template = merge_ci_fields(file = outfile, template = template, ...)
      }
      ############################
      # End Merging CI fields
      ############################
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
  user = NULL) {

  ci = match.arg(ci)
  outdev = ""
  if (dev) {
    outdev = "DEVEL_"
  }
  user = neuroc_user(
    user = user,
    dev = dev)
  prefix = switch(
    user,
    neuroconductor = "NEUROC",
    "neuroconductor-devel" = "NEUROC",
    oslerinhealth = "OSLER")
  key_val = paste0(prefix, "_", outdev, toupper(ci), "_KEY")
  key = Sys.getenv(key_val)
  if (is.na(key) || key == "") {
    stop(paste0(key_val, " not set!"))
  }

  return(key)
}

#' @rdname use_neuroc_template
#' @param template_file file to template
#' @export
add_neuroc_keys = function(
  template_file,
  ci = c("travis", "appveyor"),
  dev = FALSE,
  user = NULL) {

  user = neuroc_user(user = user, dev = dev)

  key = neuroc_key(
    ci = ci,
    dev = dev,
    user = user
  )

  tfile = tempfile()
  file.copy(template_file, tfile)
  suppressWarnings({
    x = readLines(tfile)
  })

  x = gsub(
    "ants_user=(.*)",
    paste0("ants_user=", user),
    x)
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

get_remote_table_path = function(table_path, verbose = TRUE) {
  if (grepl("^http", table_path)) {
    if (verbose) {
      message("Downloading Table of packages")
    }
    destfile = tempfile(fileext = ".txt")
    dl = download.file(url = table_path, destfile = destfile,
                       quiet = !verbose)
    if (dl != 0 && grepl("^https", table_path)) {
      table_path = sub("https", "http", table_path)
      destfile2 = tempfile(fileext = ".txt")
      dl = download.file(url = table_path, destfile = destfile2,
                         quiet = !verbose)
      if (dl == 0) {
        destfile = destfile2
      }
    }
    table_path = destfile
  }
  table_path
}

copy_and_ignore_file = function(ci, user, dev, deployment, pkg_directory,
                                outfile = NULL) {

  template_file = neuroc_ci_template_path(
    ci = ci,
    ants = FALSE,
    user = user,
    dev = dev,
    deployment = deployment)
  if (is.null(outfile)) {
    outfile = basename(template_file)
  }
  ####################
  # Adding build ignoring
  ####################
  usethis::use_build_ignore(outfile)
  outfile = file.path(pkg_directory, outfile)
  if (file.exists(outfile)) {
    bak = paste0(outfile, ".bak")
    file.copy(outfile, bak, overwrite = TRUE)

    ####################
    # Adding build ignoring
    ####################
    usethis::use_build_ignore(bak)
  }
  file.copy(template_file, outfile, overwrite = TRUE)
  outfile
}

#' Use Neuroconductor Template
#'
#' @param path path to DESCRIPTION file
#' @param ci Which continuous integration system
#' @param dev Development Site vs. not?
#' @param ... Additional arguments to pass to
#' \code{\link{extract_ci_fields}}
#' @param table_path Path to the table of packages for neuroconductor
#' @param release Stable or development version
#' @param verbose print diagnostic messages
#' @param user User for the repositories
#' @param deployment indicator if this is a release, not standard running.
#' Just deployment.
#' @param force should this stop (\code{FALSE}) on missing DESCRIPTION files?
#' Passed to \code{\link{get_repo_dep_mat}}.
#' @param fix_remotes Run \code{\link{fix_desc_remotes}} on the DESCRIPTION
#' file before running.
#'
#' @return Copy the template to the current directory
#' @export
#' @importFrom utils download.file
#' @importFrom yaml yaml.load as.yaml
#' @importFrom usethis use_build_ignore
use_neuroc_template = function(
  path = "DESCRIPTION",
  dev = FALSE,
  table_path = NULL,
  release = c("stable", "current"),
  verbose = TRUE,
  user = NULL,
  deployment = FALSE,
  force = deployment,
  fix_remotes = TRUE,
  ...) {

  if (!file.exists(path)) {
    stop(paste0("File passed into path argument: ", path,
                " does not exist!"))
  }
  pkg_directory = dirname(path)
  owd = getwd()
  on.exit({
    setwd(owd)
  })
  setwd(pkg_directory)
  user = neuroc_user(user = user, dev = dev, deployment = deployment)

  if (verbose) {
    message(paste0("User is set to ", user))
  }
  table_path = neuroc_table_path(
    table_path = table_path,
    dev = dev,
    user = user,
    deployment = deployment)

  table_path = get_remote_table_path(table_path, verbose)

  # if (verbose) {
  #   message("Getting System Requirements")
  # }
  # sreq = system_requirements(path = path)
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
    user = user,
    deployment = deployment,
    force = force,
    fix_remotes = fix_remotes)
  bak = paste0(path, ".bak")
  file.copy(path, bak, overwrite = TRUE)
  ####################
  # Adding build ignoring
  ####################
  usethis::use_build_ignore(
    basename(bak)
  )
  file.copy(new_desc, path, overwrite = TRUE)

  if (verbose) {
    message("Removing workflows")
  }
  workflow_dir <- file.path(pkg_directory, ".github", "workflows")
  dir.create(workflow_dir, showWarnings = FALSE, recursive = TRUE)
  x = list.files(path = workflow_dir, pattern = ".y(a|)ml",
                 ignore.case = TRUE, full.names = TRUE)
  if (length(x) > 0) file.remove(x)

  if (verbose) {
    message("Writing CI files")
  }
  usethis::use_build_ignore(".github")
  yaml_file = copy_and_ignore_file(
    ci = "autoci",
    user, dev, deployment,
    workflow_dir,
    outfile = NULL)
  pkgdown_yaml_file = copy_and_ignore_file(
    ci = "autoci_pkgdown",
    user, dev, deployment,
    workflow_dir,
    outfile = NULL)
  tic_file = copy_and_ignore_file(
    ci = "tic",
    user, dev, deployment,
    pkg_directory,
    outfile = "tic.R")

  outfiles = c(yaml_file, tic_file, pkgdown_yaml_file)
  n_commit_id = neuroc_deps_commit_id()

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
  release = FALSE,
  user = NULL,
  deployment = FALSE) {

  ci = match.arg(ci)
  outdev = ""
  if (dev) {
    outdev = "DEVEL_"
  }
  if (deployment) {
    outdev = paste0(outdev,"RELEASE_")
  }
  user = neuroc_user(
    user = user,
    dev = dev,
    deployment = deployment)
  prefix = switch(
    user,
    neuroconductor = "NEUROC",
    "neuroconductor-devel" = "NEUROC",
    "neuroconductor-devel-releases" = "NEUROC",
    "neuroconductor-releases" = "NEUROC",
    "oslerinhealth-releases" = "OSLER",
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
  user = NULL,
  deployment = FALSE) {

  user = neuroc_user(user = user, dev = dev, deployment = deployment)

  key = neuroc_key(
    ci = ci,
    dev = dev,
    user = user,
    deployment = deployment
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

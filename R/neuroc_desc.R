#' Neuroc DESCRIPTION File
#'
#' @param path path to DESCRIPTION file
#' @param table_path Path to the table of packages for neuroconductor
#' @param release Stable or development version
#' @param dev Development Site vs. not?
#' @param verbose print diagnostic messages
#' @param user GitHub username for repos
#' @param deployment indicator if this is a release, not standard running.
#' Just deployment.
#' @param force should this stop (\code{FALSE}) on missing DESCRIPTION files?
#' Passed to \code{\link{get_repo_dep_mat}}.
#' @return Path to new DESCRIPTION file
#' @export
#' @details
#' The function will parse a description, delete any remotes for packages in
#' neuroconductor, add the neuroconductor remotes (in order),
#' adds \code{biocViews} field (for Travis), puts the \code{covr} in Suggests for
#' code coverage if not already in Imports/Suggests/Depends,
#' replaces the SystemRequirements field with the suggested value(s) from
#' https://goo.gl/x7rcCD
#'
#' @importFrom desc description
#' @importFrom tools toTitleCase
neuroc_desc = function(
  path = "DESCRIPTION",
  table_path = NULL,
  release = c("stable", "current"),
  dev = FALSE,
  verbose = TRUE,
  user = NULL,
  deployment = FALSE,
  force = !deployment
){

  release = match.arg(release)

  #############################################
  # Get the installation order for Neuroc
  #############################################
  user = neuroc_user(user = user, dev = dev, deployment = deployment)

  table_path = neuroc_table_path(
    table_path = table_path,
    dev = dev,
    user = user,
    deployment = deployment)
  if (verbose) {
    msg = paste0("Getting Installation Order")
    message(msg)
  }
  ord = neuroc_install_order(
    table_path = table_path, release = release,
    dev = dev, user = user, deployment = deployment,
    force = force)
  ord_packs = ord$install_order



  #############################################
  # subset the df for the stable/current
  #############################################
  all_neuro_deps = neurocInstall::neuro_package_table(
    path = table_path, long = TRUE)

  #############################################
  # reorder this so it's in order (easier later)
  #############################################
  all_neuro_deps = all_neuro_deps[
    all_neuro_deps$release %in% release, , drop = FALSE]
  rownames(all_neuro_deps) = all_neuro_deps$repo
  all_neuro_deps = all_neuro_deps[ord_packs, ,
                                  drop = FALSE]
  rownames(all_neuro_deps) = NULL

  #############################################
  # Make proper remote
  #############################################
  if (nrow(all_neuro_deps) > 0) {
    all_neuro_deps$remote = paste0(
      user, "/",
      all_neuro_deps$repo)
    run = !is.na(all_neuro_deps$commit) & all_neuro_deps$commit != ""
    all_neuro_deps$remote[run] = paste0(all_neuro_deps$remote[run],
                                        "@", all_neuro_deps$commit[run])
  }

  #############################################
  # Parse Description
  #############################################
  if (verbose) {
    msg = paste0("Parsing DESCRIPTION file")
    message(msg)
  }
  # Fixes the multiple suggests
  desc = read.dcf(file = path, all = TRUE)
  desc = dcf_collapser(desc, cn = c("Imports", "Suggests", "Depends"))
  write.dcf(x = desc, file = path)

  desc = desc::description$new(file = path)


  #############################################
  # Grab the remotes
  #############################################
  if (verbose) {
    msg = paste0("Reading Remotes")
    message(msg)
  }
  get_the_remotes = function() {
    remotes = desc$get_remotes()
    if (length(remotes) == 0) {
      remotes = NULL
    }
    return(remotes)
  }
  remotes = get_the_remotes()
  desc$del("Remotes")
  if (!is.null(remotes)) {
    # getting the package name - same as repo because neuroc
    #
    dcfs = ghtravis::get_remote_package_dcf(remotes = remotes)
    get_pack = function(tmp) {
      if (!file.exists(tmp)) {
        return(NA)
      }
      desc = read.dcf(file = tmp, all = TRUE)
      desc = dcf_collapser(desc, cn = c("Imports", "Suggests", "Depends"))
      tfile = tempfile()
      write.dcf(x = desc, file = tfile)
      d = desc::description$new(file = tfile)
      unname(d$get("Package"))
    }
    packs = sapply(dcfs, get_pack)
    na_packs = is.na(packs)
    # fixing issue with commit that went awry and is gone
    if (any(na_packs)) {
      bad_rem = remotes[na_packs]
      bad_rem = sapply(bad_rem, function(x) {
        gsub("(.*)@.*", "\\1", x )
      })
      bad_rem = sapply(bad_rem, function(x) {
        gsub("(.*)#.*", "\\1", x )
      })
      bad_rem = strsplit(bad_rem, "/")
      bad_rem = sapply(bad_rem, function(x) {
        x[length(x)]
      })
      packs[na_packs] = bad_rem
    }
    names(remotes) = packs
    drop_repos = packs %in% all_neuro_deps$repo
    remotes = remotes[ !drop_repos]

  }

  desc$del("Repository")
  repository = tools::toTitleCase(user)
  desc$set(Repository = repository)

  # Look at ANTsR
  deps = desc$get_deps()
  deps = deps[ deps$type %in% c("Imports", "Depends", "Suggests"), ,
               drop = FALSE]
  deps = deps$package

  #########################################
  # Coverage
  #########################################
  if (verbose) {
    msg = paste0("Adding Code Coverage")
    message(msg)
  }
  run_suggests = c("covr", "testthat")
  for (isug in run_suggests) {
    if (!desc$has_dep(isug)) {
      desc$set_dep(package = isug, type = "Suggests")
    }
  }


  neuro_deps = all_neuro_deps[ all_neuro_deps$repo %in% deps, , drop = FALSE]
  # run if have neuroconductor dependencies
  if (nrow(neuro_deps) > 0) {
    if (verbose) {
      msg = paste0("Writing Neuroc Remotes")
      message(msg)
    }
    ## added for missing remotes
    run = !is.na(neuro_deps$commit) & neuro_deps$commit != ""
    add_remotes = paste0(user, "/",
                         neuro_deps$repo)
    add_remotes[run] =  paste0(add_remotes[run],
                               "@", neuro_deps$commit[run])
    remotes = c(remotes, add_remotes)
  }
  # Fixes Github issue #82
  if (length(remotes) > 0) {
    desc$add_remotes(remotes = remotes)
  }


  if (verbose) {
    msg = paste0("Adding biocViews")
    message(msg)
  }
  bc_views = desc$get("biocViews")
  if (is.na(bc_views)) {
    desc$set(biocViews = "")
  }

  # read and reformat SystemRequirements (if needed)
  if (verbose) {
    msg = paste0("Adjust SystemRequirements")
    message(msg)
  }

  sysreqs_df = read.csv('https://goo.gl/x7rcCD',
                        stringsAsFactors = FALSE)
  rownames(sysreqs_df) <- sysreqs_df$Package
  pkg = unname(desc$get("Package"))
  if (pkg %in% sysreqs_df$Package) {
    new_sysreq = sysreqs_df[pkg,]$Recommended.System.Requirements
    if (!is.na(new_sysreq) & new_sysreq != "") {
      desc$set(SystemRequirements = new_sysreq)
    }
  }

  out_path = tempfile()
  desc$write(out_path)

  return(out_path)

}

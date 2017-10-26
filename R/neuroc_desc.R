#' Neuroc DESCRIPTION File
#'
#' @param path path to DESCRIPTION file
#' @param table_path Path to the table of packages for neuroconductor
#' @param release Stable or development version
#' @param dev Development Site vs. not?
#' @param verbose print diagnostic messages
#' @param user GitHub username for repos
#'
#' @return Path to new DESCRIPTION file
#' @export
#' @details
#' The function will parse a description, delete any remotes for packages in
#' neuroconductor, add the neuroconductor remotes (in order),
#' adds \code{biocViews} field (for Travis), puts the \code{covr} in Suggests for
#' code coverage if not already in Imports/Suggests/Depends,
#' replaces the SystemRequirements field with the suggested value(s) from
#' https://goo.gl/x7rcCD
#' @importFrom desc description
neuroc_desc = function(
  path = "DESCRIPTION",
  table_path = NULL,
  release = c("stable", "current"),
  dev = FALSE,
  verbose = TRUE,
  user = NULL
){

  release = match.arg(release)

  #############################################
  # Get the installation order for Neuroc
  #############################################
  if (verbose) {
    msg = paste0("Getting Installation Order")
    message(msg)
  }
  user = neuroc_user(user = user, dev = dev)

  table_path = neuroc_table_path(
    table_path = table_path,
    dev = dev,
    user = user)
  ord = neuroc_install_order(
    table_path = table_path, release = release,
    dev = dev, user = user)
  ord_packs = ord$install_order


  #############################################
  # subset the df for the stable/current
  #############################################
  all_neuro_deps = neurocInstall::neuro_package_table(
    path = table_path, long = TRUE)

  #############################################
  # reorder this so it's in order (easier later)
  #############################################
  all_neuro_deps = all_neuro_deps[ all_neuro_deps$release %in% release, ,
                                   drop = FALSE]
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
      all_neuro_deps$repo,
      "@", all_neuro_deps$commit)
  }

  #############################################
  # Parse Description
  #############################################
  if (verbose) {
    msg = paste0("Parsing DESCRIPTION file")
    message(msg)
  }
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
      d = desc::description$new(file = tmp)
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
  if (!desc$has_dep("covr")) {
    desc$set_dep(package = "covr", type = "Suggests")
  }


  neuro_deps = all_neuro_deps[ all_neuro_deps$repo %in% deps, , drop = FALSE]
  # run if have neuroconductor dependencies
  if (nrow(neuro_deps) > 0) {
    if (verbose) {
      msg = paste0("Writing Neuroc Remotes")
      message(msg)
    }
    add_remotes = paste0(user, "/",
                         neuro_deps$repo,
                         "@", neuro_deps$commit
    )
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

  sysreqs_df = read.csv('https://goo.gl/x7rcCD',stringsAsFactors = FALSE)
  rownames(sysreqs_df) <- sysreqs_df$Package
  if (unname(desc$get("Package")) %in% sysreqs_df$Package) {
    new_sysreq = sysreqs_df[unname(desc$get("Package")),]$Recommended.System.Requirements
    if (!is.na(new_sysreq) & new_sysreq != "") {
        desc$set(SystemRequirements = new_sysreq)
    }
  }

  out_path = tempfile()
  desc$write(out_path)

  return(out_path)

}

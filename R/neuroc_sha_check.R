
#' Check SHA consistency of the system
#'
#' @inheritParams neuroc_install_order
#' @param drop_packages Packages to drop from consistency check
#' @param verbose print diagnostic messages
#'
#' @return A list of \code{data.frame}s of information of
#' dependency commits and the level of dependency
#' @export
#'
#' @examples
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#' library(dplyr)
#' L = neuroc_sha_check(release = "stable", dev = FALSE,
#' drop_packages = "rtapas", verbose = TRUE)
#' xdf = L$original_df
#' bad_packages = xdf$original_package[
#' any(is.na(xdf$original_commit) | xdf$original_commit == "")]
#' res = L$dependency_df
#' res = res %>%
#'   arrange(original_package, level, remote_package)
#'
#' res = res %>%
#'   group_by(remote_package) %>%
#'   mutate(overall_n_commit = length(unique(remote_commit_id))) %>%
#'   ungroup
#'
#' res = res %>%
#'   group_by(original_package, remote_package) %>%
#'   mutate(n_commit = length(unique(remote_commit_id)))
#'
#' res = right_join(xdf %>%
#'                    select(original_package, original_commit_id),
#'                  res)
#' bad = res %>%
#'   select(-level) %>%
#'   distinct() %>%
#'   filter(n_commit > 1 | overall_n_commit > 1)
#' }
neuroc_sha_check = function(
  release = c("stable", "current"),
  dev = FALSE,
  deployment = FALSE,
  table_path = NULL,
  user = NULL,
  drop_packages = NULL,
  verbose = FALSE
){

  #############################
  # Match release
  #############################
  release = match.arg(release)

  user = neuroc_user(user = user, dev = dev, deployment = deployment)
  table_path = neuroc_table_path(
    table_path = table_path,
    dev = dev, user = user,
    deployment = deployment)


  neuro_deps = neurocInstall::neuro_package_table(path = table_path, long = TRUE)
  df = neuro_deps[ neuro_deps$release %in% release, ]


  df = df[, c("repo", "commit_id")]
  xdf = df
  colnames(df)[ colnames(df) == "repo"] = "pkg"

  colnames(xdf)[ colnames(xdf) == "repo"] = "original_package"
  colnames(xdf)[ colnames(xdf) == "commit_id"] = "original_commit_id"

  df = cbind(xdf, df)
  missing_cid = df$commit_id %in% "" | is.na(df$commit_id)
  if (any(missing_cid)) {
    ddd = df[missing_cid,  ]
    warning(paste0("No commit IDs: ", paste(ddd$pkg, collapse = ", ")))
  }
  df$commit_id[missing_cid] = "master"

  df$remotes = ""
  if (!is.null(drop_packages)) {
    df = df[ !df$original_package %in% drop_packages,]
  }

  n_levels = 10

  xdf = df
  original_df = xdf

  df = xdf
  level_data = vector(mode = "list", length = n_levels)
  names(level_data) = 1:n_levels

  for (ilevel in 1:n_levels) {
    if (verbose) {
      message(paste0("Looking at level: ", ilevel, " dependencies"))
    }
    res_df = get_all_remotes(df, verbose = verbose, user = user)
    res_df = res_df[ grepl(paste0("^", user), res_df$remotes),]
    if (nrow(res_df) == 0) break
    if (nrow(res_df) > 0 & ilevel == n_levels) {
      warning("More than 10 levels deep of dependencies, stopping")
    }
    res_df$level = ilevel
    all_remotes = split_remote(res_df$remotes, user = user)
    res_df = cbind(res_df, all_remotes)
    res_df = res_df[ which(res_df$remote_package != ""),]
    level_data[[ilevel]] = res_df
    df = res_df[, c("remote_package", "remote_commit_id", "original_package")]
    colnames(df)[ colnames(df) == "remote_package"] = "pkg"
    colnames(df)[ colnames(df) == "remote_commit_id"] = "commit_id"
    rownames(df) = NULL
    df = unique(df)
    Sys.sleep(2)
  }

  res = do.call(rbind, level_data)
  rownames(res) = NULL
  L = list(original_df = original_df,
       dependency_df = res)
  return(L)
}
#

#' @export
#' @rdname neuroc_sha_check
neuroc_bad_sha = function(...) {
  L = neuroc_sha_check(...)
  original_package = level = remote_package = remote_commit_id = NULL
  rm(list = c("original_package", "level", "remote_package",
              "remote_commit_id"))
  overall_n_commit = n_commit = original_commit_id = NULL
  rm(list = c("overall_n_commit", "n_commit", "original_commit_id"))

  if (requireNamespace("dplyr", quietly = TRUE)) {
    xdf = L$original_df
    res = L$dependency_df
    bad = any(is.na(xdf$original_commit) | xdf$original_commit == "")
    bad_packages = NULL
    if (any(bad)) {
      bad_packages = xdf$original_package[bad]
      message(paste0("No commit IDs: ", paste(bad_packages, collapse = ", ")))
    }
    res = res %>%
      dplyr::arrange(original_package, level, remote_package)

    res = res %>%
      dplyr::group_by(remote_package) %>%
      dplyr::mutate(overall_n_commit = length(unique(remote_commit_id))) %>%
      dplyr::ungroup()

    res = res %>%
      dplyr::group_by(original_package, remote_package) %>%
      dplyr::mutate(n_commit = length(unique(remote_commit_id)))

    res = dplyr::right_join(
      xdf %>%
        dplyr::select(original_package, original_commit_id),
      res)
    bad = res %>%
      dplyr::select(-level) %>%
      dplyr::distinct() %>%
      dplyr::filter(n_commit > 1 | overall_n_commit > 1)
    attr(bad, "bad_packages") = bad_packages
  } else {
    stop("dplyr is required for neuroc_bad_sha")
  }
  return(bad)
}




##########################################
# Crazy utilites
###########################################






remote_desc = function(pkg, commit, user = "neuroconductor") {
  url = file.path("https://raw.githubusercontent.com",
                  user,
                  pkg,
                  commit,
                  "DESCRIPTION")
  tfile = tempfile()
  res = httr::GET(url, httr::write_disk(path = tfile))
  httr::stop_for_status(res)
  return(tfile)
}


get_commits = function(pkg, user = "neurocondcutor") {
  new_repo <- gh::gh(paste0(
    "GET /repos/", user, "/",
    pkg, "/commits")
  )
  return(new_repo)
}


remote_remotes =  function(pkg, commit, user = "neuroconductor") {
  tfile = remote_desc(pkg, commit, user)
  desc = desc::description$new(tfile)
  x = desc$get_remotes()
  if (length(x) == 0) {
    x = ""
  }
  x
}

split_remote = function(x, user = "neuroconductor") {
  x = sub(paste0(user, "/"), "", x)
  x = strsplit(x, "@")
  x = lapply(x, function(x) {
    if (length(x) == 0) {
      x = c("", "")
    }
    x
  })
  x = do.call(rbind, x)
  colnames(x) = c("remote_package", "remote_commit_id")
  as.data.frame(x, stringsAsFactors = FALSE)
}

get_all_remotes = function(df, verbose = FALSE, user = "neuroconductor") {
  res = vector(mode = "list", length = nrow(df))
  names(res) = df$original_package
  if (verbose & !(verbose > 1)) {
    pb = utils::txtProgressBar(min = 0, max = nrow(df))
  }
  for (iid in seq(nrow(df))) {
    idf = df[iid,]
    pkg = idf$pkg
    commit = idf$commit_id
    if (verbose > 1) {
      message(paste0(pkg, ", commit: ", commit))
    }
    dd = remote_remotes(pkg, commit, user = user)
    dd = data.frame(remotes = dd, stringsAsFactors = FALSE)
    dd$pkg = pkg
    dd$original_package = idf$original_package
    res[[iid]] = dd
    if (verbose & !(verbose > 1)) {
      utils::setTxtProgressBar(pb, value = iid)
    }
  }
  if (verbose & !(verbose > 1)) {
    close(pb)
  }
  res_df = do.call(rbind, res)
  return(res_df)
}


# taken from devtools
parse_git_repo = function(path) {
  username_rx <- "(?:([^/]+)/)?"
  repo_rx <- "([^/@#]+)"
  subdir_rx <- "(?:/([^@#]*[^@#/]))?"
  ref_rx <- "(?:@([^*].*))"
  pull_rx <- "(?:#([0-9]+))"
  release_rx <- "(?:@([*]release))"
  ref_or_pull_or_release_rx <- sprintf("(?:%s|%s|%s)?", ref_rx,
                                       pull_rx, release_rx)
  github_rx <- sprintf("^(?:%s%s%s%s|(.*))$", username_rx,
                       repo_rx, subdir_rx, ref_or_pull_or_release_rx)
  param_names <- c("username", "repo", "subdir", "ref", "pull",
                   "release", "invalid")
  replace <- stats::setNames(sprintf("\\\\%d", seq_along(param_names)),
                             param_names)
  params <- lapply(replace, function(r) gsub(github_rx, r,
                                             path, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid git repo: %s", path))
  params <- params[sapply(params, nchar) > 0]
  if (!is.null(params$pull)) {
    params$ref <- NA
    params$pull <- NULL
  }
  if (!is.null(params$release)) {
    params$ref <- NA
    params$release <- NULL
  }
  params
}
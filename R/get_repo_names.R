#' @title Get Repository names
#' @description Simple extractor of the \code{names} field from the
#' GitHub repository
#' @param ... arguments to be passed to \code{\link{get_all_repos}}
#'
#' @return Vector of repository names
#' @export
get_repo_names = function(...) {
  repos = get_all_repos(...)
  sapply(repos, `[[`, "name")
}
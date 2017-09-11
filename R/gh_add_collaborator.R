#' Add GitHub Collaborator
#'
#' @param repo Repository Name, in the form of user/repo
#' @param username Username to add as a collaborator
#' @param .token Github personal authentication token (PAT)
#' @param ... additional arguments to \code{\link{gh}}
#'
#' @return Object of class \code{gh_response}
#' @export
#'
#' @examples \dontrun{
#' gh_add_collaborator(
#' repo = "neuroconductor/ANTsR",
#' username = "muschellij2")
#' }
#'
gh_add_collaborator = function(
  repo = "neuroconductor/ANTsR",
  username = "muschellij2",
  .token = NULL,
  ...
){
  if (is.null(.token)) {
    .token = github_pat()
  }
  endpoint = paste0("PUT /repos/", repo, "/collaborators/", username)
  res = gh(endpoint = endpoint, .token = .token, ...)
  return(res)
}

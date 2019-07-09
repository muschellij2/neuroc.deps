#' @title Get Install Order of Repositories
#' @description Get
#' @param repos Repositories to find the install order
#' @param dep_type Dependency types to check against
#' @param force should this stop (\code{FALSE}) on missing DESCRIPTION files?
#' @return List of dependency order
#' @export
#'
#' @examples
#' \dontrun{
#' repos = get_repo_names(username = "neuroconductor")
#' repos = paste0("neuroconductor/", repos)
#' dep_mat = get_repo_dep_mat(repos)
#' ord1 = install_order(dep_mat)
#' order = get_repo_install_order(repos)
#' }
#' @importFrom igraph graph_from_adjacency_matrix degree
#' @importFrom ghtravis remote_package_dcf split_remotes get_remote_package_dcf
get_repo_dep_mat = function(
  repos,
  dep_type =  c("Depends", "Imports",
                "Suggests"),
  force = FALSE) {

  dcfs = ghtravis::get_remote_package_dcf(
    repos,
    url = "https://api.github.com")

  info = lapply(dcfs, function(tmp) {
    if (is.na(tmp)) {
      L = list(Package = NA,
               Version = NA)
    } else {
      L = ghtravis::read_dcf(tmp)$dcf
    }
    return(L)
  })

  pkgs = sapply(info, function(x) {
    x$Package
  })
  bad_pack = is.na(pkgs)
  if (any(bad_pack)) {
    bad_repo = repos[bad_pack]
    msg = paste0("Repos: ", paste(bad_repo, collapse = ", "),
                 "have no DESCRIPTION file for the package name!")
    if (force) {
      stop_func = base::stop
    } else {
      stop_func = base::warning
    }
    stop_func(msg)
    pkgs[bad_pack] = sapply(
      ghtravis::parse_remotes(repos[bad_pack]),
      `[[`, "repo")
  }

  dep_mat = sapply(info, function(xx) {
    run_pack = xx$Package
    # print(run_pack)
    grab = names(xx) %in% dep_type
    if (any(grab)) {
      res = xx[grab]
      res = lapply(res, function(x) {
        x = unlist(x)
        x = paste(x, collapse = ", ")
        if (length(x) > 0) {
          x = strsplit(x, " ")[[1]]
          return(ghtravis::split_remotes(x))
        } else {
          return("")
        }
      })

      all_deps = unique(unlist(res))
      tab = (pkgs %in% all_deps)
      names(tab) = pkgs
      return(tab)
    } else {
      tab = rep(FALSE, length = length(pkgs))
    }
  })
  if (length(repos) == 1) {
    dep_mat = matrix(dep_mat, nrow = 1, ncol = 1)
    rownames(dep_mat) = pkgs
  }
  colnames(dep_mat) = pkgs

  return(dep_mat)
}

#' @rdname get_repo_dep_mat
#' @export
#' @param dep_mat Logial Dependency matrix
install_order = function(dep_mat) {
  pkgs = colnames(dep_mat)

  ograph = igraph::graph_from_adjacency_matrix(
    dep_mat,
    mode = "directed")

  neuro_deps = pkgs
  install_order = list()
  i = 1
  while (length(neuro_deps) > 0) {
    graph = igraph::graph_from_adjacency_matrix(
      dep_mat,
      mode = "directed")

    outs = igraph::degree(graph, mode = "in")
    installer = names(outs)[outs == 0]
    install_order = c(install_order,
                      list(installer))
    # no_dep = names(deg)[deg == 0]

    keep = !(neuro_deps %in% installer)
    dep_mat = dep_mat[keep, keep, drop = FALSE]
    neuro_deps = neuro_deps[keep]
    i = i + 1
    if (i > 200) {
      stop("something is wrong")
    }
  }

  # list_install_order = install_order
  return(install_order)
}

#' @rdname get_repo_dep_mat
#' @export
get_repo_install_order = function(
  repos,
  dep_type =
    c("Depends", "Imports",
      "Suggests")) {
  dep_mat = get_repo_dep_mat(repos = repos, dep_type = dep_type)
  ord = install_order(dep_mat = dep_mat)
  return(ord)
}

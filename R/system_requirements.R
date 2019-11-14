# taken from https://github.com/r-lib/desc/blob/master/R/deps.R
parse_deps <- function(type, deps) {
  deps <- trimws(strsplit(deps, ",")[[1]])
  deps <- lapply(strsplit(deps, "\\("), trimws)
  deps <- lapply(deps, sub, pattern = "\\)$", replacement = "")
  res <- data.frame(
    stringsAsFactors = FALSE,
    type = if (length(deps)) type else character(),
    package = vapply(deps, "[", "", 1),
    version = vapply(deps, "[", "", 2)
  )
  res[ is.na(res) ] <- "*"
  res
}


system_requirements = function(path = "DESCRIPTION") {
  desc = desc::description$new(file = path)
  ss = desc$get("SystemRequirements")
  if (all(is.na(ss))) {
    return(NULL)
  }
  ss = unlist(strsplit(ss, split = "\n"))
  ss = paste(ss, collapse = " ")
  ss = unlist(strsplit(ss, split = "\t"))
  ss = paste(ss, collapse = " ")
  deps = parse_deps("SystemRequirements", ss)
}
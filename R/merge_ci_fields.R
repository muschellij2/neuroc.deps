#' Merge CI fields
#'
#' @param file File to extract fields from
#' @param template Character string of template to merge with
#' @param ... options to \code{\link{extract_ci_fields}}
#'
#' @return Character String of template
#' @export
#'
#' @examples
#'     template_file = neuroc_ci_template_path(
#'     ci = "travis",
#'     ants = FALSE,
#'     user = "neuroconductor")
#'     template = readLines(template_file)
#'     file = neuroc_example_travis(package = "mimosa")
#'     file = file$travis_file
#'     out = merge_ci_fields(file = file, template = template)
#'
merge_ci_fields = function(file, template, ...) {
  fields = extract_ci_fields(file, ...)
  if (length(fields) > 0) {
    #  make into yaml
    template = paste(template, collapse = "\n")
    template = yaml::yaml.load(string = template,
                               as.named.list = TRUE)
    nfields = names(fields)
    #merge all the fields (if it's a list fail)
    for (ifield in nfields) {
      tfield = template[[ifield]]
      if (is.list(tfield)) {
        stop(paste0("Field:", ifield,
                    "is a list and cannot be currently merged"))
      }
      # make sure no redundant so unique
      # this may be a problem with logicals
      tfield = unique(c(tfield, fields[[ifield]]))
      template[[ifield]] = tfield
    }
    # revert back to string
    template = yaml::as.yaml(
      template,
      indent = 2,
      indent.mapping.sequence = TRUE)
    template = strsplit(template, "\n")[[1]]
  }

  return(template)
}
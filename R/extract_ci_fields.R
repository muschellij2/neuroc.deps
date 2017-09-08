
#' Extract field from Continuous Integration YAML
#'
#' @param file YAML file to read in
#'
#' @param fields Fields to extract from YAML
#' @return Either NULL or a list of fields
#' @export
#'
#' @importFrom yaml yaml.load_file
#' @examples \dontrun{
#' extract_ci_fields(file = ".travis.yml")
#' }
extract_ci_fields = function(file, fields = c("r_binary_packages")) {
  if (!file.exists(file)) {
    return(NULL)
  }
  yaml = yaml::yaml.load_file(file)
  fields = intersect(names(yaml), fields)
  res = yaml[fields]
  if (length(res) == 0) {
    return(NULL)
  }
  return(res)
}
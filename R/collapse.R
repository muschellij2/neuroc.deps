#' Fixes the multiple suggests
#' @param desc dcf file
#' @param cn Dependencies tags
#'
#' @return A dcf structure with collapsed dependecies
#' @export
collapser = function(desc, cn) {
  for (icn in cn) {
    if (icn %in% colnames(desc)) {
      x = desc[, icn]
      x = unlist(x)
      x = paste(x, collapse = ", ")
      desc[, icn] = x
    }
  }
  return(desc)
}
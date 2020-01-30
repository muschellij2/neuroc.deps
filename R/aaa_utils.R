in_ci = function () {
  nzchar(Sys.getenv("CI"))
}

github_pat = function (quiet = FALSE) {
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(pat)) {
    if (!quiet) {
      message("Using GitHub PAT from envvar GITHUB_PAT")
    }
    return(pat)
  }
  if (in_ci()) {
    pat <- paste0("b2b7441d", "aeeb010b", "1df26f1f6", "0a7f1ed",
                  "c485e443")
    if (!quiet) {
      message(paste0("Using bundled GitHub PAT. Please add ",
                     "your own PAT to the env var `GITHUB_PAT`"))
    }
    return(pat)
  }
  return(NULL)
}

dcf_collapser = function(desc, cn) {
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



change_yaml_field = function(template, field_name, field_value) {
  if (!is.null(field_value)) {
    template = gsub("(\\s*)on:(\\s*)$", "\\1'on':\\2", template)
    #  make into yaml
    template = paste(template, collapse = "\n")
    template = yaml::yaml.load(string = template,
                               as.named.list = TRUE)
    template[[field_name]] = field_value
    # revert back to string
    template = yaml::as.yaml(
      template,
      indent = 2,
      indent.mapping.sequence = TRUE)
    template = strsplit(template, "\n")[[1]]
  }

  return(template)
}
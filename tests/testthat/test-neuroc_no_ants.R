context("testing use_neuroc")

test_that("checking non-ants Neuroc Package", {

  L = neuroc.deps::neuroc_example_description(package = "cifti")

  table_path = neuroc_table_path(user = "neuroconductor")
  destfile = tempfile(fileext = ".txt")
  x = try({
    download.file(url = table_path, destfile = destfile,
                  quiet = TRUE)
  })
  if (inherits(x, "try-error") || x != 0) {
    httr::GET(table_path, httr::write_disk(destfile, overwrite = TRUE),
              config = httr::config(ssl_verifypeer = FALSE))
  }
  table_path = destfile
  Sys.setenv("NEUROC_TRAVIS_KEY" = "asdfdsdf")
  Sys.setenv("NEUROC_APPVEYOR_KEY" = "asdfdsdf")

  tester = function(object) {
    # testthat::expect_that(object, !testthat::gives_warning())
    testthat::expect_failure(expect_warning(object))
    testthat::expect_failure(expect_error(object))
  }

  # testthat::expect_silent({
  #   use_neuroc_template(
  #     path = L$description_file,
  #     table_path = table_path,
  #     user = "neuroconductor",
  #     dev = FALSE,
  #     verbose = FALSE)
  # })

  ##############################################
  # Testing ANTsR Dep
  ##############################################
  L = neuroc.deps::neuroc_example_description(package = "extrantsr")

  tester({
    out = use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE)
  })

  testthat::expect_true(file.exists(file.path(L$directory, ".travis.yml")))

  tester({
    out = use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      os_to_run = c("linux"),
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE)
  })
  res = yaml::yaml.load_file(out[["travis"]])
  testthat::expect_equal(res$os, "linux")


  L = neuroc.deps::neuroc_example_description(package = "ITKR")

  tester({
    neuroc_require_ants(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
      verbose = FALSE)
  })

  # tester({
  #   neuroc_require_ants(
  #     path = L$description_file,
  #     table_path = table_path,
  #     user = "neuroconductor",
  #     dev = FALSE,
  #     bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
  #     verbose = FALSE)
  # })
  #

  L = neuroc.deps::neuroc_example_description(package = "mimosa")
  travis = neuroc.deps::neuroc_example_travis(package = "mimosa")
  out_travis_file = file.path(L$directory, ".travis.yml")
  file.copy(travis$travis_file,
            out_travis_file,
            overwrite = TRUE)

  tester(function() {
    result = use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE,
      merge_ci = TRUE)
    result
  })


  L = neuroc.deps::neuroc_example_description(package = "mimosa")

  tester(function() {
    result = use_neuroc_template(
      path = L$description_file,
      ci = "travis_pkgdown",
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE,
      merge_ci = TRUE)
    result
  })



})

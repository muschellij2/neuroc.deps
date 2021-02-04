testthat::context("testing use_neuroc")

testthat::test_that("checking non-ants Neuroc Package", {

  L = neuroc.deps::neuroc_example_description(package = "cifti")

  table_path = neuroc_table_path(user = "neuroconductor")
  destfile = tempfile(fileext = ".txt")
  x = try({
    download.file(url = table_path, destfile = destfile, mode = "wb",
                  quiet = TRUE)
  })
  if (inherits(x, "try-error") || x != 0) {
    httr::GET(table_path, httr::write_disk(destfile, overwrite = TRUE),
              config = httr::config(ssl_verifypeer = FALSE))
  }
  table_path = destfile


  tester = function(object) {
    # testthat::expect_that(object, !testthat::gives_warning())
    testthat::expect_failure(testthat::expect_warning(object))
    testthat::expect_failure(testthat::expect_error(object))
  }

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

  testthat::expect_true(file.exists(file.path(L$directory, "tic.R")))
  testthat::expect_true(file.exists(file.path(L$directory,
                                              ".github", "workflows", "autoci.yml")))


  L = neuroc.deps::neuroc_example_description(package = "ITKR")

  tester({
    neuroc_require_ants(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE)
  })

})

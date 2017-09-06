test_that("checking non-ants Neuroc Package", {

  L = neuroc.deps::example_package(package = "cifti")

  table_path = neuroc_table_path(user = "neuroconductor")
  destfile = tempfile(fileext = ".txt")
  download.file(url = table_path, destfile = destfile,
                quiet = TRUE)
  table_path = destfile
  Sys.setenv("NEUROC_TRAVIS_KEY" = "asdfdsdf")
  Sys.setenv("NEUROC_APPVEYOR_KEY" = "asdfdsdf")

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
  L = neuroc.deps::example_package(package = "extrantsr")

  testthat::expect_silent({
    use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE)
  })


  L = neuroc.deps::example_package(package = "ITKR")

  testthat::expect_true({
    neuroc_require_ants(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
      verbose = FALSE)
  })


})
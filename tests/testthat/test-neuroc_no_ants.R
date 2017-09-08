test_that("checking non-ants Neuroc Package", {

  L = neuroc.deps::neuroc_example_description(package = "cifti")

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
  L = neuroc.deps::neuroc_example_description(package = "extrantsr")

  testthat::expect_silent({
    use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE)
  })


  L = neuroc.deps::neuroc_example_description(package = "ITKR")

  testthat::expect_true({
    neuroc_require_ants(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      bin_packages = c("ITKR", "ANTsR", "ANTsRCore"),
      verbose = FALSE)
  })


  L = neuroc.deps::neuroc_example_description(package = "mimosa")
  travis = neuroc.deps::neuroc_example_travis(package = "mimosa")
  out_travis_file = file.path(L$directory, ".travis.yml")
  file.copy(travis$travis_file,
            out_travis_file,
            overwrite = TRUE)

  testthat::expect_silent({
    result = use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE,
      merge_ci = TRUE)
  })



})
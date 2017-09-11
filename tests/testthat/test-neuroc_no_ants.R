test_that("checking non-ants Neuroc Package", {

  L = neuroc.deps::neuroc_example_description(package = "cifti")

  table_path = neuroc_table_path(user = "neuroconductor")
  destfile = tempfile(fileext = ".txt")
  download.file(url = table_path, destfile = destfile,
                quiet = TRUE)
  table_path = destfile
  Sys.setenv("NEUROC_TRAVIS_KEY" = "asdfdsdf")
  Sys.setenv("NEUROC_APPVEYOR_KEY" = "asdfdsdf")

  tester = function(object) {
    testthat::expect_that(object, testthat::not(testthat::gives_warning()))
    testthat::expect_that(object, testthat::not(testthat::throws_error()))
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
    use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE)
  })


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

  tester({
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

  tester({
    result = use_neuroc_template(
      path = L$description_file,
      table_path = table_path,
      user = "neuroconductor",
      dev = FALSE,
      verbose = FALSE,
      merge_ci = TRUE)
  })



})

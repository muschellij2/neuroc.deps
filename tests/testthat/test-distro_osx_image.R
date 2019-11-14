context("testing different OSX")

test_that("OSX Image and distribution change", {

  table_path = neuroc_table_path(user = "neuroconductor")
  destfile = tempfile(fileext = ".txt")
  download.file(url = table_path, destfile = destfile,
                quiet = TRUE)
  table_path = destfile
  Sys.setenv("NEUROC_TRAVIS_KEY" = "asdfdsdf")
  Sys.setenv("NEUROC_APPVEYOR_KEY" = "asdfdsdf")

  L = neuroc.deps::neuroc_example_description(package = "cifti")
  res <- use_neuroc_template(
    path = L$description_file,
    table_path = table_path,
    linux_distribution = "xenial",
    osx_image = "xcode10")
  travis = res[["travis"]]
  out = yaml::read_yaml(travis)

  testthat::expect_equal(out$dist, "xenial")
  testthat::expect_equal(out$osx_image, "xcode10")
})

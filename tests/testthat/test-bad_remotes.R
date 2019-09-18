context("testing bad remotes")

test_that("bad remotes file", {
  path = system.file("bad_remotes_DESCRIPTION",
                     package = "neuroc.deps")
  tfile = tempfile()
  testthat::expect_error(desc::description$new(path))
  fixed = fix_desc_remotes(path, tfile)
  testthat::expect_silent(desc::description$new(fixed))
})
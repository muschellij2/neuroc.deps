# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks(args=c("--as-cran","--install-args=--build"))

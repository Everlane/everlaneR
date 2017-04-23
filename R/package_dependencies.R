# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

install_packages_if <- function(package_name, lib_path) {
  if ( !package_name %in% rownames(installed.packages()) ) {
    if ( missing(lib_path) ) {
      install.packages(package_name)
    }
    else {
      install.packages(package_name, lib = temp,
                       repos = "http://cran.us.r-project.org")
    }
  }
  else {
    message (paste0(package_name, " is already installed!"))
  }
}

package_dependecy_playbook <- function(additional_packages) {
  .libPaths(c(.libPaths(), temp <- tempdir()))
  packages <- c("dply", "plyr", "reshape2" , "ggplot2", "RPostgreSQL", "lubridate",
                "scales", "tidyr", "zoo", "lazyeval", "devtools")

  if (!missing(additional_packages)) {
    packages <- c(packages, additional_packages)
  }

  for (package in packages) {
    install_packages_if(package_name = package, lib_path = temp)
  }

  require(devtools)
  withr::with_libpaths(new = temp, install_github("cloudyr/aws.s3"))
}



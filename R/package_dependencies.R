# easy package dependency management for deploying code to remote servers
#
# basic idea is to have a package installation playbook that installs common
# packages to a temp folder on the cloud server.
#

# function for quickly checking if package is already installed
############################################################
install_packages_if <- function(package_name, lib_path) {
  # check if package is installed
  if ( !package_name %in% rownames(installed.packages()) ) {

    # optional arugment lib_path- can specify specific libpath to install to
    # if missing, install to default dir
    if ( missing(lib_path) ) {
      install.packages(package_name)
    }
    else {
      install.packages(package_name, lib = temp,
                       repos = "http://cran.us.r-project.org")
    }
  }
  else { # if package is already installed, return message
    message (paste0(package_name, " is already installed!"))
  }
}

# package installation playbook
############################################################
package_dependecy_playbook <- function(additional_packages) {
  .libPaths(c(.libPaths(), temp <- tempdir())) # add temp directory to libpaths (R looks at these paths when loading packages)

  # vector of commonly used packages
  packages <- c("dply", "plyr", "reshape2" , "ggplot2", "RPostgreSQL", "lubridate",
                "scales", "tidyr", "zoo", "lazyeval", "devtools")

  # optional argument of manually appending additional packages to install
  if (!missing(additional_packages)) {
    packages <- c(packages, additional_packages)
  }

  # install packages to temp dir
  for (package in packages) {
    install_packages_if(package_name = package, lib_path = temp)
  }

  # install packages from github
  require(devtools)
  withr::with_libpaths(new = temp, install_github("cloudyr/aws.s3"))
}



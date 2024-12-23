# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

# make sure to install the latest version of the app
library(devtools)
devtools::install_local('.',force=T)

# quaddata and localtree seem to sometimes not get set after install 
# which causes the app to crash upon running the analysis
if(!exists("quaddata")) {
  EJAM:::dataload_from_local(varnames = "quaddata")
}
localtree <- SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")

library(shinytest2)
library(testthat)
library(EJAM)

# this is the main function that does the test commands
source("tests/app-functionality.R")

# filter to only shiny tests
# test_check("EJAM") # this runs all the tests
test_app(".", filter="shiny-functionality")
# This is what ensures tests are run during  R CMD check,
#   which you can start via  check() (i.e., build then do ⁠R CMD check)
# check() automatically builds and checks a source package, using all known best practices. 
# check_built() checks an already-built package.
# Passing ⁠R CMD check⁠ is essential if you want to submit your package to CRAN: you must not have any ERRORs or WARNINGs, and you want to ensure that there are as few NOTEs as possible. If you are not submitting to CRAN, at least ensure that there are no ERRORs or WARNINGs: these typically represent serious problems.
# check() automatically builds a package before calling check_built(), as this is the recommended way to check packages. Note that this process runs in an independent R session, so nothing in your current workspace will affect the process. Under-the-hood, check() and check_built() rely on pkgbuild::build() and rcmdcheck::rcmdcheck().

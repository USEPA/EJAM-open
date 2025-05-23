# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

# make sure to install the latest version of the app using your checked out branch's code
# devtools doesn't seem to work with GHA
library(remotes)
remotes::install_local('.',force=T)

library(EJAM)
library(shinytest2)
library(testthat)

# shinytest2 sometimes failing to take screenshots because they are copied into the global tmp directory
# unixtools::set.tempdir('~/tmp')

# this is the main function that does the test commands
source("tests/app-functionality.R")
testthat::set_max_fails(200)
# test_check("EJAM") # this runs all the tests
# "functionality" filter to only shiny tests because they're all named with 'functionality'
# filter="FIPS-functionality" would filter to just the FIPS shinytest
test_app(".", filter="-functionality")

# If you want to create a new test
# shinytest2::record_test(".")

# When tests fail, run this to review diffs
# snapshot_review()
# Run this to review specific files
# testthat::snapshot_review(files="latlon-functionality/latlon-pctlowinc.json")
# To accept the new snapshots
# snapshot_accept()


# This is what ensures tests are run during  R CMD check,
#   which you can start via  check() (i.e., build then do ⁠R CMD check)
# check() automatically builds and checks a source package, using all known best practices. 
# check_built() checks an already-built package.
# Passing ⁠R CMD check⁠ is essential if you want to submit your package to CRAN: you must not have any ERRORs or WARNINGs, and you want to ensure that there are as few NOTEs as possible. If you are not submitting to CRAN, at least ensure that there are no ERRORs or WARNINGs: these typically represent serious problems.
# check() automatically builds a package before calling check_built(), as this is the recommended way to check packages. Note that this process runs in an independent R session, so nothing in your current workspace will affect the process. Under-the-hood, check() and check_built() rely on pkgbuild::build() and rcmdcheck::rcmdcheck().

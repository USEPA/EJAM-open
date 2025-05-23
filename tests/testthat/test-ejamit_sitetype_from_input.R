
  # ejamit() test cases for 3 site types: sitepoints, fips, shapefile


  ## Confirmed  ejamit() could successfully use the function ejamit_sitetype_from_input() to examine the params passed to ejamit()
  ## and it would work as expected instead of having the code be inside ejamit()
  ## The missing and NULL params in outer func ejamit() do in fact get reported as missing() and NULL in the inner _check() func when passed like this.


  ################################################ #

  test_that(" if 0 of 3 is provided - try to interactively select file of latlon", {

    ejamit_test = function(sitepoints = NULL, fips = NULL, shapefile = NULL) {

      sitetype <- ejamit_sitetype_from_input(sitepoints = sitepoints, fips = fips, shapefile = shapefile)


      cat("          is sitepoints reported as missing?   ")
      cat(missing(sitepoints), ' ...  ')
      cat('sitetype: \n')
      return(sitetype)
    }
    testthat::skip_if_not(interactive(), message = "Skipping test: not interactive")
    expect_no_error({
      sitetype <- ejamit_test( )
    })
    # expect_equal(sitetype, "latlon") # now more flexible - allow interactively upload of any type
    expect_null(sitetype)
  })

  test_that("if 1 is provided - note which", {

    ejamit_test = function(sitepoints = NULL, fips=NULL, shapefile=NULL) {

      sitetype <- ejamit_sitetype_from_input(sitepoints = sitepoints, fips = fips, shapefile = shapefile)


      cat("          is sitepoints reported as missing?   ")
      cat(missing(sitepoints), ' ...  ')
      cat('sitetype: \n')
      return(sitetype)
    }
    expect_no_error({
      s1 <- ejamit_test(1)
      s2 <- ejamit_test(sitepoints = 1)
      s3 <- ejamit_test(fips = 2)
      s4 <- ejamit_test(shapefile = 3)
    })
    expect_equal(s1, "latlon")
    expect_equal(s2, "latlon")
    expect_equal(s3, "fips")
    expect_equal(s4, "shp")
  })
  ############# #
  test_that("if 2 are provided - stop or warn", {

    ejamit_test = function(sitepoints = NULL, fips=NULL, shapefile=NULL) {

      sitetype <- ejamit_sitetype_from_input(sitepoints = sitepoints, fips = fips, shapefile = shapefile)


      cat("          is sitepoints reported as missing?   ")
      cat(missing(sitepoints), ' ...  ')
      cat('sitetype: \n')
      return(sitetype)
    }
    expect_error({
      # try({
      ejamit_test(sitepoints = 1, fips = 2)
      # })
    })
    expect_error({
      # try({
      ejamit_test(sitepoints = 1, shapefile = 3)
      # })
    })
    expect_error({
      # try({
      ejamit_test(fips = 2, shapefile = 3)
      # })
    })
  })

  test_that("if 3 are provided - stop or warn", {

    ejamit_test = function(sitepoints=NULL, fips=NULL, shapefile=NULL) {

      sitetype <- ejamit_sitetype_from_input(sitepoints = sitepoints, fips = fips, shapefile = shapefile)


      cat("          is sitepoints reported as missing?   ")
      cat(missing(sitepoints), ' ...  ')
      cat('sitetype: \n')
      return(sitetype)
    }
    expect_error({
      # try({
      ejamit_test(sitepoints = 1, fips = 2, shapefile = 3)
      # })
    })
  })

  ################################################ #


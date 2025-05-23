
test_that("shape2zip() ok", {

  junk = capture_output({
    expect_no_error({
      tfile = tempfile(fileext = ".zip")
      testfilename_a = shape2zip(shp = testshapes_2, file = tfile)
    })  
    expect_true( 
      file.exists(testfilename_a)
    )
  })
})

test_that("shape2zip() ok if file = 'x.shp.zip'", {
  junk = capture_output({
    expect_warning({
      tfile = tempfile(fileext = ".shp.zip") 
      testfilename_b = shape2zip(shp = testshapes_2, file = tfile)
    })  
    expect_true(
      file.exists(testfilename_b)
    )
  })
})


#############################################################################

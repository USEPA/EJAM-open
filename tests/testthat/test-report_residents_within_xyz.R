
checkit <- function(mytest) {
  params <- rbindlist(mytest)
  names(params) <- c('sitetype', 'radius', 'nsites')
  data.frame(params,
             text = sapply(mytest, function(z) {
               
               report_residents_within_xyz(
                 
                 sitetype = z[[1]], 
                 radius = z[[2]], 
                 nsites = z[[3]]
               )
             })
  )
}
############################## #   ############################## # 

test_that("report_residents_within_xyz test123", {
  
  
  test1 <- list(
    
    # list('latlon', 0, 1), # cannot occur - zero radius with latlon type
    # list('latlon', 0, 100), # cannot occur - zero radius with latlon type
    list('latlon', 3, 1),
    list('latlon', 3, 100),
    
    list('fips', 0, 1),
    list('fips', 0, 100),
    # list('fips', 3, 1), # cannot occur - nonzero radius with fips type
    # list('fips', 3, 100), # cannot occur - nonzero radius with fips type
    # list( NA, '99 miles', 'seven sites'),  # fails if NA
    
    list('shp', 0, 1),
    list('shp', 0, 100),
    list('shp', 3, 1),
    list('shp', 3, 100),
    
    list('farm',              '99 miles',          'seven'),
    list( "Georgia location", '9.9 kilometers',    "several"),
    list('study location',    "close proximity to",  100),
    
    list('Type X site', 3, 100)   # ok singular / plural 
  )
  ############################## #   
  expect_no_error({
    x = checkit(test1)
  }
  # , label = "test1" 
  )
  ############################## #   ############################## # 
  
  test2 <- list(
    
    # fix/note singular/plural
    
    list('Type X facility', 3, 100), 
    list('Type X facilities', 3, 100), 
    
    # fix "within" 
    
    list('study location', "at", 100),       # within at
    list('Delaware Counties', "within", 3),  # within 'within' 
    
    # fix "" cases
    
    list( "Georgia location", '9.9 kilometers', ""), 
    list( "", '9.9 kilometers', "several"),
    list( "Georgia location", '', "several"),
    list('', '', '')
  ) 
  ############################## #   
  expect_no_error({
    
    x = checkit(test2)
  })
  ############################## #   ############################## # 
  
  test3 <- list(
    
    #   na values 
    list(NA, 3, 100),
    list('latlon', NA, 100),
    list('latlon', 3, NA)
  )
  ############################## #   
  
  expect_no_error({
    
    x = checkit(test3)
    
    # ## but not useful results if NA 
    # sitetype radius nsites                                                    text
    # 1     <NA>      3    100       Residents within 3 miles of any of the 100 places
    # 2   latlon     NA    100         Residents within any of the 100 selected points
    # 3   latlon      3     NA Residents within 3 miles of any of the  selected points
  })
  
})
########################################################################### #

test_that("report_residents_within_xyz test4 warns if NULL params", {
  
  test4 <- list( 
    
    # NULL values 
    list(NULL, 3, 100),
    list('latlon', NULL, 100),
    list('latlon', 3, NULL)
  )
  ############################## # 
  expect_no_error({
    suppressWarnings({
      x = checkit(test4)
    })
  })
  
  expect_warning({
    x = checkit(test4)
  } )
  
})
########################################################################### #

rm(checkit)


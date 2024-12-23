
testthat::test_that("fixcolnames_infer works at all", {
  
  test_address_table_renamed <- test_address_table
  testthat::expect_no_error({names(test_address_table_renamed) <- fixcolnames_infer(names(test_address_table_renamed))})
  testthat::expect_identical(test_address_table_goodnames, test_address_table_renamed)
  
  testthat::expect_equal(fixcolnames_infer(currentnames = test_address_parts1),
                             c("lat", "lon", "address", "street", "city", "state", "zip"),
                            ignore_attr = TRUE)
  testthat::expect_identical(fixcolnames_infer(currentnames = names(test_address_table) ),
                             c("Acol", "street", "city", "state", "zip", "other_column"))
})
###################### #


######################################################## # ######################################################## # 
######################################################## # ######################################################## # 
######################################################## # ######################################################## # 
# function to run same tests for whatever dataset of examples
# for the suite of functions that rename variable names

####################################################### #

testsetup = function(tlist, oldnames, fun2test) {
  bestnames = names(tlist)
  newnames = fun2test(oldnames)
  results = data.frame(old = oldnames, 
                       new = newnames, 
                       match_but_not_best = oldnames %in% lat_alias,
                       renamed = oldnames !=  newnames, 
                       new_is_canonical = newnames %in% bestnames)
  return(results)
}
############################### #
run_renaming_tests = function(tlist, oldnames, testname="test", fun2test = fixcolnames_infer) {
  
  ## run standard tests on functions that rename things
  
  results = testsetup(tlist = tlist, oldnames = oldnames, fun2test = fun2test)
  
  bestnames = names(tlist)
  synonyms =  as.vector(unlist(tlist)) # remove canonical name from list of aliases if shows up there too
  synonyms =  synonyms[!(synonyms %in% bestnames)]
  old_is_canon = oldnames %in% bestnames
  
  testthat::test_that(paste(testname, "-if oldname already canonical, always old = new"), {
    testthat::expect_true(
    all((results$old == results$new)[old_is_canon])
    )
  })
  
  testthat::test_that(paste(testname, "-if gets renamed, new name is among canonical"), {
    testthat::expect_true(
      all((results$new_is_canonical)[results$renamed])
    )  
  })
  
  testthat::test_that(paste(testname, "-if not a synonym, never renamed "), {
    testthat::expect_true(
      all(!results$renamed[!(results$old %in% synonyms)])
    )
  })
  
  # hard to test if 1st choice was used 
  
  ###################################### # 
  ## THIS TEST IS ONLY RIGHT FOR when you want to pick the best 1 match instead of renaming all
  if (identical(fixcolnames_infer,  fun2test)  | identical(fun2test , EJAM:::latlon_infer))  {
    if (identical(fun2test , EJAM:::latlon_infer)) {
      cat("installed version of latlon_infer() being used, which may differ from latest source version!\n")
    }
    testthat::test_that(paste(testname, "-among synonyms of 1 terms, renames 1 and only 1"), {
      best1 = names(tlist)[[1]]
      synonyms_of_1 = as.vector(unlist(tlist[[1]]))
      synonyms_of_1 = synonyms_of_1[!(synonyms_of_1 %in% best1)]
      results1 = testsetup(tlist[[1]], oldnames = synonyms_of_1, fun2test = fun2test)
      testthat::expect_true({
        sum(results1$renamed) == 1
      })  
    } )
  } else {
    ## THIS IS IF YOU EXPECT THEM ALL TO GET RENAMED
    testthat::test_that("all noncanon synonyms get renamed",{
      testthat::expect_true(
        all(results$renamed[!old_is_canon & results$old %in% synonyms])
      )
    })
  }
  
  ###################################### # 
}

######################################################## # ######################################################## # 
######################################################## # ######################################################## # 
######################################################## # ######################################################## # 


# check case of only a single standard term

run_renaming_tests(tlist = list(lat = lat_alias[lat_alias != "lat"]), 
                   oldnames = lat_alias[lat_alias != "lat"],
                   testname = "oldnames all match the same 1 standard term",
                   fun2test = fixcolnames_infer)

######################################################## # 

# check case of multiple sets of terms

mylist <- list(
  lat = lat_alias, 
  lon = lon_alias,
  address = c("address"),
  street = c("street", "street address", "address1", "address 1"),
  city = c("city", "cityname", "city name"),
  state = c("state", "mystate", "statename", "ST"),
  zip = c("zip", "zipcode", "zip code")
)
myold = as.vector(unlist(mylist))
myold = myold[!(myold %in% names(mylist))]
myold = c(myold, "junk", "typo")

run_renaming_tests(tlist = mylist, 
                   oldnames = myold, 
                   testname = "fixcolnames_infer - latlon etc example",
                   fun2test = fixcolnames_infer
                   )
######################################################## # 

# check using the long alias_list (that is default in another function)

mylist = eval(formals(fixmapheadernamescolname)$alias_list)
  # names(eval(formals(fixmapheadernamescolname)$alias_list))
  # as.vector(unlist(eval(formals(fixmapheadernamescolname)$alias_list)))

# cat("installed version of latlon_infer() being used, which may differ from latest source version!\n")
run_renaming_tests(tlist = mylist, 
                   oldnames = as.vector(unlist(mylist)), 
                   testname = "latlon_infer", 
                   fun2test =fixmapheadernamescolname   ####### # 
)
######################################################## # 




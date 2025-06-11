#################################################################### #
# LIST OF FUNCTIONS TO TEST ####
#################################################################### #

################################## #     misc fips-related functions

# fips_valid()
# fipstype()
# fips_lead_zero()  - tests are at the end of this file
# counties_as_sites() # creates table like getblocksnearby() does. and could use in mod_fipspicker-DRAFT.R
#   states_as_sites()
# is.island()

################################## #    fips_ from_

###  if func is the OUTPUT, names are like   fips <- fips_from_x(x)

#          fips_from_table()
#     name2fips() and names2fips()  # inconsistent naming, but useful aliases
#          fips_from_name()  # same as name2fips()
#    fips_state_from_state_abbrev()
#    fips_state_from_statename()     # should it be statename or state_name
# fips_counties_from_statefips(   )  # should it be statefips or state_fips
# fips_counties_from_state_abbrev()
# fips_counties_from_statename(   )  # should it be statename or state_name
# fips_counties_from_countyname()
# fips_counties_from_countynamefull()  internal helper
#       fips_bgs_in_fips()
### and
###   see   getblocksnearby_from_fips() which uses  fips_bgs_in_fips()

##NOT  cities_as_sites()  would be a name that makes sense but not used.
##NOT regions_as_sites()  would be a name that makes sense but not used.
##NOT   tracts_as_ and blockgroups_as_  maybe useful?

################################## #    fips2...

####  if fips is the INPUT, names are like   x <- fips2x(fips)
#
# fips_st2eparegion()  # but not eparegion2statefips() ?? or
#    fips2state_fips(    )     #  fips2statefips would be a more consistent name?
#    fips2state_abbrev(  )
#    fips2statename(     ) # should it be statename or state_name
#    fips2countyname()
#    fips2name()    # inverse of name2fips()
############################################################################# #

#################################################################### #
## misc functions ####
#################################################################### #

# fips_valid()

test_that("fips_valid() works but is slow", {

  expect_no_error({
    testfipsx <- c(
      "3651000", # cdp
      "10", "10001", "02170000300",
      "021700003003", "721537506011",
      "010010201001000")
    x <- fips_valid(testfipsx)
  })
  expect_true(all(x))
  expect_identical(
    fipstype(testfipsx),
    c('city',
      "state", "county", "tract",
      "blockgroup", "blockgroup",
      "block"
    )
  )

  expect_no_error({
    fips_valid(c('01',NA))
    fips_valid(NA)
    fips_valid("text")
  })
  expect_no_warning({
    fips_valid(c('01',NA))
    fips_valid(NA)
    fips_valid("text")
  })
  expect_false(fips_valid(NA))
  expect_identical(
    fips_valid(c(NA, "01")),
    c(FALSE, TRUE)
  )

  expect_identical(
    fips_valid(c(
      1, "1", "01",
      "00",
      "10001",
      "01001", 1001
    )),
    c(TRUE , TRUE , TRUE , # once leading zeroes are added, these are ok state fips
      FALSE ,   # "00"  is not an actual fips
      TRUE,  TRUE,  TRUE    )# once leading zeroes are added, these are ok county fips
  )

  expect_identical(
    fips_valid(
      fips_lead_zero(
        c(
          1, "1", "01", # lead zero fixed 1 and "1"
          "00",
          "10001",
          "01001", 1001 # lead zero fixed 1001
        ))),
    c(TRUE, TRUE, TRUE,
      FALSE,
      TRUE,
      TRUE, TRUE
    )
  )

  expect_identical(
    fips_valid(1:5),
    c(TRUE, TRUE, FALSE, TRUE,TRUE)
  )
}
)
#################################################################### #

testthat::test_that("fipstype_from_nchar for 1-15", {

  #     n      ftype
  # 1   1      state
  # 2   2      state
  # 3   3       <NA>
  # 4   4     county
  # 5   5     county
  # 6   6       city
  # 7   7       city
  # 8   8       <NA>
  # 9   9       <NA>
  # 10 10      tract
  # 11 11      tract  # tract unless is bg missing a leading zero.. AMBIGUOUS CASE IF NOT SURE IF IT IS MISSING A LEADING ZERO.
  # 12 12 blockgroup
  # 13 13       <NA>
  # 14 14      block
  # 15 15      block

  t1 <- c(1:15, 15)
  t1_type_found <- fipstype_from_nchar(t1)
  t1_type_expected <- c(
    "state", "state", NA, # 1,2,  3
    "county", "county",   # 4,5
    "city", "city",     # 6,7
    NA, NA,   # 8,9

    "tract", # 10
    "tract",   # and   11   ***

    "blockgroup",     # 12
    NA,          # 13
    "block", "block",   # 14,15
    "block"   # REPEAT OF 15
  )
  expect_identical(t1_type_expected, t1_type_found)

})
############################################################################# #

testthat::test_that("fipstype_from_nchar NA if BAD inputs", {

  t2 <- c(16, 0, NA, "", 'text', FALSE)
  t2_type_found <- fipstype_from_nchar(t2)
  t2_type_expected <- c(NA, NA, NA, NA, NA, NA)
  # expect_identical(t2_type_expected, t2_type_found) # ONE IS LOGICAL OTHER IS CHARACTER
  expect_true(all(is.na(t2_type_found)))

})
############################################################################# #

# fipstype2nchar

testthat::test_that("fipstype2nchar ok", {

  #     n      ftype
  # 1   1      state
  # 2   2      state
  # 3   3       <NA>
  # 4   4     county
  # 5   5     county
  # 6   6       city
  # 7   7       city
  # 8   8       <NA>
  # 9   9       <NA>
  # 10 10      tract
  # 11 11      tract  # tract unless is bg missing a leading zero.. AMBIGUOUS CASE IF NOT SURE IF IT IS MISSING A LEADING ZERO.
  # 12 12 blockgroup
  # 13 13       <NA>
  # 14 14      block
  # 15 15      block

  t1 <- c(1:15)
  t1_type_found <- fipstype_from_nchar(t1)
  t1_n_incl_lead0_found <- fipstype2nchar(t1_type_found) # ??
  # cbind(t1_original = t1, t1_type_found, t1_n_incl_lead0_found)

  t1_n_expected <- c(2,2, NA, 5,5, 7,7, NA,NA,
                     11,11,  12,  NA,  15,15 )
  # NOT same as original inputs, t1
  expect_equal(t1_n_incl_lead0_found, t1_n_expected)

  # print(
  #   data.table(type_inferred = t1_type_found, inferred_from_what = t1,
       # inferred_incl_lead0 = t1_n_incl_lead0_found, expected = t1_n_expected)
  # )
  #    type_inferred inferred_from_what inferred_incl_lead0 expected
  #
  # 1:         state                  1                   2        2
  # 2:         state                  2                   2        2
  # 3:          <NA>                  3                  NA       NA
  # 4:        county                  4                   5        5
  # 5:        county                  5                   5        5
  # 6:          city                  6                   7        7
  # 7:          city                  7                   7        7
  # 8:          <NA>                  8                  NA       NA
  # 9:          <NA>                  9                  NA       NA
  # 10:         tract                 10                  11       11
  # 11:         tract                 11                  11       11
  # 12:    blockgroup                 12                  12       12
  # 13:          <NA>                 13                  NA       NA
  # 14:         block                 14                  15       15
  # 15:         block                 15                  15       15
})
############################################################################# #

# fipstype()

# testfips16 = c("1", "12", "123", "1234", "12345", "123456", "1234567", "12345678", "123456789",
#                "1234567890", "12345678901", "123456789012", "1234567890123", "12345678901234",
#                "123456789012345", "1234567890123456")
# cbind(fipstype(testfips16), fips_lead_zero(testfips16), testfips16)
# testfips16
#
# [1,] "state"      "01"              "1"
# [2,] "state"      "12"              "12"
# [3,] NA           NA                "123"
# [4,] "county"     "01234"           "1234"
# [5,] "county"     "12345"           "12345"
# [6,] "city"       "0123456"         "123456"
# [7,] "city"       "1234567"         "1234567"
# [8,] NA           NA                "12345678"
# [9,] NA           NA                "123456789"
# [10,] "tract"      "01234567890"     "1234567890"
# [11,] "tract" ?    "12345678901"     "12345678901"  # AMBIGUOUS CASE - MIGHT BE BLOCK GROUP MISSING THE LEADING 0 ***
# [12,] "blockgroup" "123456789012"    "123456789012"
# [13,] NA           NA                "1234567890123"
# [14,] "block"      "012345678901234" "12345678901234"
# [15,] "block"      "123456789012345" "123456789012345"
# [16,] NA           NA                "1234567890123456"
######################################## #


test_that("fipstype() 11 digits tract vs blockgroup", {

  realtracts = substr(blockgroupstats$bgfips,1,11)
  realbgs    = blockgroupstats$bgfips

  test_tract_missing0 =   4013116500   # 10 digits tract missing 0
  test_tract_good     = "04013116500"  # 11 digits full tract includes leading 0 !!!!!!!!!!
  test_bg_missing0    =   40131165002  # 11 digits blockgroup missing leading 0 !!!!!!!!!!
  test_bg_good        = "040131165002" # 12 digits full blockgroup

  # confirm test cases are what we want
  expect_equal(11, nchar(test_tract_good))  # 11 digits full tract includes leading 0
  expect_equal(11, nchar(test_bg_missing0)) # 11 digits blockgroup missing leading 0
  expect_true(test_tract_missing0 %in% as.numeric(realtracts)) # it is a real tract
  expect_true(test_tract_good %in% realtracts) # it is a real tract
  expect_true(test_bg_missing0 %in% as.numeric(realbgs)) # it is a real BG
  expect_true(test_bg_good %in% realbgs) # it is a real BG

  # Able to distinguish between tract and blockgroup for an 11-digit number:

  expect_identical(
    fipstype(test_tract_missing0), 'tract'
  )
  expect_identical(
    fipstype(test_tract_good), 'tract'
  )
  expect_identical(
    fipstype(test_bg_missing0), 'blockgroup'
  )
  expect_identical(
    fipstype(test_bg_good), 'blockgroup'
  )
  ##################### #

  # > nchar(060372623011)
  # [1] 11
  # > fips_lead_zero('60372623011') # 11 digits - identified it as actual BG missing a 0
  # [1] "060372623011"
  # # fips_lead_zero(060372623011) == "060372623011"
  # [1] TRUE

  # > nchar("34023001410")
  # [1] 11
  # fips_lead_zero("34023001410") == "34023001410" # 11 digits - identifies it as actual TRACT.
  # [1] TRUE

})
######################################## #

test_that("fipstype() works", {

  casex = vector()
  testx = vector()
  truex = vector()
  realbgs    = blockgroupstats$bgfips
  realtracts = substr(blockgroupstats$bgfips,1,11)
  testx[1] = "12345678901";  casex[1]  <- "test_11_char_invalid"; truex[1] <- NA
  testx[2] =  12345678901;   casex[2]  <- 'test_11_num_invalid';  truex[2] <- NA

  testx[3]  =  40109109500;   casex[3] <- 'test_11_tract_num_neednozero';    truex[3] <- "tract"
  testx[12] = "06073020041";  casex[12] <- 'test_10_char_tract_missingzero'; truex[12] <- "tract"
  testx[4]  =   6073020041;   casex[4] <- 'test_10_num_tract_missingzero';   truex[4] <- "tract"
  testx[5]  =   60730200411;  casex[5] <- 'test_11_BG_num_missing_leadzero'; truex[5] <- "blockgroup"
  testx[6]  = "060730200411"; casex[6] <- 'test_12_bg_char_hasleadzero';     truex[6] <- "blockgroup"
  testx[7]  =  240338006062;  casex[7] <- 'test_12_bg_num_neednozero';       truex[7] <- "blockgroup"
  testx[8]  = "240338006062"; casex[8] <- 'test_12_bg_char_neednozero';      truex[8] <- "blockgroup"

  testx[9] =   "123456789012"; casex[9]  <- 'test_12_char_invalid'; truex[9] <- NA
  testx[10] =   123456789012;  casex[10] <- 'test_12_num_invalid'; truex[10] <- NA
  testx[11] =  "012345678912"; casex[11] <- 'test_12_char_invalid_hasleadzero'; truex[11] <- NA

  n = 12

  print(
    cbind(fips = testx[1:n],
          actual =  truex[1:n],
          fipstype = fipstype(testx[1:n]),
          expected = "?",
          fips_valid = fips_valid(testx[1:n]),
          fips_valid_afterlead0fixed = fips_valid(fips_lead_zero(testx[1:n])),
          nchar = nchar(testx[1:n]),
          case = casex[1:n],
          bgfips_ok_asis = ifelse(testx[1:n] %in% realbgs, 'bg',''),
          tractfips_ok_asis = ifelse(testx[1:n] %in% realtracts, 'tract','')
    )
  )

  ######################################### #

  test_tract_missing0 =   4013116500   # 10 digits tract missing 0
  test_tract_good     = "04013116500"  # 11 digits full tract includes leading 0 !!!!!!!!!!
  test_bg_missing0    =   40131165002  # 11 digits blockgroup missing leading 0 !!!!!!!!!!
  test_bg_good        = "040131165002" # 12 digits full blockgroup

  testfips16 = c("1", "12",   # 1,2 is State fips
                 "123",    # 3 is bad
                 "1234", "12345",   # 4 5 county
                 "123456", "1234567",    # 6,7 is CDP/CITY
                 "12345678", "123456789",  # 8 9 bad
                 "1234567890",   # 10  like a tract MISSING LEAD ZERO

                 test_tract_good, # 11 digit TRACT INCLUDES LEAD 0
                 #test_bg_missing0, # 11 digit BLOCKGROUP MISSING LEAD 0

                 # "12345678901",  # 11 NOT A REAL FIPS AND  AMBIGUOUS -- SHOULD CHECK ACTUAL FIPS VALIDITY
                 "123456789012", # 12 bg
                 "1234567890123",   # 13 bad
                 "12345678901234", "123456789012345",  # 14,15 block
                 "1234567890123456")  # 16 bad


  suppressWarnings({

    expect_identical(

      fipstype(testfips16),

      c("state", "state",   # 1,2 is State fips
        NA,                   # 3 is bad
        "county", "county",    # 4 5 county
        "city", "city",         # 6,7 is CDP/CITY
        NA, NA,     # 8, 9 bad
        "tract",     # 10   tract
        "tract",       # 11   AMBIGUOUS ??????? ***
        "blockgroup",   # 12
        NA,              # 13 bad
        "block", "block",    # 14, 15
        NA)                    # 16 bad
    )

    expect_no_error({
      testfips <- c("1", "12", "123", "1234", "12345",
                    "123456",  "1234567", # CDP/CITY
                    "", NA, "words")
      fipstype(testfips)
    })
    expect_warning({
      fipstype(NA)
    })
    expect_warning({
      fipstype("WORD")
    })
    expect_identical(
      c("state","state",NA,"county","county",
        'city','city',
        NA,NA,NA),
      fipstype(testfips)
    )
    test_types = fips_counties_from_statename(c("Connecticut", "Delaware") )
    expect_true({
      all(
        fipstype(test_types) == "county"
      )
    })

    expect_true(!identical(fipstype(c(NA,NA)), c(NA,NA))) # c(NA,NA) is logical class
    expect_identical(fipstype( c(NA_character_,NA_character_)), c(NA_character_,NA_character_))
    expect_identical(fipstype("words"), NA_character_)
    expect_identical(fipstype(""), NA_character_)
    expect_identical(fipstype(99), "state")
    expect_no_warning(fipstype(99))
    expect_identical(fipstype(1:5), c("state","state","state","state","state"))
  })
})
#################################################################### #

# (this makes it appear in file outline view in expected order)
`fips_lead_zero tests are at end of this file` = function() {}
rm(`fips_lead_zero tests are at end of this file`)

#################################################################### #
#    counties_as_sites()          # creates table like getblocksnearby()   and could get used by EJAM/R/mod_fipspicker-DRAFT.R

test_that("counties_as_sites works", {
  suppressWarnings({
    expect_no_error({
      counties_as_sites(
        fips_counties_from_state_abbrev("DE")
      )
    })

    # counties_as_sites(fips_counties_from_state_abbrev("DE"))
    x = counties_as_sites(
      fips_counties_from_state_abbrev("DE")
    )
    expect_true(is.data.table(x))
    expect_identical(
      names(x), c("ejam_uniq_id", "countyfips", "bgid", "blockid", "blockwt",
                  "distance", "distance_unadjusted")
    )
    expect_true(
      all(x$countyfips %in% fips_counties_from_state_abbrev("DE"))
    )
    expect_true(
      length(unique(x$ejam_uniq_id)) == length(fips_counties_from_state_abbrev("DE"))
    )

    expect_warning(
      counties_as_sites(c(10001, NA))  # it does warn in this case
    )
    expect_warning(
      counties_as_sites(NA)  # it probably should warn in this case and does
    )
    # note it ignores duplicates:   counties_as_sites(c(10005, 10005))
  })
})
#################################################################### #
#      states_as_sites()

test_that("states_as_sites works", {
  suppressWarnings({
    suppressMessages({

      expect_no_error({
        states_as_sites(
          fips_state_from_state_abbrev("DE")
        )
      })
      x = states_as_sites(
        fips_state_from_state_abbrev(c("RI", "DE"))
      )
      expect_true(is.data.table(x))
      expect_identical(
        names(x), c("ejam_uniq_id", "statefips", "bgid")
      )
      expect_true(
        all(x$statefips %in% fips_state_from_state_abbrev(c("RI", "DE")))
      )
      expect_true(
        length(unique(x$ejam_uniq_id)) == length(fips_state_from_state_abbrev(c("RI", "DE")))
      )

      expect_no_error(states_as_sites(c(10, NA)) )  # should not crash
      expect_warning(
        states_as_sites(c(10, NA))  # it does warn in this case
      )
      expect_warning(
        states_as_sites(NA)  # it probably should warn in this case !
      )
      # note it ignores duplicates:   states_as_sites(c(10, 10))
    })
  })
})

#################################################################### #

test_that("is.island() works", {
  suppressWarnings({
    suppressMessages({

      expect_true({
        all(is.island(statename = tolower(unique(stateinfo2$statename[stateinfo2$is.island.areas]))))
      })
      expect_true({
        all(is.island(ST = tolower(unique(stateinfo2$ST[stateinfo2$is.island.areas]))))
      })
      expect_true({
        all(is.island(fips = unique(stateinfo2$FIPS.ST[stateinfo2$is.island.areas])))
      })
      expect_true({
        !(is.island(NA))
      })
      expect_error({
        is.island(NULL)
      })
      expect_error({
        is.island(ST = "NY", statename = "New York")
      })
      # is.island(c("PR", "DE", "AS", NA))
      # is.island(statename = c("Guam", "New York", "american samoa", NA))
      # is.island(fips = c(21001, 60, "60", "600010000000"))
      # tail(cbind(stateinfo2[ , c("statename", "is.island.areas")], is.island(stateinfo2$ST)),10)
    })

  })
})
#################################################################### #
#################################################################### #
## fips_ from_ ####
#################################################################### #
#################################################################### #


#################################################################### #
#     name2fips()  # inconsistent name but useful . inverse of  fips2name()
#          fips_from_name()  # same as name2fips()

#
#  # reports twice because it cannot tell the two query terms are synonyms

# names2fips( "Jackson, PA"  )
# "4237344"  # just reports one of the many possible answers

test_that("name2fips() works on city/town/cdp", {

  testplaces = c("North Richmond, CA", "McFarland, CA", "Chelsea, MA", "Hamtramck, MI",
                 "St. John the Baptist Parish, LA", "Dallas, TX",
                 "chicago, IL", "chicago, Illinois", "East Chicago, IN",
                 "Salt Lake City, UT", "Commerce City, North Denver",
                 "Atlanta, GA", "Westside, GA", "Queens Creek Watershed, GA",
                 "Grand Rapids, MI", "Jackson, MI", "Adrian, MI", "East St. Louis, IL",
                 "Cahokia Heights, IL", "Cahokia village, IL", "xyz facility, IL", "Rocky Mountain Interagency site, CO",
                 "New York CAFO", "mega-site adjacent to xyz, New York",
                 "Bad Name project", "Chicago large Project, IL",
                 "Cuyahoga County, OH", "Cuyahoga County, Ohio", "Cuyahoga, OH", "Cuyahoga County, OH project",

                 "New York", "new york, NY", "new york, new york"
  )

  testnostate = c(
    # "California", "CA",
    "chicago", # "chicago, IL", "chicago, Illinois", "East Chicago, IN",
    "Yakima"
    # , "Yakima, WA"
  )

  junk = capture_output({

    suppressWarnings({
      expect_no_error({
        x = name2fips(testplaces)
      })
    })

    expect_identical(
      name2fips("Atlanta, GA"), "1304000"
    )
    expect_identical(
      name2fips("Atlanta, GA"), "1304000"
    )
    expect_identical(
      name2fips( "Salt Lake City, UT"), "4967000"
    )

    # >   cbind(testplaces, x)
    # testplaces                            x
    # [1,] "North Richmond, CA"                  "0652162"
    # [2,] "McFarland, CA"                       "0644826"
    # [3,] "Chelsea, MA"                         "2513205"
    # [4,] "Hamtramck, MI"                       "2636280"
    # [5,] "St. John the Baptist Parish, LA"     "22095"
    # [6,] "Dallas, TX"                          "4819000"
    # [7,] "chicago, IL"                         "1714000"
    # [8,] "chicago, Illinois"                   "1714000"
    # [9,] "East Chicago, IN"                    "1819486"
    # [10,] "Salt Lake City, UT"                  "4967000"
    # [11,] "Commerce City, North Denver"         NA
    # [12,] "Atlanta, GA"                         "1304000"
    # [13,] "Westside, GA"                        NA
    # [14,] "Queens Creek Watershed, GA"          NA
    # [15,] "Grand Rapids, MI"                    "2634000"
    # [16,] "Jackson, MI"                         "2641420"
    # [17,] "Adrian, MI"                          "2600440"
    # [18,] "East St. Louis, IL"                  "1722255"
    # [19,] "Cahokia Heights, IL"                 NA
    # [20,] "Cahokia village, IL"                 "1710370"
    # [21,] "xyz facility, IL"                    NA
    # [22,] "Rocky Mountain Interagency site, CO" NA
    # [23,] "New York CAFO"                       NA
    # [24,] "mega-site adjacent to xyz, New York" NA
    # [25,] "Bad Name project"                    NA
    # [26,] "Chicago large Project, IL"           NA
    # [27,] "Cuyahoga County, OH"                 "39035"
    # [28,] "Cuyahoga County, Ohio"               "39035"
    # [29,] "Cuyahoga, OH"                        NA
    # [30,] "Cuyahoga County, OH project"         NA
    # [31,] "New York"                            "36"
    # [32,] "new york, NY"                        "3651000"
    # [33,] "new york, new york"                  "3651000"
    #

    expect_warning({
      x = name2fips(testnostate)
    })

    expect_identical(
      c( "3579070","3579070"),
      names2fips( c(  "Torreon, New Mexico" , 'torreon,nm'))
    )

  })
})
#################################################################### #
test_that("fips_from_name aka name2fips() works on state or county", {
  junk = capture_output({

    suppressWarnings({
      expect_no_error({
        x = name2fips(c("delaware", "NY"))
      })
      expect_true({
        all.equal(x, c("10", "36"))
      })
      expect_identical(
        name2fips("rhode island"), "44"
      )
      expect_true(
        "48201" == name2fips("Harris County, TX")
      )
      expect_true(
        "48201" == name2fips("harris county, tx")
      )
      expect_true(
        all(c("48201", "36") == name2fips(c("harris county, tx", "NY")))
      )
      expect_true(
        13 == name2fips("georgia")
      )
      expect_true(
        is.na(name2fips(NA))
      )
      expect_true(
        identical(name2fips(c("georgia", "Harris County, TX", NA)), c("13", "48201", NA))
      )
    })
  })

})
#################################################################### #
# fips_from_table()

test_that("fips_from_table() works", {
  # fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips',
  #                 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
  suppressWarnings({
    mydat <- data.frame(
      FIPS = c("10001", 1, 10, NA, "text"),
      other = c("ok", "ok", "ok", "not ok, na", "not fips, text")
    )
    expect_no_error({
      expect_warning({
        x = fips_from_table(fips_table = mydat)
      })
    })
    expect_true(
      all(c("10001", "01", "10", NA, NA) == x, na.rm = T)
    )

    mydat_y <- data.frame(
      countyfips = c("10001", 1, 10, NA, "text"),
      other = c("ok", "ok", "ok", "not ok, na", "not fips, text")
    )
    expect_warning({
      y = fips_from_table(mydat_y)
    })
    expect_identical(x, y)

    # uses acceptable names in order or preference picking best one
    # uses statefips as first choice here and ignores countyfips
    mydat_z <- data.frame(
      countyfips = "01001",
      statefips = "10"
    )
    expect_true(
      fips_from_table(mydat_z) == "10"
    )
    expect_warning(
      # no suitable colname found
      fips_from_table(data.frame(x = 1:3, y = 1:3))
    )

  })
})
#################################################################### #
# fips_place2placename()

test_that("fips_place2placename() works", {

  # testplaces = c("North Richmond, CA", "McFarland, CA", "Chelsea, MA", "Hamtramck, MI",
  #                "St. John the Baptist Parish, LA", "Dallas, TX",
  #                "chicago, IL", "chicago, Illinois", "East Chicago, IN",
  #                "Salt Lake City, UT", "Commerce City, North Denver",
  #                "Atlanta, GA", "Westside, GA", "Queens Creek Watershed, GA",
  #                "Grand Rapids, MI", "Jackson, MI", "Adrian, MI", "East St. Louis, IL",
  #                "Cahokia Heights, IL", "Cahokia village, IL", "xyz facility, IL", "Rocky Mountain Interagency site, CO",
  #                "New York CAFO", "mega-site adjacent to xyz, New York",
  #                "Bad Name project", "Chicago large Project, IL",
  #                "Cuyahoga County, OH", "Cuyahoga County, Ohio", "Cuyahoga, OH", "Cuyahoga County, OH project",
  #
  #                "New York", "new york, NY", "new york, new york"
  # )
  #
  # testfips = fips_place_from_placename(testplaces)


  testfips1 = c(4276536L, 657240L, 4203656L, 5531225L, 7218891L, 2721986L,
                3901042L, 4053400L, 3439630L, 423760L, 3986366L, 506340L, 4243944L,
                5430364L, 5582825L, 5136216L, 2740238L, 4250272L, 3405740L, 3714460L
  )
  testname1 = c("Thompsontown borough, PA", "Pine Mountain Club CDP, CA", "Avondale borough, PA",
                "Greenfield town, WI", "Coco comunidad, PR", "Fosston city, MN",
                "Albany village, OH", "Oakland town, OK", "Lebanon borough, NJ",
                "Florence town, AZ", "Woodlawn village, OH", "Birdsong town, AR",
                "Littlestown borough, PA", "Gauley Bridge town, WV", "Vilas town, WI",
                "Heathsville CDP, VA", "Maple Lake township, MN", "Mohnton borough, PA",
                "Beverly city, NJ", "Cooleemee town, NC")

  expect_no_error({
    testresult1 = fips_place2placename(testfips1)
  })
  expect_equal(
    testresult1,
    testname1
  )

  suppressWarnings({
    expect_true(
      is.na(fips_place2placename(NA))
    )

    expect_true(
      all(is.na(fips_place2placename(c(NA, NA))   )  )
    )
  })
})
#################################################################### #
# fips_place_from_placename()

test_that("fips_place_from_placename() works", {
  junk <- testthat::capture_output({
    expect_no_error({
      fips_place_from_placename("Thompsontown borough, PA")
      fips_place_from_placename("Thompsontown borough, pennsylvania")
      fips_place_from_placename("Thompsontown, PA")
    })
    expect_equal(
      fips_place_from_placename(c("atlanta,ga", "los angeles,ca")),
      c(1304000, 644000)
    )
    expect_true(is.na(fips_place_from_placename("xyz,ny")))
  })
})

#################################################################### #
#    fips_state_from_state_abbrev()

test_that("fips_state_from_state_abbrev() works", {
  suppressWarnings({
    suppressMessages({
      expect_true({
        all(fips2state_abbrev(
          fips_state_from_state_abbrev(c("DE", "RI"))
        ) %in% c("DE", "RI"))
      })
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("pr") )) == "PR"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("vi") )) == "VI"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("GU") )) == "GU"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_state_abbrev("MP") )) == "MP"
      )
      expect_true(
        fips_state_from_state_abbrev("UM") == 74 #    U.S. Minor Outlying Islands
      )

      expect_warning(
        fips_state_from_state_abbrev(c("text", "other", "DE"))
      )  # probably should warn
      expect_true(
        is.na(fips_state_from_state_abbrev("text"))  # DOES return NA
      )
      expect_warning(fips_state_from_state_abbrev(13))  # probably should warn there is no such fips
      expect_warning(fips_state_from_state_abbrev(c(NA, "RI")))  # maybe should warn
      expect_identical(fips2state_abbrev(1:5), c("AL", "AK", NA, "AZ", "AR"))

    })
  })
})
#################################################################### #
#    fips_state_from_statename()

test_that("fips_state_from_statename() works", {
  suppressWarnings({
    suppressMessages({
      expect_true({
        all(fips2state_abbrev(
          fips_state_from_statename(c("delaware", "Montana"))
        ) %in% c("DE", "MT"))
      })
      testnames = stateinfo2$statename
      testnames = testnames[!(testnames %in% c("United States", "U.S. Minor Outlying Islands"))]
      testfips <- fips_state_from_statename(testnames)
      expect_true(
        all(fips_valid(testfips))
      )

      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("PUERto rico") )) == "PR"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("U.S. Virgin Islands") )) == "VI"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("GUAM") )) == "GU"
      )
      expect_true(
        unique(fips2state_abbrev( fips_state_from_statename("Northern Mariana Islands") )) == "MP"
      )
      expect_true(
        fips_state_from_statename("U.S. Minor Outlying Islands") == 74 #    U.S. Minor Outlying Islands
      )

      expect_warning(
        fips_state_from_statename("text")
      )  # probably should warn NO SUCH STATE
      expect_true(
        is.na(fips_state_from_statename("text"))  # DOES return NA
      )
      expect_warning(fips_state_from_statename(3))  # probably should warn IT IS NOT TEXT
      expect_warning(fips_state_from_statename(c(NA, "Montana")))  # maybe should warn some are NA
    })
  })
})
#################################################################### #
# fips_states_in_eparegion

test_that("fips_states_in_eparegion", {

  test_all_stfips = stateinfo2$FIPS.ST[!is.na(stateinfo2$FIPS.ST)]

  expect_no_error({
    test_all_stfips_found = fips_states_in_eparegion(1:10)
  })
  expect_true(
    setequal(
      test_all_stfips,
      c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
        "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
        "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
        "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47",
        "48", "49", "50", "51", "53", "54", "55", "56", "60", "66", "69",
        "72", "74", "78")
    )
  )
  expect_setequal(
    test_all_stfips_found, test_all_stfips
  )

  for (region in 1:10) {
    expect_setequal(
      fips_states_in_eparegion(region),
      stateinfo2$FIPS.ST[stateinfo2$REGION %in% region]
      # name2fips(c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont"))
    )
  }

})
#################################################################### #
# fips_counties_from_statefips()  # should it be statefips or state_fips

test_that("fips_counties_from_statefips() works", {
  suppressWarnings({
    myst = stateinfo2$FIPS.ST[!is.na(stateinfo2$FIPS.ST)]
    myst = myst[!(myst %in% c(60,66,69,74,78))] # remove US territories since don't have counties
    expect_no_error({
      myst2 = fips_counties_from_statefips(myst)
    })
    expect_identical(
      sort(unique(substr(myst2, 1, 2))),
      myst
    )
    expect_true(5 == unique(nchar(myst2)))
    expect_true(all(fips_valid(myst2)))
    expect_true(length(myst2) > 3200)

  })
})
#################################################################### #
# fips_counties_from_state_abbrev()

test_that("fips_counties_from_state_abbrev() works", {
  suppressWarnings({
    suppressMessages({
      expect_true({
        all(fips2state_abbrev(
          fips_counties_from_state_abbrev(c("DE", "RI"))
        ) %in% c("DE", "RI"))
      })
      expect_true(
        unique(fips2state_abbrev( fips_counties_from_state_abbrev("pr") )) == "PR"
      )
      ## Commenting out since island areas are dropped from EJAM in v2.32
      # expect_true(
      #   unique(fips2state_abbrev( fips_counties_from_state_abbrev("vi") )) == "VI"
      # )
      # expect_true(
      #   unique(fips2state_abbrev( fips_counties_from_state_abbrev("GU") )) == "GU"
      # )
      # expect_true(
      #   unique(fips2state_abbrev( fips_counties_from_state_abbrev("MP") )) == "MP"
      # )

      expect_true(
        is.na(fips_counties_from_state_abbrev("UM"))#  NO data for U.S. Minor Outlying Islands
      )

      expect_warning(expect_warning(
        expect_warning(
          fips_counties_from_state_abbrev("text")
        )))
      expect_warning(expect_warning(
        expect_warning(fips_counties_from_state_abbrev(13))
      ))
      expect_warning(expect_warning(
        expect_warning(fips_counties_from_state_abbrev(c(NA, "RI")))
      ))

      expect_true(
        is.na(
          suppressWarnings(
            fips_counties_from_state_abbrev("text")
          )
        ) # PROBABLY SHOULD BUT DOES NOT RETURN NA, just empty
      )
    })
  })
})
#################################################################### #
# fips_counties_from_statename(   )  # should it be statename or state_name

test_that("fips_counties_from_statename() works", {
  suppressWarnings({
    suppressWarnings({
      suppressMessages({
        expect_true({
          all(fips2state_abbrev(
            fips_counties_from_statename(c("Montana", "District of Columbia"))
          ) %in% c("MT", "DC"))
        })
        expect_true(
          unique(fips2state_abbrev( fips_counties_from_statename("PUERto rico") )) == "PR"
        )
        ## Commenting out since island areas are currently dropped from EJAM in v2.32
        # expect_true(
        #   unique(fips2state_abbrev( fips_counties_from_statename("U.S. Virgin Islands") )) == "VI"
        # )
        # expect_true(
        #   unique(fips2state_abbrev( fips_counties_from_statename("GUAM") )) == "GU"
        # )
        # expect_true(
        #   unique(fips2state_abbrev( fips_counties_from_statename("Northern Mariana Islands") )) == "MP"
        # )

        expect_true(
          is.na(fips_counties_from_statename("U.S. Minor Outlying Islands"))  #  NO data for U.S. Minor Outlying Islands
        )

        expect_warning(expect_warning(expect_warning(fips_counties_from_statename("text"))))
        expect_true(
          suppressWarnings(
            is.na(fips_counties_from_statename("text"))   # DOES NOT RETURN NA, just empty
          )
        )
        expect_warning(expect_warning(expect_warning(fips_counties_from_statename(13))))
        expect_warning(expect_warning(expect_warning(fips_counties_from_statename(c(NA, "Montana")))))
      })
    })
  })
})
#################################################################### #
# fips_counties_from_countyname()

test_that("fips_counties_from_countyname() works", {
  suppressWarnings({
    suppressMessages({
      expect_identical(
        fips_counties_from_countyname("Harris County", "TX"),
        "48201"
      )
      expect_identical(
        "Baltimore County, MD",
        fips2countyname(fips_counties_from_countyname("Baltimore County, MD"))
      )
      expect_true(
        is.na((fips_counties_from_countyname("Har", "TX", exact = T)))
      )
      expect_true(
        all('county' == fipstype(fips_counties_from_countyname("Har", "TX", exact = FALSE)))
      )
      expect_true({
        length(
          fips2countyname(fips_counties_from_countyname("Har", "TX", exact = FALSE))
        ) == 5
        # fips_counties_from_countyname("Har",               "TX")    # finds 5 matches
        # fips_counties_from_countyname("Harris",            "TX")    # finds 2 matches
        # fips_counties_from_countyname("Harris ",           "TX")    # finds 1 match
        # fips_counties_from_countyname("Harris County, Texas", "TX") # finds 0 if state spelled out
      })
    })
  })
})

############################################################################# #
### fips_bgs_in_fips() ####

############################################################################# #
# convert any FIPS codes to the FIPS of all the blockgroups that are
#   among or within or containing those FIPS
# @   details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#
# Takes a vector of one or more FIPS that could be State (2-digit), County (5-digit),
#   Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips).
#
#   Returns unique vector of FIPS of all US blockgroups (including DC and Puerto Rico)
#   that contain any specified blocks, are equal to any specified blockgroup fips,
#   or are contained within any provided tract/county/state FIPS.
#
# @   param fips vector of US FIPS codes, as character or numeric,
#   with or without their leading zeroes, each with as many characters
# @   seealso [fips_lead_zero()]
# @   return vector of blockgroup FIPS (or NA values) that may be much longer than the
#   vector of fips passed to this function.
# @   examples
#   # all blockgroups in one state
#   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#   length(fips_bgs_in_fips("72"))
#   # all blockgroups in this one county
#   fips_bgs_in_fips(30001)
#   # all blockgroups that contain any of these 6 blocks (just one bg)
#   fips_bgs_in_fips( blockid2fips$blockfips[1:6])
#   # 2 counties
#   fips_bgs_in_fips(c(36009,36011))
############################################################################# #

test_that('fips_bgs_in_fips - by state', {
  expect_no_warning({val <- fips_bgs_in_fips(36)})
  expect_no_warning({val <- fips_bgs_in_fips("36")})
  expect_equal(length(val), 16070)
})
################## #
test_that('fips_bgs_in_fips - by county', {
  expect_no_warning({val <- fips_bgs_in_fips(36071)})
  expect_no_warning({val <- fips_bgs_in_fips("36071")})
  expect_equal(length(val), 292)
  # check it's the same as the subset of state codes
  x <- fips_bgs_in_fips("36")
  y <- x[which(startsWith(x, "36071"))]
  expect_equal(y, val)
})
################## #
test_that('fips_bgs_in_fips - by  tract', {
  expect_no_warning({val <- fips_bgs_in_fips(36071000100)})
  expect_no_warning({val <- fips_bgs_in_fips("36071000100")})
  expect_equal(length(val), 4)
  # check it's the same as the subset of state codes
  x <- fips_bgs_in_fips("36")
  y <- x[which(startsWith(x, "36071000100"))]
  expect_equal(y, val)
})
################## #
test_that('fips_bgs_in_fips - by  block group', {
  expect_no_warning({val <- fips_bgs_in_fips(360710001001)})
  expect_no_warning({val <- fips_bgs_in_fips("360710001001")})
  expect_equal(length(val), 1)
  # check it's the same as the subset of state codes
  x <- fips_bgs_in_fips("36")
  y <- x[which(startsWith(x, "360710001001"))]
  expect_equal(y, val)
})
################## #

# ACTUALLY     CANNOT use fips_bgs_in_fips() with city fips SINCE CDP IS NOT BROKEN INTO BGS EXACTLY

test_that('fips_bgs_in_fips - by CITY', {
  expect_warning({val <- fips_bgs_in_fips(3651000)})
  expect_warning({val <- fips_bgs_in_fips("3651000")})
  #expect_equal(length(val), 1)
  # check it's the same as the subset of state codes
  # x <- fips_bgs_in_fips("36")
  # y <- x[which(startsWith(x, "3651000"))]
  # expect_equal(y, val)
})

################## #
# returns only UNIQUE bg fips once each,
# even if 2 inputs contain or are inside same bg (turn into same bgid)
#  - do we want that to be the behavior? ***

test_that("fips_bgs_in_fips - returns only UNIQUE BGS in and/or containing the(se) fips", {
  expect_true({
    length(fips_bgs_in_fips(c("36071010801"))) == 3 # contains 3 unique blockgroups
  })
  expect_true({
    length(fips_bgs_in_fips(rep("36071010801", 5))) == 3 # will not return more matches than just unique
  })
  expect_true({
    length(fips_bgs_in_fips(rep("360710108011", 5))) == 1 # will not return more matches than just unique
  })
  expect_true({
    length(fips_bgs_in_fips(c(360710108011012, 360710108011006, 360710108011023))) == 1 # one unique bg returned even if it contains multiple blocks provided as query terms
  })
})

test_that('fips_bgs_in_fips - by BLOCK - uniques only - is that behavior we want?', {
  expect_true( {
    length(fips_bgs_in_fips(rep("360710108011", 5))) == 1
  })
  expect_no_warning({val <- fips_bgs_in_fips(c(360710108011012, 360710108011006, 360710108011023))})
  expect_no_warning({val <- fips_bgs_in_fips(c("360710108011012", "360710108011006", "360710108011023"))})
  expect_equal(length(val), 1)
})
################## #
test_that('fips_bgs_in_fips - leading zero addition', {

  expect_no_warning({val <- fips_bgs_in_fips("1055")}) # county
  expect_no_warning({val <- fips_bgs_in_fips(1055)})
  expect_equal(length(val), 90)
  expect_equal(substr(val[1], 1,2) , "01")

  expect_no_warning({val <- fips_bgs_in_fips("1")}) # state
  expect_no_warning({val <- fips_bgs_in_fips(1)})
  expect_equal(length(val), 3925)
  expect_equal(substr(val[1], 1,2) , "01")

  expect_no_warning({val <- fips_bgs_in_fips("1055011002")}) # tract
  expect_no_warning({val <- fips_bgs_in_fips(1055011002)})
  expect_equal(length(val), 3)
  expect_equal(substr(val[1], 1,2) , "01")

  expect_no_warning({val <- fips_bgs_in_fips(10690401001010)}) # not a bg
  expect_no_warning({val <- fips_bgs_in_fips("10690401001010")})
  expect_equal(length(val), 1) #
  expect_equal(substr(val[1], 1,2) , "01")
})
################## #
test_that('fips_bgs_in_fips - returns BGS in tract(s)', {
  tractfips1 <- "10005051900"
  expect_true(fipstype(tractfips1) == "tract") # tract as input,
  expect_true(all(fips_bgs_in_fips(tractfips1) %in% blockgroupstats$bgfips)) # returns actual bg fips
  expect_no_condition({val <- fips_bgs_in_fips(tractfips1)}) # tract that contains 3 bgs
  expect_equal(length(val), 3)
  expect_true(all(substr(val, 1, 11) == tractfips1))
  rm(tractfips1)
})
################## #
# > fipstype("blue")
# [1] "county"
# > fipstype("sdfsdfsdfasdf0")
# [1] "block"
### THESE RETURNED NULL, not NA:
# fips_bgs_in_fips("blue")
# fips_bgs_in_fips("36-071")
# fips_bgs_in_fips("36-07")
# fips_bgs_in_fips("$1001")

#  NO ERROR for invalid strings, no string cleaning (dashes/dots not removed)
test_that('fips_bgs_in_fips - NO ERROR if invalid text', {
  suppressWarnings({
    expect_no_error({val <- fips_bgs_in_fips("blue")})
    expect_no_error({val <- fips_bgs_in_fips("36-071")})
    expect_no_error({val <- fips_bgs_in_fips("36-07")})
    expect_no_error({val <- fips_bgs_in_fips("$1001")})
  })
  expect_equal(length(val), 0)
})

#  warnings for invalid strings, no string cleaning (dashes/dots not removed)
test_that('fips_bgs_in_fips - WARN if invalid text', {
  suppressWarnings({
    expect_warning({val <- fips_bgs_in_fips("blue")})
    expect_warning({val <- fips_bgs_in_fips("36-071")})
    expect_warning({val <- fips_bgs_in_fips("36-07")})
    expect_warning({val <- fips_bgs_in_fips("$1001")})
  })
})
################## #


#################################################################### #
## fips2...  ####
#################################################################### #

############################### #

# test f2p()

test_that("f2p pop for 1 state = its counties", {

  junk = capture.output({
    expect_equal(
      sum(f2p(fips_counties_from_state_abbrev('DE'))),
      f2p(name2fips('de'))
    )
  })

})


test_that("f2p pop for 1 tract = its blockgroups", {

  expect_equal(
    f2p("01055000900"),
    sum(f2p(blockgroupstats[substr(bgfips,1,11) %in% "01055000900", bgfips]))
  )

})

test_that("f2p pop ok for some blockgroups", {

  expect_true({
    all.equal(
      f2p(blockgroupstats[c(1000,2000,3000,4000), bgfips]),
      blockgroupstats[c(1000,2000,3000,4000), pop]
    )
  })

})
############################### #

# fips2pop()

test_that("fips2pop pop for 1 state = its counties", {

  junk = capture.output({
    expect_equal(
      sum(fips2pop(fips_counties_from_state_abbrev('DE'))),
      fips2pop(name2fips('de'))
    )
  })

})

test_that("fips2pop pop for 1 tract = its blockgroups", {

  expect_equal(
    fips2pop("01055000900"),
    sum(fips2pop(blockgroupstats[substr(bgfips,1,11) %in% "01055000900", bgfips]))
  )

})

test_that("fips2pop pop ok for some blockgroups", {

  expect_true({
    all.equal(
      fips2pop(blockgroupstats[c(1000,2000,3000,4000), bgfips]),
      blockgroupstats[c(1000,2000,3000,4000), pop]
    )
  })

})

test_that("fips2pop ok if multiple types", {

  # returns NA for block if this is not already loaded, but to ensure we test it....
  dataload_dynamic('blockid2fips')

  expect_no_error(

    x <- fips2pop(
      c(
        '01', # state
        '10001', # county
        "1377540", # city/cdp  # name2fips('Trion, GA'),
        "01055000900", # tract
        blockgroupstats$bgfips[4000], # bg
        "010090507011017" # block  blockid2fips$blockfips[12345]
      )
    )

  )
  expect_equal(length(x), 6)
  expect_true(x[1] > x[2])
  expect_true(x[5] > x[6])
})
############################### #


# fips_st2eparegion()  # fips_st2eparegion(fips_state_from_state_abbrev(stateinfo$ST))

test_that("fips_st2eparegion() works", {
  suppressWarnings({
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("ME")) == 1
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("NY")) == 2
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("VA")) == 3
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("GA")) == 4
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("IL")) == 5
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("LA")) == 6
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("KS")) == 7
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("MT")) == 8
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("ca")) == 9
    )
    expect_true(
      fips_st2eparegion(fips_state_from_state_abbrev("wa")) == 10
    )
    expect_true({
      all(fips_st2eparegion(stateinfo$FIPS.ST) %in% 1:10)
    })
  })
})
#################################################################### #
#    fips2state_abbrev(  )

test_that("fips2state_abbrev() works", {
  suppressWarnings({
    expect_identical(
      fips2state_abbrev(stateinfo$FIPS.ST),
      stateinfo$ST
    )
  })
})
#################################################################### #
#    fips2state_fips(    )

test_that("fips2state_fips() works", {
  suppressWarnings({
    TESTFIPS = c(2, "02170", 10001, "01003011103", "010030111031", "010030111031054")
    expect_identical(
      fips2state_fips(TESTFIPS),
      c("02", "02", "10","01" ,"01", "01")
    )
    expect_identical(
      fips2state_fips(1:5),
      c("01", "02", "03","04" ,"05")
    )
  })
})
#################################################################### #
#    fips2statename(     )

test_that("fips2statename() works", {
  suppressWarnings({
    TESTFIPS = c(2, "02170", 10001, "01003011103", "010030111031", "010030111031054")
    expect_identical(
      fips2statename(TESTFIPS),
      fips2statename( c("02", "02", "10","01" ,"01", "01") )
    )
    expect_warning(fips2statename(NA))
    expect_true(
      is.na(fips2statename(NA))  # returns United States instead of NA due to stateinfo2 table
    )
  })
})
#################################################################### #
#    fips2countyname()

test_that("fips2countyname() works", {
  suppressWarnings({
    expect_true({

      # REMOVE ISLAND AREAS SINCE THEIR FIPS ARE NOT QUITE LIKE COUNTY FIPS - FIRST 5 LETTERS ARE NOT UNIQUE "COUNTY"

      bg = blockgroupstats[!is.island(blockgroupstats$ST), c("countyname","bgfips", "ST")]
      first_bg_per_county = match(unique(bg$countyname), bg$countyname)

      bgfips_examples = bg$bgfips[first_bg_per_county]
      countyfips_eg = substr(bgfips_examples,1,5)
      their_countyname = paste0(
        bg$countyname[first_bg_per_county],
        ", ",
        bg$ST[first_bg_per_county])
      all.equal(
        fips2countyname(fips = countyfips_eg),
        their_countyname
      )

    })
    expect_warning(
      fips2countyname(NA)
    )
    expect_no_error(
      fips2countyname(NA)
    )
    expect_true(
      is.na(fips2countyname(10))
    )

  })
})
#################################################################### #
#  fips2tractname()

test_that("fips2tractname() works", {

  test_tracts = c("04019005200", "31047968600", "16031950600")
  # fipstype(test_tracts)

  suppressMessages({

    expect_equal(
      fips2tractname(test_tracts, prefix = "TRACT: ")
      ,
      paste0("TRACT: ", test_tracts)

    )
    expect_equal(
      fips2tractname(test_tracts)
      ,
      paste0("tract ", test_tracts)
    )

    expect_no_error(
      suppressWarnings({
        fips2tractname(NA)
      })
    )
    suppressWarnings(
      expect_warning(
        fips2tractname(NA)
      )
    )

    expect_true(
      suppressWarnings(
        is.na(fips2tractname(NA))
      )
    )

  })
})
#################################################################### #
#  fips2blockgroupname()


test_that("fips2blockgroupname() works", {

  test_bg = c("061110086021", "370850704022")
  # fipstype(test_bg)

  suppressMessages({

    expect_equal(
      fips2blockgroupname(test_bg, prefix = "PREFIX: ")
      ,
      paste0("PREFIX: ", test_bg)

    )
    expect_equal(
      fips2blockgroupname(test_bg)
      ,
      paste0("blockgroup ", test_bg)
    )

    expect_no_error(
      suppressWarnings({
        fips2blockgroupname(NA)
      })
    )
    suppressWarnings(
      expect_warning(
        fips2blockgroupname(NA)
      )
    )

    expect_true(
      suppressWarnings(
        is.na(fips2blockgroupname(NA))
      )
    )

  })
})

#################################################################### #
#    fips2name()    # inverse of name2fips()

test_that("fips2name() works", {
  suppressWarnings({

    # for states
    expect_true(
      all(fips2name(fips = fips_state_from_state_abbrev(stateinfo$ST)) == stateinfo$statename)
    )

    # for counties
    expect_true(
      identical(
        fips2name("10001"),
        "Kent County, DE"
      )
    )
    allcountyfips = fips_counties_from_statefips(stateinfo$FIPS.ST)
    expect_true(
      identical(
        fips2name(allcountyfips),
        fips2countyname(allcountyfips)
      )
    )
    expect_true({
      identical(
        fips2name(fips = fips_counties_from_statename("Delaware")),
        c("Kent County, DE", "New Castle County, DE", "Sussex County, DE")
      )
    })

    #
  })
})
#################################################################### #


################################################################### #
## fips_lead_zero() ####
################################################################### #
# Adds a leading zero to fips code if leading zero seems to be missing,
# and replaces with NA if length not plausible.
# @  param fips vector of numeric or character US FIPS codes
# @  return vector of same length

# Note it does NOT VALIDATE FIPS beyond
# checking length seems OK as-is or with an added leading 0.
#  ***  Should this function warn/ return NA if fips is invalid? i.e.,
#  if state code becomes 00 or state code is not in list of valid ones, or
#  next 3 digits are 000 or 1st 5 digits as adjusted are not in list of valid county fips, or
#  resulting longer fips is not in lists of valid tracts, blockgroups, blocks?
#  Only checks if it is a length that
#  might be a state, county, tract, blockgroup, or block FIPS code
#  already, or might be with one added leading zero.

# testfips16 = c("1", "12", "123", "1234", "12345", "123456", "1234567", "12345678", "123456789",
#                "1234567890", "12345678901", "123456789012", "1234567890123", "12345678901234",
#                "123456789012345", "1234567890123456")
# cbind(fipstype(testfips16), fips_lead_zero(testfips16), testfips16)
# testfips16
#
# [1,] "state"      "01"              "1"
# [2,] "state"      "12"              "12"
# [3,] NA           NA                "123"
# [4,] "county"     "01234"           "1234"
# [5,] "county"     "12345"           "12345"
# [6,] NA           NA                "123456"
# [7,] NA           NA                "1234567"
# [8,] NA           NA                "12345678"
# [9,] NA           NA                "123456789"
# [10,] "tract"      "01234567890"     "1234567890"
# [11,] "tract"      "12345678901"     "12345678901"  # AMBIGUOUS CASE - MIGHT BE BLOCK GROUP MISSING THE LEADING 0 ***
# [12,] "blockgroup" "123456789012"    "123456789012"
# [13,] NA           NA                "1234567890123"
# [14,] "block"      "012345678901234" "12345678901234"
# [15,] "block"      "123456789012345" "123456789012345"
# [16,] NA           NA                "1234567890123456"


test_that("fips_lead_zero correct for 1 through 16 digits long", {
  testfips16 = c(
    "1", "12",                      # 1 or 2 digits becomes 2 digit state fips
    "123",
    "1234", "12345",               # 4 or 5 digits becomes 5 digit county fips
    "123456", "1234567", "12345678", "123456789",
    "1234567890",     # 10  digits becomes 11 digit tract fips

    "12345678901",  # 11 is ambiguous and gets checked to see if real bg missing 0 or real tract.
    #### and if not valid, we just add a zero.

    "123456789012",     # 12 digits is already like a 12 digit blockgroup fips
    "1234567890123",
    "12345678901234","123456789012345", # 14 or 15 becomes 15 digit block fips
    "1234567890123456")
  suppressWarnings({
    expect_identical(
      fips_lead_zero(testfips16),
      c("01", "12",                       # state fips 2 digits
        NA,
        "01234", "12345",                # county fips 5 digits
        "0123456"  ,       "1234567",    # city/cdp fips  7 digits
        NA, NA,
        "01234567890",    #TrACT 11
        "012345678901",    # AMBIGUOUS IF NOT REAL FIPS AND 11 digits

        "123456789012",             # blockgroup fips 12 digits
        NA,
        "012345678901234", "123456789012345", # block 15 digits
        NA)
    )
  })
})

#################### # #################### #
test_that("negative, decimal, space, any non-digit means NA is returned", {
  junk = c(1, -1, 1.1, "-1", "1.1", "  1", "text", NA)
  suppressWarnings({
    expect_identical(
      fips_lead_zero(junk),
      c("01", NA,NA,NA,NA,NA,NA,NA)
    )
  })
})
#################### # #################### #
# test with 1 digit
#  meant to only add one zero at most, to create state-codes
# should it warn if the fips <1 or >78 (largest possible 2digit number code) ?

test_that('1 digit', {
  expect_no_warning({val <- fips_lead_zero("1")})
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero(1)})
  expect_equal(val, "01")

  expect_no_warning({val <- fips_lead_zero("0")})
  expect_equal(val, "00")
  expect_no_warning({val <- fips_lead_zero(0)})
  expect_equal(val, "00")
})
#################### # #################### #
# test with 2 digits
# it doesn't add any zeros since it infers this to be a state-code
test_that('2 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("01")}) # leading zero
  })
  expect_equal(val, "01")
  expect_no_warning({val <- fips_lead_zero(10)}) # numeric
  expect_equal(val, "10")
  expect_no_warning({val <- fips_lead_zero(01)}) #leading zero in numeric
  expect_equal(val, "01")
  suppressWarnings({
    expect_no_warning({fips_lead_zero("00")})  # DOES NOT FULLY VALIDATE SO DOES NOT KNOW 00 IS NOT ANY STATE'S FIPS
    expect_no_error({val <- fips_lead_zero("00")}) # zero string
  })
  expect_equal(val, "00")
})
#################### # #################### #
# test with 3 digits
#  A 3 digit fips is invalid since 1st two are state fips, which cannot be 00 - should it warn/ give NA? ***
test_that('3 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("001")}) # leading zero
    expect_warning({val <- fips_lead_zero("001")}) # leading zero
  })
  #expect_equal(val, "011")
  expect_true(is.na(val))
  suppressWarnings({
    expect_warning({val <- fips_lead_zero(100)}) # numeric
    expect_no_error({val <- fips_lead_zero(100)}) # numeric
  })
  #expect_equal(val, "100")
  expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(001)}) #leading zero in numeric
  expect_equal(val, "01")
  suppressWarnings({
    expect_warning({val <- fips_lead_zero("000")}) # zero string
    expect_no_error({val <- fips_lead_zero("000")}) # zero string
  })
  expect_true(is.na(val))
})
#################### # #################### #
# test with 4 digits
# adds one leading zero if needed to create 5-digit county-code
#  but note fips is invalid if first of 4 digits is zero (no state fips is 00) - should it warn/ give NA? ***
test_that('4 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("0001")}) # leading zero
    expect_no_warning({val <- fips_lead_zero("0001")}) # leading zero # zero string ### LENGTH SEEMS OK AND IT DOES NOT KNOW 00001 IS NO COUNTY'S FIPS
  })
  expect_equal(val, "00001")
  #expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000)}) # numeric
  expect_equal(val, "01000")
  #expect_true(is.na(val))
  expect_no_warning({val <- fips_lead_zero(0001)}) #leading zero in numeric
  expect_equal(val, "01")
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("0000")}) # zero string
    expect_no_warning({val <- fips_lead_zero("0000")}) # zero string ### LENGTH SEEMS OK AND IT DOES NOT KNOW 00000 IS NO COUNTY'S FIPS
  })
  expect_equal(val, "00000")
})
#################### # #################### #
# test with 5 digits
# adds no leading zero since it is inferred to be 5 digit county-code
test_that('5 digit', {
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("00001")}) # leading zero - invalid actual code, but format is valid - does not warn since not really validating fully
    expect_no_warning({val <- fips_lead_zero("00001")}) # leading zero- same
  })
  expect_equal(val, "00001")
  #expect_true(is.na(val))
  suppressWarnings({
    expect_no_warning({val <- fips_lead_zero(10000)}) # numeric
    expect_no_error({val <- fips_lead_zero(10000)}) # numeric
  })
  expect_equal(val, "10000")
  #expect_true(is.na(val))
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero(00001)}) #leading zero in numeric - invalid actual code, but format is valid - does not warn since not really validating fully
    expect_no_warning({val <- fips_lead_zero(00001)}) #leading zero in numeric - same
  })
  expect_equal(val, "01")
  suppressWarnings({
    expect_no_error({val <- fips_lead_zero("00000")}) # zero string - invalid actual code, but format (length) is valid
    expect_no_warning({val <- fips_lead_zero("00000")}) # same
  })
  expect_equal(val, "00000")
})
#################### # #################### #
# test with 6 digits
#    6 digits is city or other cdp -
# 6 digit numeric has problems... unless change options scipen...
#   100,000 gets converted to 1e+05 which is 5 digits and returned
# raise threshold with options(scipen = 999) and the test passes
# should add options(scipen = 0) to return back to default
test_that('6 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})
  suppressWarnings({
    testthat::expect_no_warning({val <- fips_lead_zero("000001")}) #
    expect_true(!is.na(val))
  })
  #
  expect_no_warning({val <- fips_lead_zero(100000)}) # numeric -------------------    1e+05  unless adjust options in which case it is NA and test passes
  expect_true(!is.na(val))
  suppressWarnings({
    expect_no_warning({val <- fips_lead_zero(100001)}) # numeric
    expect_true(!is.na(val))
  })
  suppressWarnings({
    expect_no_warning({val <- fips_lead_zero("000000")}) # zero string
    expect_true(!is.na(val))
  })
  options(scipen = 0)
})
#################### # #################### #
# test with 7 digits
# length is  city or other cdp
test_that('7 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("0000001")}) # leading zero
  expect_true(!is.na(val))
  expect_no_warning({val <- fips_lead_zero(3651000)}) # numeric
  expect_true(!is.na(val))
  expect_no_warning({val <- fips_lead_zero(1000001)}) # numeric
  expect_true(!is.na(val))
  expect_no_warning({val <- fips_lead_zero("0000000")}) # zero string
  expect_true(!is.na(val))

  options(scipen = 0)
})
#################### # #################### #
# test with 8 digits
# 8 characters is ALWAYS INVALID - cannot be a county (that is 5) nor can it be a tract (that is 11 including any leading zero)
test_that('8 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_warning({val <- fips_lead_zero("00000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(10000000)}) # numeric
  # expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(10000001)}) # numeric
  expect_true(is.na(val))

  expect_no_warning({val <- fips_lead_zero(00000001)}) #leading zero in numeric - drops the zeroes and then sees it as a 1 which is ok
  expect_equal(val, "01")
  expect_warning({val <- fips_lead_zero("00000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})
#################### # #################### #
# test with 9 digits
# ALWAYS INVALID -- cannot be a county (that is 5) nor can it be a tract (that is 11 including any leading zero)
test_that('9 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_warning({val <- fips_lead_zero("000000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(100000000)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(100000001)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero("000000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})
#################### # #################### #
# test with 10 digits
# adds a leading zero if needed to create 11 digit census-tract-code
test_that('10 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("0000000001")}) # leading zero
  expect_equal(val, "00000000001")
  expect_no_warning({val <- fips_lead_zero(1000000000)}) # numeric
  expect_equal(val, "01000000000")
  expect_no_warning({val <- fips_lead_zero(1000000001)}) # numeric
  expect_equal(val, "01000000001")
  expect_no_warning({val <- fips_lead_zero("0000000000")}) # zero string
  expect_equal(val, "00000000000")

  options(scipen = 0)
})
#################### # #################### #
# test with 11 digits
# AMBIGUOUS CASE - MIGHT BE A BLOCKGROUP MISSING A LEADING ZERO
# or A complete TRACT FIPS.
#
test_that('11 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  test_invalid_11 = "01234567891" # lead zero gets added and then it is called a blockgroup though invalid
  expect_no_warning(fips_lead_zero(test_invalid_11))
  expect_equal(fips_lead_zero(test_invalid_11), paste0("0", test_invalid_11))

  test_tract_missing0 =   4013116500   # 10 digits tract missing 0
  test_tract_good     = "04013116500"  # 11 digits full tract includes leading 0 !!!!!!!!!!
  test_bg_missing0    =   40131165002  # 11 digits blockgroup missing leading 0 !!!!!!!!!!
  test_bg_good        = "040131165002" # 12 digits full blockgroup

  expect_equal(fips_lead_zero(test_tract_missing0), paste0("0", test_tract_missing0))
  expect_equal(fips_lead_zero(test_tract_good), test_tract_good)
  expect_equal(fips_lead_zero(test_bg_missing0), paste0("0", test_bg_missing0))
  expect_equal(fips_lead_zero(test_bg_good), test_bg_good)

  # expect_no_warning({val <- fips_lead_zero("00000000001")}) # leading zero
  # expect_equal(val, "00000000001")
  # expect_no_warning({val <- fips_lead_zero(10000000000)}) # numeric
  # expect_equal(val, "10000000000")
  # expect_no_warning({val <- fips_lead_zero(10000000001)}) # numeric
  # expect_equal(val, "10000000001")
  # expect_no_warning({val <- fips_lead_zero("00000000000")}) # zero string
  # expect_equal(val, "00000000000")

  options(scipen = 0)
})
#################### # #################### #
# test with 12 digits
# adds no leading zero since it is inferred as a 12 digit block-group-code
test_that('12 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("000000000001")}) # leading zero
  expect_equal(val, "000000000001")
  expect_no_warning({val <- fips_lead_zero(100000000000)}) # numeric
  expect_equal(val, "100000000000")
  expect_no_warning({val <- fips_lead_zero(100000000001)}) # numeric
  expect_equal(val, "100000000001")
  expect_no_warning({val <- fips_lead_zero("000000000000")}) # zero string
  expect_equal(val, "000000000000")

  options(scipen = 0)
})
#################### # #################### #
# test with 13 digits
# ALWAYS INVALID LENGTH, SINCE BGFIPS IS 12 AND BLOCK IS 15
test_that('13 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_warning({val <- fips_lead_zero("0000000000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000000)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000001)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero("0000000000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})
#################### # #################### #
# test with 14 digits
# add leading zero  to create 15-digit block-code
test_that('14 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("00000000000001")}) # leading zero
  expect_equal(val, "000000000000001")
  expect_no_warning({val <- fips_lead_zero(10000000000000)}) # numeric
  expect_equal(val, "010000000000000")
  expect_no_warning({val <- fips_lead_zero(10000000000001)}) # numeric
  expect_equal(val, "010000000000001")
  expect_no_warning({val <- fips_lead_zero("00000000000000")}) # zero string
  expect_equal(val, "000000000000000")

  options(scipen = 0)
})
#################### # #################### #
# test with 15 digits
# add no leading zero since it is inferred to be a 15-digit block-code
test_that('15 digit', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_no_warning({val <- fips_lead_zero("000000000000001")}) # leading zero
  expect_equal(val, "000000000000001")
  expect_no_warning({val <- fips_lead_zero(100000000000000)}) # numeric
  expect_equal(val, "100000000000000")
  expect_no_warning({val <- fips_lead_zero(100000000000001)}) # numeric
  expect_equal(val, "100000000000001")
  expect_no_warning({val <- fips_lead_zero("000000000000000")}) # zero string
  expect_equal(val, "000000000000000")

  options(scipen = 0)
})
#################### # #################### #
# test with 16 digits
# ALWAYS INVALID LENGTH - 16 digits cannot be valid fips since 15 is block fips and longest we can interpret
test_that('16 digit +', {
  options(scipen = 999)
  on.exit({options(scipen = 0)})

  expect_warning({val <- fips_lead_zero("0000000000000001")}) # leading zero
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000000000)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero(1000000000000001)}) # numeric
  expect_true(is.na(val))
  expect_warning({val <- fips_lead_zero("0000000000000000")}) # zero string
  expect_true(is.na(val))

  options(scipen = 0)
})

options(scipen = 0)
#################### # #################### #
# test with invalid text input
test_that('warn on invalid text- cant coerce to numeric FIPS', {
  suppressWarnings({
    expect_warning({
      val <- fips_lead_zero("blue")
      ## revised that function so it now does warn in this case, but does return something like "0blue" still.
    })
  })
  expect_true(TRUE) # or it warns the test is empty
})

################################################################### #
################################################################### #


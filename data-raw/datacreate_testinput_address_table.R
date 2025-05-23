
#          datacreate_testinput_address_table.R

##  to see test objects and test files once they are created and the package is reloaded/installed/attached:
##
# cbind(data.in.package  = sort(grep("address", datapack()$Item, value = T)))
# cbind(files.in.package = sort(basename(testdata('address', quiet = T))))

##################### #

# test data names:

tnames <- c(
  
  # VECTORS:
  "testinput_address_parts",  # no numbering is relevant to this one
  "testinput_address_2",
  "testinput_address_9",
  
  # TABLES:
  "testinput_address_table_9",
  
  "testinput_address_table",            # no numbering like "_2" suffix in the name, so it can be easier to remember and is the easiest to use
  "testinput_address_table_goodnames",  # no numbering
  "testinput_address_table_withfull"    # no numbering
)
##################### #

# test data contents:

testinput_address_parts <- c("latitude", 
                             "LONGITUDE", 
                             "address", 
                             "STREET",
                             "City", 
                             "Statename", 
                             "zipcode")

testinput_address_2 <- c("1200 Pennsylvania Ave, NW Washington DC", "Research Triangle Park")

# create this table of 9 and then use that to create the vector of 9:

testinput_address_table_9 <- structure(list(
  
  FacilityID = c("17119110017423171", "18089110000397794", "18089110000397829", "18089110000398374", "18127110000607558",
                 "26163110027375668", "39017110000392557", "39035110011945681", "42003110001116934"),
  FacLong = c(-90.136, -87.45, -87.433, -87.328, -87.138, -83.157, -84.383, -81.673, -79.859),
  FacLat = c(38.694, 41.667, 41.671, 41.615, 41.637, 42.302, 39.483, 41.47, 40.395),
  FacilityName = c("US Steel-Granite City Works", "ArcelorMittal Indiana Harbor - West",
                   "ArcelorMittal Indiana Harbor - East", "US Steel-Gary Works",
                   "ArcelorMittal Burns Harbor LLC", "Dearborn-Works", "AK Steel Corporation - Middletown Works",
                   "ArcelorMittal Cleveland", "US Steel-Edgar Thompson"),
  Address = c("1951 State Street", "3001 Dickey Road", "3210 E Watling Street", "One North Broadway",
              "250 W. US Highway 12", "4001 Miller Road", "1801 Crawford Street",
              "3060 Eggers Avenue", "13th Street and Braddock Avenue"),
  City = c("Granite City", "East Chicago", "East Chicago", "Gary", "Burns Harbor", "Dearborn",
           "Middletown", "Cleveland", "Braddock"),
  State = c("IL", "IN", "IN", "IN", "IN", "MI", "OH", "OH", "PA"),
  ZipCode = c("62040", "46312", "46312", "46403", "46304-9745", "48120", "45043", "44105", "15104"),
  County = c("Madison County", "Lake County", "Lake County", "Lake County", "Porter County",
             "Wayne County", "Butler County", "Cuyahoga County", "Allegheny County")
), row.names = c(NA, -9L), class = "data.frame")

testinput_address_9 <- address_from_table(testinput_address_table_9)

testinput_address_table <- data.frame(
  Acol = 1:2,
  STREET =  c("1200 Pennsylvania Ave", "5 pARK AVE"),
  City = c("Washington", "NY"),
  statename = c("DC", "NY"),
  zipcode = c("", ""),
  other_column = 9:10
)

testinput_address_table_goodnames <- structure(list(
  Acol = 1:2,
  street = c("1200 Pennsylvania Ave", "5 pARK AVE"),
  city = c("Washington", "NY"), 
  state = c("DC", "NY"),
  zip = c("", ""),
  other_column = 9:10
), class = "data.frame", row.names = c(NA, -2L))

testinput_address_table_withfull <- data.frame(
  Address = testinput_address_2, 
  testinput_address_table
)
##################### #

# metadata ####

for (objectname in tnames) {
  assign(objectname, metadata_add(get(objectname)))
}
##################### #

# use_data ####

usethis::use_data(
  
  testinput_address_parts,
  testinput_address_2,
  testinput_address_9,
  
  testinput_address_table_9,
  testinput_address_table,
  testinput_address_table_goodnames,
  testinput_address_table_withfull,
  
  overwrite = TRUE
)
##################### #

# documentation ####

dataset_documenter("testinput_address_table",
                   title = "datasets for trying address-related functions",
                   description = paste0("datasets for trying address-related functions (", paste0(tnames, collapse = ", "), ")
#' @aliases ", paste0(tnames, collapse = " "))
)
##################### #

# write.xlsx ####

# use openxlsx::write.xlsx() instead of writexl package function called write_xlsx()
# writexl is zero dependency package for writing xlsx files that is light weight,
# but we already have imported openxlsx package to use more features like formatting it offers for xlsx download in app,
# so may as well just use that to write xlsx and maybe can avoid dependency on writexl.

savex <-  function(x, folder = "./inst/testdata", fname = "example.xlsx")  {
  if (!dir.exists(folder)) {stop("tried to save .xlsx but folder does not exist: ", folder)}
  fpath <- file.path(folder, fname)
  openxlsx::write.xlsx(x, file = fpath, overwrite = TRUE)
  if (!file.exists(fpath)) {stop("tried but could not save ", fpath)} else {cat("saved ", fpath, "\n")}
}
# testinput_address_parts is a vector not table
# testinput_address_2     is a vector not table
# testinput_address_9    is a vector not table

savex(testinput_address_table_9,          "./inst/testdata/address", "testinput_address_table_9.xlsx")
savex(testinput_address_table,            "./inst/testdata/address", "testinput_address_table.xlsx")
savex(testinput_address_table_goodnames,  "./inst/testdata/address", "testinput_address_table_goodnames.xlsx")
savex(testinput_address_table_withfull,   "./inst/testdata/address", "testinput_address_table_withfull.xlsx")

rm(savex)
##################### #

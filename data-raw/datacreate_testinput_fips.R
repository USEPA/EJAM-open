# datacreate_testinput_fips.R

################################## #

# 2 states

testinput_fips_states <- fips_state_from_state_abbrev(c("DE", "RI"))
# shp = shapes_from_fips(testinput_fips_states)
# mapfast(shp)
# dim(shp)
testinput_fips_states <- metadata_add(testinput_fips_states)
usethis::use_data(testinput_fips_states, overwrite = TRUE)
dataset_documenter(
  "testinput_fips_states",
  description = "Census FIPS codes vector as example of input, e.g., ejamit(fips = testinput_fips_states)"
)
################################## #

# 2 cities/towns

testinput_fips_cities <- c(2743000, 2743306)
# 2743000 # minneapolis city
# Minnetrista city     MN        Hennepin County 2743306
# shp = shapes_from_fips(testinput_fips_cities)
# mapfast(shp)
# dim(shp)
testinput_fips_cities <- metadata_add(testinput_fips_cities)
usethis::use_data(testinput_fips_cities, overwrite = TRUE)
dataset_documenter(
  "testinput_fips_cities",
  description = "Census FIPS codes vector as example of input, e.g., ejamit(fips = testinput_fips_cities)"
)
################################## #

# all counties in one state

testinput_fips_counties <- fips_counties_from_state_abbrev("DE")
## same as this:
# testinput_fips_counties <- read_csv_or_xl(testdata("counties_in_Delaware.xlsx", quiet = T))
# testinput_fips_counties <- testinput_fips_counties$countyfips

testinput_fips_counties <- metadata_add(testinput_fips_counties)
usethis::use_data(testinput_fips_counties, overwrite = TRUE)
dataset_documenter(
  "testinput_fips_counties",
  description = "Census FIPS codes vector as example of input, e.g., ejamit(fips = testinput_fips_counties)"
)
# ejamit(fips = testinput_fips_counties)
################################## #

# all tracts in one county

testinput_fips_tracts = unique(substr(
  fips_bgs_in_fips(
    fips_counties_from_state_abbrev("AR")[1]
  ), 1, 11))
# shp = shapes_from_fips(testinput_fips_tracts)
# mapfast(shp)
# dim(shp)
## [1] 8 4
testinput_fips_tracts <- metadata_add(testinput_fips_tracts)
usethis::use_data(testinput_fips_tracts, overwrite = TRUE)
dataset_documenter(
  "testinput_fips_tracts",
  description = "Census FIPS codes vector as example of input, e.g., ejamit(fips = testinput_fips_tracts)"
)
################################## #

# all blockgroups in one county

testinput_fips_blockgroups <- unique(fips_bgs_in_fips(testinput_fips_tracts))
# shp = shapes_from_fips(testinput_fips_blockgroups)
# mapfast(shp)
# dim(shp)
### [1] 14  4
testinput_fips_blockgroups <- metadata_add(testinput_fips_blockgroups)
usethis::use_data(testinput_fips_blockgroups, overwrite = TRUE)
dataset_documenter(
  "testinput_fips_blockgroups",
  description = "Census FIPS codes vector as example of input, e.g., ejamit(fips = testinput_fips_blockgroups)"
)
################################## #

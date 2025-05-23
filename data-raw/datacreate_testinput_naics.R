# datacreate_testinput_naics.R

# no folder in testdata for this type
# since cannot upload table of that type to app and they cannot be direct inputs to ejamit()
#  but may want testdata objects to try out relevant functions that turn these into latlon

testinput_naics <- c(3366, 33661, 336611)
testinput_naics <- metadata_add(testinput_naics)
# latlon_from_naics(testinput_naics)
usethis::use_data(testinput_naics, overwrite = TRUE)
dataset_documenter(
  "testinput_naics",
  description = "NAICS codes vector as example of input, e.g., latlon_from_naics(testinput_naics)"
)

# latlon_from_naics(testinput_naics)

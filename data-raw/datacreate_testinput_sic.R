# datacreate_testinput_sic.R

# no folder in testdata for this type
# since cannot upload table of that type to app and they cannot be direct inputs to ejamit()
#  but may want testdata objects to try out relevant functions that turn these into latlon

testinput_sic <- c('6150', '6300', '5995')
testinput_sic <- metadata_add(testinput_sic)
# latlon_from_sic(testinput_sic)
usethis::use_data(testinput_sic, overwrite = TRUE)
dataset_documenter(
  "testinput_sic",
  description = "SIC codes vector as example of input to e.g., latlon_from_sic(testinput_sic)"
)

# latlon_from_sic(testinput_sic)

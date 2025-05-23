# datacreate_testinput_mact.R

# no folder in testdata for this type
# since cannot upload table of that type to app and they cannot be direct inputs to ejamit()
#  but may want testdata objects to try out relevant functions that turn these into latlon

testinput_mact <- "OOOO"
testinput_mact <- metadata_add(testinput_mact)
# latlon_from_mactsubpart(testinput_mact)[1:5,]
usethis::use_data(testinput_mact, overwrite = TRUE)
dataset_documenter(
  "testinput_mact",
  description = "MACT subpart code as example of input, e.g., latlon_from_mactsubpart(testinput_mact)"
)

# head(latlon_from_mactsubpart(testinput_mact))

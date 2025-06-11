
testoutput_ejamit_fips_counties <- ejamit(fips = testinput_fips_counties)
testoutput_ejamit_fips_counties <- metadata_add(testoutput_ejamit_fips_counties)
usethis::use_data(testoutput_ejamit_fips_counties, overwrite = TRUE)
dataset_documenter("testoutput_ejamit_fips_counties", description = "This is the output of ejamit(fips = testinput_fips_counties)",
                   seealso = "[ejamit()] [testdata()]")

testoutput_ejamit_fips_cities <- ejamit(fips = testinput_fips_cities)
testoutput_ejamit_fips_cities <- metadata_add(testoutput_ejamit_fips_cities)
usethis::use_data(testoutput_ejamit_fips_cities, overwrite = TRUE)
dataset_documenter("testoutput_ejamit_fips_cities", description = "This is the output of ejamit(fips = testinput_fips_cities)",
                   seealso = "[ejamit()] [testdata()]")

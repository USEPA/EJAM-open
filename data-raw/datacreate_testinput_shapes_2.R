
# create dataset(s) as examples of inputs to ejamit() etc.

################################# #

# testshapes_2 ####

testshapes_2 <- shapes_from_fips(fips = name2fips(c('riverview,de', 'viola,de')))    # 6 KB as .zip
testshapes_2 <- metadata_add(testshapes_2)
usethis::use_data(testshapes_2, overwrite = TRUE)
dataset_documenter('testshapes_2', description = "Sample shapefile/polygon data as spatial data.frame",
                   seealso = "same as [testinput_shapes_2]"
                   )

################################# #

# testinput_shapes_2 ####

# an alias / copy of testshapes_2

testinput_shapes_2 <- testshapes_2
testinput_shapes_2 <- metadata_add(testinput_shapes_2)
usethis::use_data(testinput_shapes_2, overwrite = TRUE)
dataset_documenter('testinput_shapes_2', description = "Sample shapefile/polygon data as spatial data.frame",
                   seealso = "same as [testshapes_2]")

# save as .zip shapefile in testdatafolder()
shape2zip(testinput_shapes_2, './inst/testdata/shapes/testinput_shapes_2.zip')

################################# #

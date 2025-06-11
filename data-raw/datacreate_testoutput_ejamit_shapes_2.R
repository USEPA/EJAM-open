
testoutput_ejamit_shapes_2 <- ejamit(shapefile = testinput_shapes_2, radius = 0)
testoutput_ejamit_shapes_2 <- metadata_add(testoutput_ejamit_shapes_2)
usethis::use_data(testoutput_ejamit_shapes_2, overwrite = TRUE)
dataset_documenter("testoutput_ejamit_shapes_2", description = "This is the output of ejamit(shapefile = testinput_shapes_2, radius = 0)",
                   seealso = "[ejamit()] [testdata()]")

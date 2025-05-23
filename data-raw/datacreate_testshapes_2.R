
# testshapes_2 <- shapes_from_fips(fips = name2fips(c('tucson,az', 'tempe, AZ')))
testshapes_2 <- shapes_from_fips(fips = name2fips(c('riverview,de', 'viola,de')))    # 6 KB as .zip
# testshapes_2 <- shapes_from_fips(fips = fips_counties_from_state_abbrev('DE')[1:2])

# save as package dataset R object for lazy loading
testshapes_2 <- metadata_add(testshapes_2)
use_data(testshapes_2, overwrite = T)

dataset_documenter('testshapes_2', description = "Sample shapefile/polygon data as spatial data.frame")

# save as .zip shapefile in testdatafolder()
shape2zip(testshapes_2, './inst/testdata/shapes/testshapes_2.zip')



################################# #

# mapfast(testshapes_2) # works now
# map_shapes_leaflet(testshapes_2) # works now
## mapview  ::   mapview(testshapes_2)  #  has nice basemap selection button
# leaflet(testshapes_2) %>% addPolygons() %>% addTiles() # not needed
## map_shapes_mapview(testshapes_2) # not needed
## map_shapes_plot(testshapes_2)  # not useful

################################# #

# out = ejamit(shapefile = testshapes_2)

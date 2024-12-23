#################################################################### #

# Actually this specifies pins board info on 'title' and 'description' 
# but NOT what pins pkg calls metadata.
# The actual pins metadata parameter will note the ejscreen version 
# when the object is being written to the pins board using `pin_write(metadata = )`

metadata4pins <- list(
  
  c( 
    varlist = "frs", name = "frs",
    title = "frs data from EJScreen for EJAM", 
    description = "data.table -- See documentation in EJAM package"
  ),
  c( 
    varlist = "frs_by_programid", name =  "frs_by_programid",
    title = "frs_by_programid data from EJScreen for EJAM", 
    description = "data.table -- See documentation in EJAM package"
  ),
  c(
    varlist = "frs_by_naics", name = "frs_by_naics",
    title = "frs_by_naics data from EJScreen for EJAM", 
    description = "data.table -- See documentation in EJAM package" 
  ),
  c(
    varlist = "frs_by_sic", name = "frs_by_sic",
    title = "frs_by_sic data from EJScreen for EJAM", 
    description = "data.table -- See documentation in EJAM package"      
  ),
  c(
    varlist = "frs_by_mact", name = "frs_by_mact",
    title = "frs_by_mact data from EJScreen for EJAM", 
    description = "data.table -- See documentation in EJAM package"   
  ),
  c(
    varlist = "bgej", name = "bgej",
    title = "bgej data from EJScreen for EJAM", 
    description = "data.frame -- approx 243k blockgroups, like blockgroupstats but for EJ Index raw scores, with bgfips, bgid, etc. - See documentation in EJAM package"
  ),
  c(
    varlist = "bgid2fips", name = "bgid2fips",
    title = "bgid2fips data for EJAM", 
    description = "data.table of approx 242k blockgroups with Census FIPS for each blockgroup ID - See documentation in EJAM package"
  ),
  c(
    varlist = "blockid2fips", name = "blockid2fips",
    title = "blockid2fips data for EJAM", 
    description = "data.table of approx 8 million Census blocks with Census FIPS for each block ID - See documentation in EJAM package" 
  ),
  c(
    varlist = "blockpoints", name = "blockpoints",
    title = "blockpoints data for EJAM", 
    description = "data.table of approx 8 million Census blocks with blockid, lat, lon - See documentation in EJAM package"
  ),
  c(
    varlist = "quaddata", name = "quaddata",
    title = "quaddata data for EJAM", 
    description = "data.table of approx 8 million Census blocks with BLOCK_X, BLOCK_Z, BLOCK_Y, blockid, used to create index of all US block point locations - See documentation in EJAM package"
  ),
  c(
    varlist = "blockwts", name = "blockwts",
    title = "blockwts data from EJScreen for EJAM", 
    description = "data.table of approx 8 million Census blocks with blockid, bgid, blockwt, block_radius_miles - See documentation in EJAM package" 
  ),
  
  
  c(
    varlist = "blockgroupstats", name = "blockgroupstats",
    title = "blockgroupstats data from EJScreen for EJAM", 
    description = "data.table of approx 243,022 Census blocks with over 100 demographic and environmental indicators - See documentation in EJAM package" 
  ),
  c(
    varlist = "bgpts", name = "bgpts",
    title = "block group centroids data, from Census for EJAM", 
    description = "data.table of approx 242,335 Census block groups, with bgid, lat, lon, bgfips, and blockcount - See documentation in EJAM package" 
  ),
  c(
    varlist = "states_shapefile", name = "states_shapefile",
    title = "US States boundaries shapefile data from Census for EJAM", 
    description = "shapefile of bounds of States for use in EJAM - See documentation in EJAM package" 
  )
  
)

metadata4pins <- do.call(rbind, args = metadata4pins)
metadata4pins = data.frame(metadata4pins)

# attr(metadata4pins, "date_saved_in_package") <- as.character(Sys.Date()) # this should be a date assigned only when datacreate_ script is run for a specific dataset to create it.
usethis::use_data(metadata4pins, overwrite = TRUE)
dataset_documenter('metadata4pins', 
                   title = "metadata about data object, for pins", 
                   details = "metadata such as description of dataset, for pins board data in EJAM package")
#################################################################### #

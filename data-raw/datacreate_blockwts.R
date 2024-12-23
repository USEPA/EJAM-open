
# The block data.table tables
#  'blockpoints', 'blockwts', 'quaddata', 'blockid2fips' 
# need to be updated when FIPS codes or boundaries of blockgroups or blocks change.

# They are created as follows: 

# for version 2.32 (in July/August 2024)
# they can be created from the .gdb.zip file that was obtained from the EJScreen team
message("The 'datacreate_blockwts.R' script does not include Island Areas GU VI MP AS, since they lack almost all indicator data in EJScreen v2.32")

# for version 2.2 (through July/August 2024)
# they were created from Census 2020 data
# using a non-CRAN package (in github) called census2020download ! 
# This script can try to install that and use it.
#
#################################################################################### # 

# setup ####

require(data.table)
if (!exists("askquestions")) {askquestions <- FALSE}

# do_update = TRUE

if (interactive() && askquestions) {
  do_update <- askYesNo("Update all block tables like blockpoints, blockwts, etc.? 
  (Must update if FIPS codes or boundaries of blockgroups or blocks changed)")
  if (is.na(do_update)) {do_update <- FALSE}
} else {
  do_update <- TRUE
}

# do_metadata = TRUE

if (interactive() && askquestions) {
  do_metadata <- askYesNo("Do metadata_add()  ?")
  if (is.na(do_metadata)) {do_metadata <- FALSE}
} else {
  do_metadata <- TRUE
}

# # do_data_save = FALSE
# 
# if (interactive() && askquestions) {
#   do_data_save <- askYesNo("Do use_data() - (PROBABLY NO)?")
#   if (is.na(do_data_save)) {do_data_save <- FALSE}
# } else {
#   do_data_save <- FALSE
# }
######################################### #

# A) use downloads from Census Bureau? (install,load census2020download pkg) ####

# tryinstall = FALSE

################################## #
warning("Note that the file from EJScreen team lacks area info for blocks, while Census Bureau downloads would have that.
            Relying only on the file from EJScreen team, we cannot calculate block_radius_miles
            as would be need to create custom nationwide proximity scores like proxistat() does.
            Also, it seems but should be confirmed that EJScreen does not do small-distance adjustments with that in buffer analysis,
            and if they did that it is not clear where they would get the block size info.")
### IMPORTANT NOTE ON block size info and proximity scores vs doaggregate() reports:
###
### POSSIBLE LIMITATION OF relying on manually obtained weights table from ejscreen team
### is that calculating custom proximity scores like proxistat() does require area of blocks
### to do small distance adjustments, but the file from EJScreen team lacks that info.
### Weight Table below, does NOT have info on size (square meters) of blocks.
### FILE LACKS area, SO CANNOT CALCULATE block_radius_miles that EJAM 2.2 used.
### So custom proximity scores like proxistat() will not be possible without the full info from Census Bureau.
### when effective radius adjustment turns out to be needed, 
### then we need to get area from another source, i.e., the Census 2020 data
### via the census2020download package, as above.

### At least for buffer aggregation (as in ejamit() or shiny app) reports,
### EJScreen/EJAM v2.32 will not actually use that in buffer analysis - 
### Should confim EJScreen does NOT do the small distance adjustment for buffer aggregation,
### even though it was doing that adjustment in the calculation of blockgroup proximity scores once a year in EJScreen.
### We can now put in placeholder like NA values for now as long as doaggregate() is prevented
### from using block_radius_miles in its calculations, via   
###   use_adjusted_distance = FALSE 
################################## #

if (interactive() && askquestions) {
  do_download_from_census <- askYesNo("Download block data from Census using package census2020download? (say no if you have a local copy of the weights file as gdb.zip from EJScreen)")
  if (is.na(do_download_from_census)) {stop('Cancel - halting script')} # {do_download_from_census <- FALSE}
} else {
  do_download_from_census <- FALSE
}
if (!do_update) {
  message("block data not updated") # end of script is here, in that case.
} else {
  if (do_download_from_census) {
    
    message("Trying to download block data from Census using package census2020download")
    # if using census2020download pkg
    # check if right pkg is installed so cando this
    if (require(census2020download)) {
      cando <- TRUE
      cat('attached census2020download package\n')
    } else {
      ## tryinstall ####
      if (interactive() && askquestions) {
        tryinstall = askYesNo("Requires the census2020download package, not found installed yet. Try to install now from github?")
        if (is.na(tryinstall)) {tryinstall <- FALSE}
      } else {
        warning('Cannot update block data -- That requires the census2020download package from\n devtools::install_github("ejanalysis/census2020download") ')
        tryinstall <- FALSE
        cando <- FALSE
      }
      if (tryinstall) {
        devtools::install_github("ejanalysis/census2020download")
        if (!require(census2020download)) {
          cando <- FALSE
          warning('failed to install necessary package from  devtools::install_github("ejanalysis/census2020download")')
        } else {
          cat('installed and attached census2020download package\n')
          cando <- TRUE
        }
      } else {
        warning('Not updating block data -- that requires the census2020download package from\n devtools::install_github("ejanalysis/census2020download") ')
        cando <- FALSE
      }
    }
    ################################################ #
    
    if (cando)  {
      ## census2020_get_data() ####

      cat('Downloading block data and preparing block tables for EJAM...')
      blocks <- census2020download::census2020_get_data()
      cat("done downloading block tables:\n")
      print(cbind(names(blocks)))
      message("block data downloaded")
      got_download <- TRUE
    } else {
      message("block data not downloaded")
      got_download <- FALSE
    }
    ################################################ #
  } else {
    
    # B) use file from EJScreen team, not Census Bureau ####
    
    # WeightTables_2020_CT2022_fix.gdb.zip file was provided directly by the EJScreen team for use here.
    # WeightTables_2020_CT2022_09032024.gdb.zip file was provided directly as an update on 9/4/24
    # (corrected for missing 19 blockgroups)
    # From	xxxxxxxxxx@epa.gov
    # Sent On	9/4/24 9:31:29 AM
    # Subject	 Updated Weight Table
    # Message	 Those “missing” 19 BGs came out the ID misassignment for about 50,000 blocks for CT. Their IDs were incorrectly assigned. Census doesn't have 2022 block IDs for CT. They were generated based on 2022 BG IDs. Thanks for reporting them! The update was made to the block weight table. Let us know if you have any questions.
    # 
    # importing the downloaded gdb.zip file of block weights used by EJScreen version 2.32
    message("Not downloading block data from Census using package census2020download")
    message("Trying to use a local file that had been manually obtained from EJScreen team as gdb.zip")

    mydir = "~/../Downloads"
    # fname = "WeightTables_2020_CT2022_fix.gdb.zip" # had 19 problems
    fname = "WeightTables_2020_CT2022_09032024.gdb.zip" # new, 9/4/24
    fpath = file.path(mydir, fname)
    fpath = rstudioapi::selectFile(path = fpath, filter = "gdb.zip (*.gdb.zip)")
    if (!file.exists(fpath)) {stop("file not found: ", fpath)}
    # library(EJAM)
    library(sf)
    print(st_layers(fpath))
    # >  st_layers(fpath)
    
    #### BEFORE 9/4/24:
    # Driver: OpenFileGDB 
    # Available layers:
    #                        layer_name geometry_type features fields crs_name
    # 1  BLOCK20_CENTROIDS_WEIGHTS_2022         Point  8174955      6   WGS 84
    # 2 COUNTY20_CENTROIDS_WEIGHTS_2022         Point     3222      6    NAD83
    # 3     BG20_CENTROIDS_WEIGHTS_2022         Point   242335      6    NAD83
    # 4  TRACT20_CENTROIDS_WEIGHTS_2022         Point    85395      6    NAD83
    
    #### AFTER 9/4/24:
    
    
    ## read the layer of interest
    layname = "BLOCK20_CENTROIDS_WEIGHTS_2022"
    if (!layname %in% st_layers(fpath)$name) {stop("layer not found: ", layname)}
    cat("reading large file, this may take a while...\n")
    x <- st_read(fpath, layer = layname)
    # Simple feature collection with 8174955 features and 6 fields
    # Geometry type: POINT
    # Dimension:     XY
    # Bounding box:  xmin: -179.1084 ymin: 17.88513 xmax: 179.6212 ymax: 71.3984
    # Geodetic CRS:  WGS 84
    ################################################ #
    # names(x)
    # [1] "GEOID"       "STCNTRBG"    "POP_WEIGHT"  "HOU_WEIGHT"  "AREA_WEIGHT" "PLFIPS"      "Shape" 
    # > dim(x)
    # [1] 8174955       7
    pts <- data.frame(sf::st_coordinates(x))
    colnames(pts) <- c("lon", "lat")
    
    
    ## note the pop or area weights do not always add up to 1 for a blockgroup, for some reason:
    ## which implies there are a few errors in the FIPS? or some FIPS are not 12 digits?
    # ***
    # y = setDT(copy(x))
    # y[ , Shape := NULL ]
    # y[, bgfips := substr(GEOID, 1, 12)]    
    # y[ , bgareawt := sum(AREA_WEIGHT), by = bgfips]
    # table(round(y$bgareawt,2))
    # 
    ## about 13k blocks have zero total pop wt for bgwt !
    ## a few dozen have nonzero but <1.00 as total bgwt ?!
    #   y[ , bgpopwt := sum(POP_WEIGHT), by = bgfips]
    #  table(round(y$bgpopwt,2))
    # rm(y)
    
    
    blocks <- data.table::data.table(blockfips = x$GEOID, 
                         lat = pts$lat, lon = pts$lon, 
                         
                         blockwt = x$POP_WEIGHT, # already created so census2020download::census2020_save_datasets() wont do that again
                         pop = NA_integer_, # census2020download::census2020_save_datasets() will see this and not try to use it to calculate blockwt 

                         area = 0)
    # *** area is missing but census2020_save_datasets() 
    # has used it / needed it to create block_radius_miles !! 
    # unless we ignore that via use_adjusted_distance = FALSE, which might actually be what ejscreen itself actually does?
                         # Keep total area, but not land and water separately:
                         # x[ , area := .(arealand + areawater)]
    
    # cols_to_keep = c("blockfips", "lat", "lon", "pop", "area") 
    # are the ones normally created by census2020download::census2020_get_data()
    # and needed as input to census2020download::census2020_save_datasets()
    # renamed via census2020download::census_col_names_map
    
    data.table::setkey(blocks, 'blockfips')
    
    got_download <- TRUE
  } # end of USING gdb file manually downloaded, not the  census2020download pkg 
  
  if (got_download) {
    
    # census2020_save_datasets() ####
    cat('Saving block tables to EJAM package...')
    
    mylistoftables <- census2020download::census2020_save_datasets(
      blocks, 
      add_metadata = do_metadata,
      save_as_data_for_package = do_data_save,  
      overwrite = TRUE
    )
    # cat("done creating 'mylistoftables', a list of block tables as datasets.")
    
    bgid2fips    <- mylistoftables$bgid2fips
    blockid2fips <- mylistoftables$blockid2fips
    blockpoints  <- mylistoftables$blockpoints
    blockwts     <- mylistoftables$blockwts
    quaddata     <- mylistoftables$quaddata
    
    for (myvar in names(mylistoftables)) {
      if (rstudioapi::isAvailable()) {rstudioapi::documentOpen(paste0('./R/data_', myvar, '.R'))}
    }
    cat("REMEMBER TO MANUALLY UPDATE THE DOCUMENTATION IN data_bgid2fips.R 
        but use NULL at end since it is not saved in package so it is not an exported object\n")
    # OR USE 
    # dataset_documenter('bgid2fips', saveinpackage = FALSE)
    
    cat("REMEMBER TO MANUALLY UPDATE THE DOCUMENTATION IN data_blockid2fips.R
    but use NULL at end since it is not saved in package so it is not an exported object\n")
    # OR USE 
    # dataset_documenter('blockid2fips', saveinpackage = FALSE)
    
    cat("REMEMBER TO MANUALLY UPDATE THE DOCUMENTATION IN data_blockpoints.R
        but use NULL at end since it is not saved in package so it is not an exported object\n") 
    # OR USE 
    # dataset_documenter('blockpoints', saveinpackage = FALSE)
    
    cat("REMEMBER TO MANUALLY UPDATE THE DOCUMENTATION IN data_blockwts.R 
        but use NULL at end since it is not saved in package so it is not an exported object\n")
    # OR USE 
    # dataset_documenter('blockwts', saveinpackage = FALSE)
    
    cat("REMEMBER TO MANUALLY UPDATE THE DOCUMENTATION IN data_quaddata.R
        but use NULL at end since it is not saved in package so it is not an exported object\n")
    # OR USE 
    # dataset_documenter('quaddata', saveinpackage = FALSE)
    
    # want to keep the objects in memory? OK - may use to run EJAM functions like recreating testoutput data
    rm(mylistoftables, blocks)
    gc()
  }
}
#################################################################################### # 












# browseURL("https://ejanalysis.github.io/census2020download/reference/blockpoints.html")
# browseURL("https://ejanalysis.github.io/census2020download/reference/blockwts.html")
# browseURL("https://ejanalysis.github.io/census2020download/reference/index.html")

# devtools::install_github("ejanalysis/census2020download")
# require # (census2020download)

# ?census2020download   # the name of the package  
# ?census2020_download  # the name of the function
# Sys.time()
# blocks <- census2020_get_data()  # takes minutes -- has to download each state, etc.
# Sys.time()
# xlist  <- census2020_save_datasets(blocks, save_as_data_for_package = FALSE)
#
# rm(blocks)
# names(xlist)

# Those block data.table objecs were then saved via pins board 

#################################################################################### # 

# The dataset used by EJAM called blockwts has a column called 
# block_radius_miles that is what the radius would be if the block were circular, 
# and it was created based on 
# 
# block_radius_miles = sqrt(area / pi) 
#
## because   area = pi * block_radius_miles^2 
#
# where area is in square miles and is the sum of land and water area from Census data.

#################################################### #

# Excerpt from 2017 tech doc on using min dist to create proximity scores
#  but the same idea applies if EJAM is
#  calculating and reporting distance from each site to avg resident in each block and then bg) ---
#  ########### #
#   Since we cannot easily find out how the
#   residents are actually distributed in those areas, we made two simplifying assumptions:
#   - residents are evenly distributed across the surface area of each block, and
#  - each block can be represented by a circle whose radius is 

#  block_radius_miles  =  sqrt(area / pi) 
#
#  We call this latter value the "Block Area Equivalent Radius."
#
#  area = pi * block_radius_miles^2 

#  Our investigations indicate that for any dij less than the Block Area Equivalent Radius, 0.9 times that
#  value is a reasonable representation of the average distance from the facility for all residents in the
#  block. We call this the dij corrected.
#  Our computational scheme determines the dij values as described above, tests for the comparison with
#  Block Area Equivalent Radius, and substitutes dij corrected values. We found that we needed to make
#  that correction for less than 1% of all facility/block combinations in an early testing dataset that used
#  2005-2009 ACS data.
#  ########### #

#    HOWEVER, a significant % of blocks have effective radius of at least 1 mile radius,
#    which is an area of at least pi*R^2 = 3.14 square miles area.

# See separate notes/ analysis in archived   dev\block counts near FRS sites
# and some of that in the 4_advanced.Rmd

#################################################################################### # 



#' utility - write data objects to pins board in .arrow format
#' 
#' @param varnames vector of quoted names of datasets, like c("bgej", "bgid2fips")
#' @param justchecking can set to TRUE to just see a list of what pins are stored in that board
#' @param silent set to TRUE to suppress cat() msgs to console
#' @param type "arrow"
#' @param access_type "all" means public (if Posit Connect instance allows that)
#' @param boardfolder if needed to specify a different folder than default
#' @param auth See help documentation for [pins::board_connect()]
#' @param server if needed to specify a server other than default (which might be
#'   stored in envt variable CONNECT_SERVER or be registered via the rsconnect package).
#'   Note if auth = "envvar" then it looks for CONNECT_SERVER to get name of server which
#'   needs to be the full url starting with https:// - see help for board_connect
#' 
#' @import pins
#' @import arrow
#' @return varnames vector
#' 
datawrite_to_pins = function(
    
  varnames = c(
    'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
    'bgej', 'bgid2fips',
    'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"),

  type = "arrow",  

  justchecking = FALSE,
  silent = FALSE,
  
  access_type = "all",
  boardfolder = "Mark",
  auth = "auto",
  server = "https://rstudio-connect.dmap-stage.aws.epa.gov"
) {
  
  # library(data.table)
  # library(magrittr)
  
  if (!all(is.character(varnames))) {
    ok = FALSE
    varnames = deparse(substitute(varnames))
    if (all(sapply(varnames, exists))) {
      if (interactive()) {
        ok <- askYesNo(paste0("looks like you provided unquoted object names ... do you mean '", varnames[1],"' etc.?"))
        if (is.na(ok)) {ok <- FALSE} # clicked cancel
        warning("varnames must be a character vector of quoted names of objects like c('x', 'y') \n")
        if (!ok) {return(NULL)}
      }}
    if (!ok) {
      stop("varnames must be a character vector of quoted names of objects like c('x', 'y') ")
    }}
  ####################################################### #
  if (!justchecking) {
  # Metadata for pins board entry ####
  
  ### this is the metadata already inside each object (each dataset)
  capture.output({
    meta <- metadata_add(0)
    })
  ### this is the metadata to write to the pins board:
  ###  A list containing additional metadata to store with the pin. 
  ###  When retrieving the pin, this will be stored in the user key, to avoid potential clashes with the metadata that pins itself use.
  meta <- list(
    date_pins_updated = c(pinsUploadDate = as.character(Sys.Date())),  
    # redundant - created date already stored by pins but ok
    ejscreen_version = attr(meta, "ejscreen_version")
  )
  }
  ####################################################### #
  # Check access to pins board ####
  #
  # https://rstudio-connect.dmap-stage.aws.epa.gov/connect/#/content/listing?q=is:published+type:pin+owner:Mark
  cat("\n")
  if (auth == "rsconnect") {
    # ignore server default here. use server and key already configured for rsconnect.
    board <- tryCatch(pins::board_connect(auth = "rsconnect", versioned = TRUE), 
                      error = function(e) {e})
  } else {
    board <- tryCatch(pins::board_connect(server = server, auth = auth, versioned = TRUE),
                      error = function(e) {e})
  }
  if (inherits(board, "error")) {
    board_available <- FALSE
    if (!silent) {cat("Failed trying to connect to pins board server.\n\n")}
  } else {
    board_available <- TRUE
    if (!silent) {cat("Successfully connected to Posit Connect pins board.\n\n")}
  }
  if (!board_available) {
    return(NULL)
  }
  
  ####################################################### #
  if (justchecking) {
    ## list all datasets in a board
    ## see metadata about board overall, or dataset
    x <- pin_search(board, boardfolder) # defaults to all objects in that folder
    x$name = gsub(paste0(boardfolder,"/"), "", x$name) 
    if ("ejscreen_version" %in% names(x$meta[[1]]$user)) {
      ejv = x$meta[[1]]$user$ejscreen_version
    } else {
      ejv = NA
    }
    x = data.frame(x[, c('name', 'title', 'type', 'file_size', 'created')], 
                   ejscreen_version = ejv)
    x$varnames = (x$name %in% varnames)
    return(x)
     ## could also show current and all prior versions of a dataset
    # pin_versions(board = board, name = 'bgej')
    }
  #################################################################### #
  # WRITE DATA TO BOARD ####
  #################################################################### #
  
  # Ask to confirm each of the defaults if interactive and did not explicitly specify which objects to pin
  if (interactive() && missing(varnames)) {
    confirmed <- rep(TRUE, length(varnames))
    for (i in seq_along(varnames)) {
      confirmed[i] <- askYesNo(paste0("Save ", varnames[i], "?"))
    }
    varnames <- varnames[!is.na(confirmed) & confirmed]
  }
  
  for (i in seq_along(varnames)) {
    
    if (!exists(varnames[i])) {
      warning(paste0("Cannot find ", varnames[i], " so it was not saved to pins."))
    } else {
      cat("pinning ", varnames[i], "\n")
      
      if (varnames[i] %in% metadata4pins$varlist) {
        pin_name = metadata4pins$name[metadata4pins$varlist == varnames[i]]
        pin_title = metadata4pins$title[metadata4pins$varlist == varnames[i]]
        pin_description = metadata4pins$description[metadata4pins$varlist == varnames[i]]
      } else {
        pin_name = varnames[i]
        pin_title = varnames[i]
        pin_description = varnames[i]
      }
      pin_name <- paste0(boardfolder, "/", varnames[i])
      
      text_to_do <- paste0("pins::pin_write(",
                           "board = board, ",
                           "x = ", varnames[i],", ",
                           "name = pin_name, ",
                           "title = pin_title, ",
                           "description = pin_description, ",
                           "versioned = TRUE, ",
                           "metadata = meta, ",
                           "type = type, ", 
                           "access_type = access_type",
                           ")"
      )
      
      cat(" ", text_to_do, '\n')
      x <- eval(parse(text = text_to_do)) 
      # executes the command with unquoted string that is the varnames[i] element, e.g., frs
      rm(x)
      
      # board |>
      #   pins::pin_write(x = bgid2fips,
      #                   name = metadata4pins$name[metadata4pins$varlist == varlist[i]],
      #                   title = metadata4pins$name[metadata4pins$varlist == varlist[i]],
      #                   description = metadata4pins$name[metadata4pins$varlist == varlist[i]],
      #                   versioned = TRUE,
      #                   metadata = attributes(meta),
      #                   access_type = access_type
      #                   )
      
      # board %>% 
      #   pins::pin_write(x = frs, 
      #                   name = "frs", type = "arrow", 
      #                   title = "frs data from EJScreen for EJAM", 
      #                   description = "data.table -- See documentation in EJAM package", 
      #                   versioned = TRUE, metadata = attributes(meta), access_type = access_type
      #   )}
      
    }
  }
  ############################################################### # 
  
  ## Confirm it worked ####
  if (interactive()) {
    for (vn in varnames) {
      board %>% pins::pin_browse(paste0(boardfolder, "/", vn))
    }}
  
  # cat( 'MIGHT HAVE TO MANUALLY SET ACCESS TO "EVERYONE" FOR EACH OF THE PINS, or try access_type parameter \n ')
  
  return(varnames)
  ############################################################### # 
  
## older code obsolete:
  #################################################################### #
  # 
  # ## FRS DATA   ####
  # 
  # if (exists("frs") & ("frs" %in% varnames)) {  
  #   # https://rstudio-connect.dmap-stage.aws.epa.gov/content/2c1f4770-adf1-4e4b-9e9b-f22ab597c858/frs.arrow
  #   board %>% 
  #     pins::pin_write(x = frs, 
  #                     name = "frs", type = "arrow", 
  #                     title = "frs data from EJScreen for EJAM", 
  #                     description = "data.table -- See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta), access_type = access_type
  #     )}
  # if (exists("frs_by_programid") & ("frs_by_programid" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = frs_by_programid, 
  #                     name = "frs_by_programid", type = "arrow", 
  #                     title = "frs_by_programid data from EJScreen for EJAM", 
  #                     description = "data.table -- See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )}
  # if (exists("frs_by_naics") & ("frs_by_naics" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = frs_by_naics, 
  #                     name = "frs_by_naics", type = "arrow", 
  #                     title = "frs_by_naics data from EJScreen for EJAM", 
  #                     description = "data.table -- See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )}
  # if (exists("frs_by_sic") & ("frs_by_sic" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = frs_by_sic, 
  #                     name = "frs_by_sic", type = "arrow", 
  #                     title = "frs_by_sic data from EJScreen for EJAM", 
  #                     description = "data.table -- See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )}
  # if (exists("frs_by_mact") & ("frs_by_mact" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = frs_by_mact, 
  #                     name = "frs_by_mact", type = "arrow", 
  #                     title = "frs_by_mact data from EJScreen for EJAM", 
  #                     description = "data.table -- See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )}
  # ################### # 
  # 
  # # BLOCKGROUP DATA      
  # 
  # if (exists("bgej") & ("bgej" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = bgej, 
  #                     name = "bgej", type = "arrow", 
  #                     title = "bgej data from EJScreen for EJAM", 
  #                     description = "data.frame -- approx 243k blockgroups, like blockgroupstats but for EJ Index raw scores, with bgfips, bgid, etc. - See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )
  # }
  # if (exists("bgid2fips") & ("bgid2fips" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = bgid2fips, 
  #                     name = "bgid2fips", type = "arrow", 
  #                     title = "bgid2fips data for EJAM", 
  #                     description = "data.table of approx 242k blockgroups with Census FIPS for each blockgroup ID - See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )
  # }
  # ################### # 
  # 
  # # BLOCKS DATA
  # 
  # if (exists("blockid2fips") & ("blockid2fips" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = blockid2fips, 
  #                     name = "blockid2fips", type = "arrow", 
  #                     title = "blockid2fips data for EJAM", 
  #                     description = "data.table of approx 8 million Census blocks with Census FIPS for each block ID - See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )
  # }
  # if (exists("blockpoints") & ("blockpoints" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = blockpoints, 
  #                     name = "blockpoints", type = "arrow", 
  #                     title = "blockpoints data for EJAM", 
  #                     description = "data.table of approx 8 million Census blocks with blockid, lat, lon - See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )
  # }
  # if (exists("quaddata") & ("quaddata" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = quaddata, 
  #                     name = "quaddata", type = "arrow", 
  #                     title = "quaddata data for EJAM", 
  #                     description = "data.table of approx 8 million Census blocks with BLOCK_X, BLOCK_Z, BLOCK_Y, blockid, used to create index of all US block point locations - See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     ) 
  # }
  # if (exists("blockwts") & ("blockwts" %in% varnames)) {
  #   board %>% 
  #     pins::pin_write(x = blockwts, 
  #                     name = "blockwts", type = "arrow", 
  #                     title = "blockwts data from EJScreen for EJAM", 
  #                     description = "data.table of approx 8 million Census blocks with blockid, bgid, blockwt, block_radius_miles - See documentation in EJAM package", 
  #                     versioned = TRUE, metadata = attributes(meta)
  #     )
  # }
  # 
  
}  
############################################################### # ############################################################### # 

# ~ ####


# older notes 


# documentation on pins package ####
#
# https://docs.posit.co/connect/user/content-settings/
# https://pins.rstudio.com/reference/board_connect.html
# https://pins.rstudio.com/articles/posit-connect.html
# https://docs.posit.co/connect/how-to/pins/
 
#################################################################### #

# board <- pins::board_connect(auth = "auto")   # uses  "rsconnect"
# confirm you can see it:
# board %>% pins::pin_browse("Mark/frs") ## launches browser
## other options for types of boards: 
## board_local() - link to local file folder
## board_folder() - link to dropbox or network drive
## board_s3() - link to S3 bucket, such as EPA Data commons
## board_url() - build board from data URLs, allow read-only access to datasets
############################################################### #

## example of reading data from a pins board into a shiny app
## see more at https://pins.rstudio.com/articles/posit-connect.html
# if (FALSE) {
#   library(shiny)
#   ui <- fluidPage(
#     tableOutput("table")
#   )
#   
#   server <- function(input, output, session) {
#     #board <- board_local()
#     data <- pin_reactive_read(board, "blockgroupstats_arrow", interval = 1000)
#     output$table <- renderTable(data()[1:100,1:10])
#   }
#   shinyApp(ui, server)
# }
############################################################### #

## read datasets back in from the board
# system.time({bgstats <- board %>% pin_read('blockgroupstats_rds')})
# system.time({bgstats <- board %>% pin_read('blockgroupstats_arrow')})


# TO READ IT LATER 

# Note user needs own Posit Connect API key if not accessible by all on network, but should be accessible by all on network as configured as of 12/2023 

### via URL ####
# if logged in and have api key etc already, 
# this does work to download it: 
# https://rstudio-connect.dmap-stage.aws.epa.gov/content/343456d8-d580-47e1-87f2-1ec95ad7f792/_rev1116/bgej.arrow


### via R code  ####
#
# library(pins)
# board <- pins::board_connect(server = "rstudio-connect.dmap-stage.aws.epa.gov")
### board <- pins::board_connect(server = server = Sys.getenv("CONNECT_SERVER")) # ??? 
# bgej <- pins::pin_read(board, "Mark/bgej") 
# bgej[bgej$ST == "DE", ]  ### IT IS A TIBBLE NOT DT, NOT DF
#
# # or generally: 
#### dataload_from_pins()   
# # which mostly does something similar to this:
# b_vars <- c('blockwts', 'quaddata', 'blockpoints', 'blockid2fips', 'bgid2fips', 'bgej')
# frs_vars <- c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
# for (varname in c(frs_vars, b_vars)) {
#   assign(varname, value = pins::pin_read(board, paste0("Mark/", varname)))
# }

############################################################### # 

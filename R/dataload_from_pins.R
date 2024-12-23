
#' Utility to download / load datasets from pin board
#'
#' @details
#'   This does work if on VPN and if credentials already set up for the user doing this:
#'
#'   board <- pins::board_connect(auth = "rsconnect")
#'
#'   This does work if that is true plus the two environment variables were created:
#'   ```
#'   board <- pins::board_connect(auth = 'manual',
#'     server = Sys.getenv("CONNECT_SERVER"),
#'     key = Sys.getenv("CONNECT_API_KEY")
#'   )
#'   ```
#'     after Sys.setenv(CONNECT_SERVER = "https://rstudio-connect.dmap-stage.aws.epa.gov")
#'
#'     and   Sys.setenv(CONNECT_API_KEY =  correct-API-key-goes-here  )
#'
#' @param varnames character vector of names of R objects to get from board,
#'   or set this to "all" to load all of them
#' @param boardfolder if needed to specify a different folder than default
#' @param auth See help documentation for [pins::board_connect()]
#' @param server if needed to specify a server other than default (which might be
#'   stored in envt variable CONNECT_SERVER or be registered via the rsconnect package).
#'   Note if auth = "envvar" then it looks for CONNECT_SERVER to get name of server which
#'   needs to be the full url starting with https:// - see help for board_connect
#' @param envir if needed to specify environment other than default, e.g., globalenv() or parent.frame()
#' @param folder_local_source path of local folder to
#'   look in for locally saved copies in case pins board is not reachable by user.
#' @param justchecking can set to TRUE to just see a list of what pins are stored in that board
#' @param silent set to TRUE to suppress cat() msgs to console
#' @param ignorelocal set it to TRUE to avoid 1st checking local disk for copies of files
#' @import pins
#'
#' @return If justchecking = FALSE,
#'
#'   returns vector of names of objects now in memory in specified envir, either because
#'
#'   1) already in memory or
#'
#'   2) loaded from local disk or
#'
#'   3) successfully downloaded.
#'
#'   If justchecking = TRUE, however,
#'
#'   returns vector of names of ALL objects found in specified pin board (or NULL if cannot connect)
#'
#'   regardless of whether they are already in the environment, and
#'
#'   regardless of whether they were specified among varnames, or are related to EJAM at all.
#'
#' @export
#'
dataload_from_pins <- function(
    varnames = c('blockwts', 'blockpoints', "quaddata",
                 # load only if /when needed:
                 'bgej',
                 'bgid2fips', 'blockid2fips',
                 'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")[1:3],
    boardfolder = "Mark",
    auth = "auto",
    server = "https://rstudio-connect.dmap-stage.aws.epa.gov",
    # server = "rstudio-connect.dmap-stage.aws.epa.gov",
    folder_local_source = NULL, # './data/', # or "~/../Downloads"
    envir = globalenv(),
    justchecking = FALSE,
    ignorelocal = FALSE,
    silent = FALSE) {
  
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
  
  if ('all' %in% tolower(varnames)) {
    varnames <- c(
      c('blockwts', 'blockpoints', 'blockid2fips', "quaddata"),
      'bgej',
      'bgid2fips',
      c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
    )
  }
  
  if (justchecking) {
    if (!ignorelocal) {
      #  display in console some info on where these objects are
      dataload_from_local(varnames = varnames, envir = envir,
                          justchecking = TRUE, folder_local_source = folder_local_source)
    }
  }
  ####################################################### #
  # Check access to pins board server ####
  #
  # https://rstudio-connect.dmap-stage.aws.epa.gov/connect/#/content/listing?q=is:published+type:pin+owner:Mark
  cat("\n")
  if (auth == "rsconnect") {
    # ignore server default here. use server and key already configured for rsconnect.
    board <- tryCatch(pins::board_connect(auth = "rsconnect"), 
                      error = function(e) {e})
  } else {
    board <- tryCatch(pins::board_connect(server = server, auth = auth),
                      error = function(e) {e})
  }
  if (inherits(board, "error")) {
    board_available <- FALSE
    if (!silent) {cat("Failed trying to connect to pins board server.\n\n")}
  } else {
    board_available <- TRUE
    if (!silent) {cat("Successfully connected to Posit Connect pins board.\n\n")}
  }
  
  ####################################################### #
  # make output in console easier to read:
  if (length(varnames) > 1) {
    widest <- max(nchar(varnames))
  } else {
    widest <- max(10, nchar(varnames))
  }
  spacing <- sapply(1:length(varnames), function(x) {
    paste0(rep(" ", widest - nchar(varnames[x])), collapse = '')
  })
  varnames_gotten <- NULL
  
  # justchecking, just report availability ####
  if (justchecking) {
    
    if (board_available) {
      ## list all datasets in a board
      ## see metadata about board overall, or dataset
      if (!silent) {cat("\nAvailable pins found at", server,":\n\n")}
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
}

if (justchecking) {
  
  # if (board_available) {
  #   message("Ignoring varnames, since justchecking = TRUE")
  #   if (!silent) {cat("\nAvailable pins found at", server,":\n\n")}
  #   varnames_info <- pins::pin_search(board, boardfolder)
  #   print(varnames_info) # view a table of info about the pins
  #   if (!silent) {cat("\n")}
  #   varnames_gotten <- gsub(paste0(boardfolder, "/"), "",  varnames_info$name)
  # } else {
  #   if (!silent) {cat("\n")}
  #   return(varnames_gotten) # get a vector of just the names of the objects
  # }
  
} else {
  
  # not justchecking, so loop over varnames and get each ####
  
  for (i in 1:length(varnames)) {
    
    varname_i <- varnames[i]
    
    # 1. TRY MEMORY ####
    
    if (exists(varname_i, envir = envir)) {
      ## done (was already in memory) ####
      if (!silent) {
        cat(varname_i, spacing[i],
            " - was already in memory (in specified envir), so local and/or server copies were not sought.\n")
      }
      varnames_gotten <- c(varnames_gotten, varname_i)
    } else {
      if (!ignorelocal) {
        
        # 2. TRY LOCAL DISK next ####
        
        # (before bothering with pins board server)
        if (!silent) {cat(varname_i, spacing[i], 
                          " - is NOT in memory. Checking local disk... ")}
        dataload_from_local(varname_i, folder_local_source = folder_local_source, ext = 'arrow', envir = envir)
        if (exists(varname_i, envir = envir)) {
          ## done (got from local disk) ####
          if (!silent) {cat(varname_i, spacing[i], 
                            " - was loaded from local folder, so server copy was not sought.\n")}
          varnames_gotten <- c(varnames_gotten, varname_i)
        }
      }
      if (!exists(varname_i, envir = envir)) {
        if (!silent && !ignorelocal) {cat(varname_i, spacing[i], 
                                          " - is NOT in local folder.\n")}
        
        # 3. TRY PINS SERVER last ####
        
        if (board_available) {
          pathpin <- paste0(boardfolder, "/", varname_i)
          if (pins::pin_exists(board, pathpin)) {
            assign(varname_i, pins::pin_read(board, pathpin), envir = envir)
            if (exists(varname_i, envir = envir)) {
              ## done (got from pins) ####
              if (!silent) {cat(varname_i, spacing[i], 
                                "- was read from pins board server, into memory (to specified envir).\n")}
              varnames_gotten <- c(varnames_gotten, varname_i)
            } else {
              
              # FAIL: problem using connect server pins board ####
              
              warning(
                "Error with ", varname_i,
                "- pins board accessed and board has it but failed anyway to obtain or assign to envir!")
            }
          } else {
            if (!silent) {cat(varname_i, spacing[i], 
                              "   - is NOT at ", server, "/", pathpin, "\n", sep = "")}
            warning(pathpin, "not found at ", server)
          }
        } else {
          
          # FAIL: not in RAM or local + cant reach pins server ####
          
          if (!silent) {cat(" and could NOT download ", varname_i, 
                            " - cannot connect to ", server, "/", "\n", sep = "")}
        }
      }
    }
    
    if (!exists(varname_i, envir = envir)) {
      # try public pins board here as last resort location to check 
      attempt = try(dataload_from_urlpins(varname_i, envir = envir)) # use default function and extension params.
      if (inherits(attempt, "try-error")) {
        if (!silent) {cat("Error trying to download ", varname_i, " from public pins board via dataload_from_urlpins()  \n", sep = "")}
      }
      if (!exists(varname_i, envir = envir)) {
        if (!silent) {cat(" and could NOT download ", varname_i, " from public pins board via ejscreendata pkgdown site \n", sep = "")}
      }
    }
    
    if (!exists(varname_i, envir = envir)) {
      # try DMAP data commons AWS here as last resort location to check 
      dataload_from_aws(varname_i, envir = envir, folder_local_source = "NOTCHECKINGLOCAL" ) # use default function and extension params. use a nonworking local folder to avoid local
      if (!exists(varname_i, envir = envir)) {
        if (!silent) {cat(" and could NOT download ", varname_i, " from DMAP AWS data commons \n", sep = "")}
      }
    }
    
  } # end of loop
}
if (!silent) {cat("\n")}
return(varnames_gotten)

#  board <- pins::board_connect(server = "rstudio-connect.dmap-stage.aws.epa.gov")
## board <- pins::board_connect(server = server = Sys.getenv("CONNECT_SERVER"))
#  bgej  <- pins::pin_read(board, "Mark/bgej") ### IT IS A TIBBLE NOT DT, NOT DF
}

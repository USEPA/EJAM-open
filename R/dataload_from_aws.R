
#' Utility to load datasets from AWS DMAP Data Commons, into memory
#'
#' @details See source code for details. 
#'  
#'  ***  tries dataload_from_local() first 
#'    (at least during development) to avoid slow downloads.
#' 
#'  Also see <https://shiny.posit.co/r/articles/improve/scoping/>
#' 
#'   These files are public-facing -- no credentials required.
#'   
#'   Use EJAM:::dataload_from_aws(justchecking=TRUE) 
#'   
#'   or EJAM:::datapack("EJAM") to get info
#'   
#'   or tables()
#'   
#'   or object.size(quaddata)
#'   
#'   blockid2fips was used only in  state_from_blockid(), which is no longer used by testpoints_n(),
#'     so not loaded unless/until needed.
#'     Avoids loading the huge file "blockid2fips" (100MB) and just uses "bgid2fips" (3MB) as needed, that is only 3% as large in memory.
#'     blockid2fips was roughly 600 MB in RAM because it stores 8 million block FIPS as text.
#' 
#'   Files may include the following:
#'   
#'   - frs               (150 MB .arrow file, approx 700 MB RAM)
#'   - frs_by_programid  (approx 500 MB RAM)
#'   - frs_by_sic        (approx  63 MB RAM)
#'   - frs_by_naics      (approx  60 MB RAM)
#'   - frs_by_mact       
#'   
#'   - quaddata     (168 MB on disk, 229 MB RAM)
#'   - blockid2fips ( 20 MB on disk, 621 MB RAM!) No longer needed.
#'   - blockpoints  ( 86 MB on disk, 164 MB RAM)
#'   - blockwts     ( 31 MB on disk, 196 MB RAM)
#'   - bgej         (123 MB RAM)
#'   - bgid2fips    ( 18 MB RAM)
#'   
#' @param varnames character vector of the quoted names of the data objects like blockwts or quaddata
#' @param ext like ".arrow" file extension
#' @param fun like "arrow::read_ipc_file" or "load" to use when reading
#' @param envir e.g., globalenv() or parent.frame()
#' @param mybucket where in AWS, like 
#' @param mybucketfolder where in AWS, like EJAM
#' @param justchecking set to TRUE to get object size (and confirm file is accessible/exists)
#' @param check_server_even_if_justchecking set this to TRUE to stop checking server to see if files are there
#'   when justchecking = TRUE. But server is always checked if justchecking = FALSE.
#' @param testing only for testing
#' @param folder_local_source path of folder (not ending in forward slash) to 
#'   look in for locally saved copies during development 
#'   to avoid waiting for download from a server. 
#' @seealso [datapack()] [dataload_from_pins()] [dataload_from_local()] [dataload_from_package()] [indexblocks()] [.onAttach()] 
#' @return nothing - just loads data into environment (unless justchecking=T)
#' 
#' @export
#'
dataload_from_aws <- function(
    varnames = c('blockwts', 'blockpoints', "quaddata",
                 'bgej',
                 'bgid2fips', 'blockid2fips', 
                 'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")[1:3],
    ext=c(".arrow", ".rda")[2],
    fun=c("arrow::read_ipc_file", "load")[2],  
    envir=globalenv(),  # should it be parent or global or package EJAM envt ??
    mybucket =  'dmap-data-commons-oa',
    mybucketfolder = "EJAM",
    folder_local_source = "./data/", 
    justchecking = FALSE, check_server_even_if_justchecking=TRUE, testing=FALSE) {
  
  ## how to get bucket contents if you want to explore the bucket ----
  # mybucket <-  'dmap-data-commons-oa' # 
  # bucket_contents <- data.table::rbindlist(
  #   get_bucket(bucket = mybucket, prefix = "EJAM"), 
  #   fill = TRUE
  # )
  # bucket_contents
  
  if (!is.character(fun)) {stop('must specify function in fun parameter as a quoted character string')}
  if (length(ext) > 1) {stop('must specify only one file extension for all the files')}
  if (ext == 'arrow') ext <- ".arrow"
  if (ext == 'rda')   ext <- '.rda'
  if ((ext == '.arrow') & missing(fun)) {fun <- "arrow::read_ipc_file"}
  
  fnames     <- paste0(varnames, ext) # varnames are like bgid2fips, ext is .rda, fnames are like bgid2fips.rda
  objectnames <- paste0(mybucketfolder,      '/', fnames) # EJAM/bgid2fips.rda 
  localpaths  <- paste0(folder_local_source, '/', fnames)
  # make output in console easier to read:  
  if (length(objectnames) > 1) {widest <- max(nchar(objectnames))} else {widest <- max(10, nchar(objectnames))}
  spacing <- sapply(1:length(objectnames), function(x) paste0(rep(" ", widest - nchar(objectnames[x])), collapse = ''))
  cat('\n')
  if (testing) {
    cat('varnames are:    ', paste0(varnames,    collapse = ", "), '\n')
    cat('fnames are:      ', paste0(fnames,      collapse = ", "), '\n')
    cat('localpaths are:  ', paste0(localpaths,  collapse = ", "), '\n')
    cat('mybucketfolder:  ', mybucketfolder,  '\n')
    cat('objectnames are: ', paste0(objectnames, collapse = ", "), '\n')
    cat('mybucket:        ', mybucket,  '\n')
    cat('checking each like this:  aws.s3::object_exists(object = "', objectnames[1], '", bucket = "',mybucketfolder,'")', '\n')
    cat('\n')
  }
  
  # if (!missing(folder_local_source) ) { # want from local drive
  dataload_from_local(varnames = varnames, 
                      # ext = ext, fun = fun,  # disabling this because AWS is currently .rda but local is currently .arrow format ***
                      envir = envir, folder_local_source = folder_local_source, justchecking = justchecking)
  # return(localpaths)
  # } # done getting from local drive
  
  ####################### # 
  
  for (i in 1:length(fnames)) {
    
    if (!justchecking & (ext == ".rda")) {
      
      if (exists(varnames[i], envir = envir) ) {
        cat(varnames[i], spacing[i], 'is already in specified envt and will not be downloaded again\n')
      } else {
        
        # if not already in memory/ global envt, get from AWS 
        cat('trying to load', varnames[i],spacing[i],  'from', objectnames[i], '\n')
        
        x <- try(aws.s3::object_exists(object = objectnames[i], bucket = mybucket))
        
        if (!inherits(x, "try-error")) {
          # no error on try
          if (x) {
            #  no error and file exists - load into specified environment
            # cat('found and trying to load...\n')
            aws.s3::s3load(object = objectnames[i], bucket = mybucket, envir = envir) ## * * ############# #  
          } else {
            warning('requested object', objectnames[i], spacing[i],  'not found on server')
          }
        } else {
          # try-error
          warning('requested object', objectnames[i], spacing[i],  'may not be on server - error when checking')
        }
      }
      
    }
    
    if (justchecking & (ext == ".rda")) {
      # TO SEE COMMAND / CHECK THIS IS WORKING
      # cat('Can download', varnames[i], 'from', objectnames[i], '\n')
      cat(paste0(  'aws.s3::s3load(object = "', objectnames[i],'", ', spacing[i], 'bucket = "', mybucket,'", envir = globalenv()', ')' ), "\n")
    }
    
    if (ext != ".rda") {
      # provide option to use arrow, much faster
      # Will try to switch to arrow which should be faster.
      # need to read documentation of aws.s3 pkg to get this syntax correct, though...
      #  aws.s3::s3read_using(bgid2fips, bucket = mybucket, object = "bgid2fips.arrow", FUN = arrow::read_feather) # not tested
      # or maybe 
      # arrow::open....
      
      text_to_do <- paste0("x <- aws.s3::s3read_using(", 
                           "object = '", objectnames[i],"', ", spacing[i],
                           "FUN = ", fun, ", ",
                           "bucket = '", mybucket,"', opts = list(show_progress = TRUE))")
      if (justchecking) {text_to_do <- paste0(text_to_do, "\n assign('", varnames[i], spacing[i], "', x, envir=globalenv())")} # printed this way but executed below with right envir,  because tricky to print parameter used to specify envir
      if (justchecking) {
        cat(text_to_do, "\n")  # TO SEE COMMAND / CHECK THIS IS WORKING
      } else {
        if (!exists(varnames[i], envir = envir)) {  # if not already in memory/ global envt, get from AWS 
          cat('loading', varnames[i], spacing[i], 'from', objectnames[i], '\n')
          if (aws.s3::object_exists(object = objectnames[i], bucket = mybucket)) {
            
            x <- eval(parse(text = text_to_do)) # executes the command
            assign(varnames[i], x, envir = envir) # because unlike using load, s3read_using() returns the object without loading it into memory as an object
            
          } else {
            warning('requested object', objectnames[i], spacing[i], 'not found on server')
          }
        } else {
          cat(varnames[i], spacing[i], 'is already in specified envt and will not be downloaded again\n')
        }
      } # end trying to get .rda
    } # end checking and or getting non .rda
  } # end loop over files
  
  if (justchecking) {
    cat('\n')
    for (i in 1:length(fnames)) {
      if (exists(varnames[i], envir = envir)) {
        cat(varnames[i], spacing[i], 'is already in memory and would not be downloaded again\n')
      } else {
        cat(varnames[i], spacing[i], 'is not in memory and downloaded would be needed\n')
      }
      if (check_server_even_if_justchecking) {
        if (aws.s3::object_exists(object = objectnames[i], bucket = mybucket)) {
          cat(
            fnames[i], spacing[i], "Found on server. File = ",
            round(aws.s3::object_size(object = objectnames[i], bucket = mybucket) / 10^6, 3), "MB\n"
          ) 
        } else {
          cat(fnames[i], spacing[i], 'not found on server.\n')
        }
      }
    }
  }
  
  baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/"
  paths <- paste0(baseurl, objectnames)
  cat('\n')
  return(paths)
  
  
  # BUCKET_CONTENTS <- data.table::rbindlist(aws.s3::get_bucket(bucket = mybucket), fill = TRUE)
  # baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/"
  # urls <- paste0(baseurl, fnames)
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/quaddata.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/bgid2fips.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockpoints.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockwts.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockid2fips.rda"
  
  ################################################################## # 
  
}

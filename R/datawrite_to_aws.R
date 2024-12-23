#' Utility NOT YET WORKING - AccessDenied
#' 
#' Utility to write object(s) like a dataset to DMAP Data Commons, formatted as .arrow or .rda
#' 
#' @details 
#'   mybucket <-  'dmap-data-commons-oa' #
#'   
#'   bucket_contents <- data.table::rbindlist(
#'   
#'     get_bucket(bucket = mybucket, prefix = "EJAM"),
#'     
#'     fill = TRUE
#'       
#'     )
#'       
#'  bucket_contents
#'       
#' @param varnames vector of object names to upload
#' @param ext file .extension appropriate to the format and fun, like ".rda" or ".arrow"
#' @param fun function to use, but as a character string, like "arrow::write_ipc_file"
#'   but fun is ignored if ext=".rda" since it then just uses s3save()
#' @param mybucket do not need to change
#' @param mybucketfolder do not need to change
#' @param justchecking set this to FALSE to actually upload instead of 
#'   just viewing in console the commands to be used, to test/check this
#'
#' @return the paths of the objects on server
#' 
#' @keywords internal
#'
datawrite_to_aws <- function(varnames= c('bgid2fips',   'blockid2fips', 'blockpoints', 'blockwts' , 'quaddata' ), 
                             ext=c(".arrow", ".rda")[2],
                             fun=c("arrow::write_ipc_file", "save")[2], # not sure save would work here. 
                             mybucket =  'dmap-data-commons-oa',
                             mybucketfolder = "EJAM",
                             justchecking = TRUE) {
  
  ## Get bucket contents if you want to explore the bucket ----
  # mybucket <-  'dmap-data-commons-oa' # 
  # bucket_contents <- data.table::rbindlist(
  #   aws.s3::get_bucket(bucket = mybucket, prefix = "EJAM"), 
  #   fill = TRUE
  # )
  # bucket_contents
  
  # ID: 62098739-3989-4af1-994d-bd55819452f9
  # Title: census 2020 block data for EJAM
  # Description: census 2020 block data for EJAM
  # Metadata Link: https://catalog.data.gov/dataset/census-block-internal-point-coordinates-and-weights-formatted-specifically-for-use-in-r-co
  # Organization: OA
  # Folder Name: EJAM
  # Geographic Extent: National
  # Registry Date: 6/20/2023 7:00:00 AM
  # Keyword: Boundaries and Base Data, Environmental Justice
  # S3 Bucket: dmap-data-commons-oa
  
  if (!is.character(fun)) {
    warning('must specify function in fun parameter as a quoted character string')
    return(NULL)
  }
  if (length(ext) > 1) {
    warning('must specify only one file extension for all the files')
    return(NULL)
  }
  if (ext == 'arrow') ext <- ".arrow"
  if (ext == 'rda')   ext <- '.rda'
  if ((ext == '.arrow') & missing(fun)) {fun <- "arrow::write_ipc_file"} 
  
  fnames <- paste0(varnames, ext)
  objectnames <- paste0(mybucketfolder, '/', fnames)
  
  for (i in 1:length(varnames)) {
    
    if (ext == ".rda") { 
      text_to_do <- paste0("aws.s3::s3save(", varnames[i], 
                           ", object = '", objectnames[i],"', ",
                           "bucket = '",mybucket,"', opts = list(show_progress = TRUE))")
      ## e.g.,  # access denied: 
      # aws.s3::s3save(bgid2fips, object = 'EJAM/bgid2fips.rda',  bucket = 'dmap-data-commons-oa', opts = list(show_progress = TRUE))
      
      # EJAM:::datawrite_to_aws("bgid2fips", justchecking = T)
      
      # arrow::write_ipc_file(bgid2fips, "bgid2fips.arrow")
      
    } else {
      
      # Use s3write_using to upload something like .arrow files to AWS DMAP Data Commons ####
      text_to_do <- paste0("aws.s3::s3write_using(", varnames[i], ", ",
                           "object = '", objectnames[i],"', ",
                           "FUN = ", fun, ", ",
                           "bucket = '",mybucket,"', opts = list(show_progress = TRUE))")
    }
    
    if (justchecking) {
      print(text_to_do)  # TO SEE COMMAND / CHECK THIS IS WORKING
    } else {
      eval(parse(text = text_to_do)) # executes the command
    }
    
  }
  # end loop
  
  baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/"
  paths <- paste0(baseurl, objectnames)
  if (justchecking) {cat("\nTo upload files, set parameter   justchecking=FALSE  \n\n")}
  
  return(paths)
}
##############################################################

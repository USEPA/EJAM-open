
#' Read table of data from .csv or .xlsx Excel file
#' 
#' If in RStudio, interactively can select file from local drive. Also used by EJAM shiny app for file uploads.
#' 
#' @description Read simple table from csv or xls or xlsx.
#' For excel format, must be simple table on first tab, 
#' one row for header (column names),
#' data itself starting in first cell of second row, like A2, and
#' all other rows and columns must be empty.
#' 
#' @param fname full path to folder and filename  
#' @param show_col_types FALSE makes it print less to console as it reads using readr::read_csv()
#' @param rowsize_warn Give warning if there are more than this many rows in the table 
#' @return data.frame with contents of table it read
#' 
#' @keywords internal
#' @export
#'
read_csv_or_xl <- function(fname=NULL, show_col_types=FALSE, rowsize_warn= 30 * 1000) {
  
  #See if in shiny application
  in_shiny <- shiny::isRunning()
  if(!in_shiny){
    if (is.null(fname)) {
      if (interactive()) {
        if (rstudioapi::isAvailable()) {
          fname <- rstudioapi::selectFile()
        }
        else {
          fname <- file.choose()
        }
      }
      else {
        stop("fname (file path/name) needed but not provided")
      }
    }
  }
  if (is.data.frame(fname)) {
    # assume they accidentally provided the data not the file with the data
    ## We maybe should warn or even disable if this is a crazy number of points (rows) ***
    return(fname)
  }
  
  file_type <-tolower(tools::file_ext(fname))
  ## We should disable upload of a crazy number of points - maybe check file size or do quick pre-read check of NROW() ? ***
  
  filecontents <- switch(file_type,
                         csv = {
                           tryCatch({
                             
                             data <- readr::read_csv(fname, show_col_types = show_col_types) %>% as.data.frame()
                             if (NROW(data) > rowsize_warn) {
                               warning("There are more than ", rowsize_warn, " rows in this dataset!")
                             }
                             data
                           }, error = function(e) {
                             if (in_shiny) {
                               shiny::validate(paste("This CSV file caused an error:", e$message))
                             } else {
                               stop("Error reading CSV file: ", e$message)
                             }
                             NULL
                           })
                         },
                         
                         xls = {
                           tryCatch({
                             sheets <- readxl::excel_sheets(fname)
                             if (length(sheets) > 1) {
                               if (in_shiny) {
                                 shiny::validate("This Excel file contains multiple sheets. Only the first sheet is processed.")
                               } else {
                                 warning("This Excel file contains multiple sheets. Only the first sheet is processed.")
                               }
                             }
                             
                             data <- readxl::read_excel(fname, sheet = 1) %>% as.data.frame()
                             if (NROW(data) > rowsize_warn) {
                               warning("There are more than ", rowsize_warn, " rows in this dataset!")
                             }
                             data
                           }, error = function(e) {
                             if (in_shiny) {
                               shiny::validate(paste("This Excel file caused an error:", e$message))
                             } else {
                               stop("Error reading Excel file: ", e$message)
                             }
                             NULL
                           })
                         },
                         
                         xlsx = {
                           tryCatch({
                             sheets <- readxl::excel_sheets(fname)
                             if (length(sheets) > 1) {
                               if (in_shiny) {
                                 shiny::validate("This Excel file contains multiple sheets. Only the first sheet is processed.")
                               } else {
                                 warning("This Excel file contains multiple sheets. Only the first sheet is processed.")
                               }
                             }
                             
                             data <- readxl::read_excel(fname, sheet = 1) %>% as.data.frame()
                             if (NROW(data) > rowsize_warn) {
                               warning("There are more than ", rowsize_warn, " rows in this dataset!")
                             }
                             data
                           }, error = function(e) {
                             if (in_shiny) {
                               shiny::validate(paste("This Excel file caused an error:", e$message))
                             } else {
                               stop("Error reading Excel file: ", e$message)
                             }
                             NULL
                           })
                         },
                         
                         stop("Invalid file type. Please upload a .csv, .xls, or .xlsx file")
  )
  
  return(filecontents)
  
  
}

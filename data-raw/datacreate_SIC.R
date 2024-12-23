
datacreate_SIC <- function(
    
  sic_url = "https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/sic-code-descriptions/sic88_97.txt",
  sic_version = "1987"
) {
  
  ###################################################################################
  # create SIC or sictable ####
  # 
  ## SIC is a named list for use in dropdown menu
  ## sictable is a data.table with code, n2,n3,n4,name,num_name  (but used to be tibble?)
  
  # see explanation of SIC vs NAICS here: 
  #  https://www.census.gov/topics/employment/industry-occupation/about/faq.html#par_textimage_291814576 
  
  # see official SIC and NAICS info from Census here: 
  #   https://www.census.gov/naics/?68967
  
  # See https://www.naics.com/everything-sic/ 
  # see history of SIC here
  #  https://guides.loc.gov/industry-research/classification-sic
  # See https://www.osha.gov/data/sic-search etc.
  #
  # In the United States, the SIC system was last revised in 1987
  #
  # and was last used by the Census Bureau for the 1992 Economic Census, 
  # and has been replaced by the North American Industry Classification System (NAICS code), 
  # which was first released in 1997. 
  # Some U.S. government departments and agencies, such as the [SEC]...
  # continued to use SIC codes through at least 2019.
  # - wikipedia.
  ###################################################################################
  
  td1 <- tempdir()
  sic_filename <- basename(sic_url)
  sic_localpath <- file.path(td1, sic_filename)
  
  download.file(
    sic_url, 
    destfile = sic_localpath
  )
  sic_txt <- readr::read_delim(
    sic_localpath, 
    skip = 1, delim = '  ', col_names = c('code','desc')
  )
  sic_txt$desc <- gsub(
    "Dry, condensed, evaporated  products",  # fix this row that had double space error
    "Dry, condensed, evaporated products", 
    sic_txt$desc
  )
  sic_txt <- sic_txt |> dplyr::mutate(code = gsub("[^0-9]+", "", code))
  
  ## separate 2- and 3-digit codes   ???  BUT THOSE DID NOT GET USED BY THIS CODE ??
  # sic_cats2 <- sic_txt |> dplyr::filter(nchar(code) == 2) #filter(str_detect(code,'--'))
  # sic_cats3 <- sic_txt |> dplyr::filter(nchar(code) == 3) #filter(str_detect(code,'\\'))
  ## save 4-digit codes as vector for list used in app
  
  sictable <- sic_txt |> dplyr::filter(nchar(code) == 4) 
  #### this may be an older format that had been used in contrast to the data.table via datacreate_sictable.R
  # attr(sictable, "date_downloaded") <- as.character(Sys.Date())
  # attr(sictable, "source_url")     <- sic_url
  # attr(sictable, "SIC_numbers_version") <- sic_version
  # attr(sictable, "date_saved_in_package") <- as.character(Sys.Date())
  # usethis::use_data(sictable, overwrite = TRUE)
  # cat("saved sictable\n")
  # return(sictable)  
  
  
  # if (!exists("sictable")) {stop(" 'sictable' is required to create 'SIC' ")}
  
  SIC <- sictable |> 
    dplyr::mutate(code_and_desc = paste(code, desc, sep = ' - ')) |> 
    dplyr::select(code_and_desc, code) |> 
    ## turn 2-column df into named list (col1 = names, col2 = values)
    tibble::deframe()
  
  attr(SIC, "date_downloaded") <- as.character(Sys.Date())
  attr(SIC, "source_url")     <- sic_url
  attr(SIC, "SIC_numbers_version") <- sic_version
  attr(SIC, "date_saved_in_package") <- as.character(Sys.Date())
  usethis::use_data(SIC)
  cat("saved SIC \n")
  
  # Document it ####
  dataset_documenter("SIC",
                     "SIC (DATA) named list of all SIC code numbers and category name for each",
                     details = "Also see [SIC info](https://siccode.com)",
                     seealso = "[SIC] [sictable] [sic_categories()]")
  
  invisible(SIC)
}
########################################## # 

SIC <- datacreate_SIC()

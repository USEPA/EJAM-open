
datacreate_sictable <- function(SIC, 
                                sic_url = "https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/sic-code-descriptions/sic88_97.txt", 
                                sic_version = "1987") {
  
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
  # which was released in 1997. 
  # Some U.S. government departments and agencies, such as the [SEC]...
  # continued to use SIC codes through at least 2019.
  # - wikipedia.
  ###################################################################################
  
  if (missing(SIC)) {
    if (exists("SIC")) {
      # was in parent env / path, so use it?
    } else {
      stop("SIC not attached, sictable not created.")
    }
  }
  
  sictable <- data.table::data.table(code = as.vector(SIC), name = names(SIC))
  sictable[ , num_name := trimws(name)]
  sictable[ , name := trimws(gsub(".* - ", "", name))]
  
  sictable[ , n2 := substr(code, 1, 2)]
  sictable[ , n3 := substr(code, 1, 3)]
  sictable[ , n4 := substr(code, 1, 4)]
  
  sictable <- sictable[ , .(code, n2, n3, n4, name, num_name)]
  
  
  attr(sictable, "date_downloaded") <- as.character(Sys.Date())
  attr(sictable, "source_url")     <- sic_url
  attr(sictable, "SIC_numbers_version") <- sic_version
  attr(sictable, "date_saved_in_package") <- as.character(Sys.Date())
  usethis::use_data(sictable, overwrite = TRUE)
  cat("saved sictable\n")
 # Document it ####
  dataset_documenter("sictable",
                     "sictable (DATA) data.table of SIC code(s) and industry names for each EPA-regulated site",
                     description = "data.table of SIC code(s) and industry names for each EPA-regulated site in Facility Registry Service
#' Also has the 2,3, and 4-digit SIC that this code falls under, where relevant for given length",
                     details = "This is similar to the data file EJAM::SIC but in a more useful format and newer functions work with it. Also see [SIC info](https://siccode.com)",
                     seealso = "[SIC] [sictable] [sic_categories()]"
                     )
  
  #    table(nchar(sictable$code))
  #     sictable 
  
  invisible(sictable)
}
############################################################################ #

sictable <- datacreate_SIC() 

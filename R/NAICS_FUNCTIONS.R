
# also see [latlon_from_naics()]  [frs_from_naics()] source code in another file


#' NAICS - General way to search for industry names and NAICS codes
#'
#' Find industry names and codes by searching for queried code(s) or text
#'
#' @param query query string(s) and/or number(s), vector of NAICS codes or industry names or any regular expression or partial words
#' @param children logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits
#' @param ignore.case see [grepl()]
#' @param fixed should it be an exact match? see [grepl()]
#' @param website_scrape whether to scrape info from the NAICS website to return a table of codes and names that match (web query uses synonyms so gets more hits)
#' @param website_url whether to return the URL of the webpage with info on the NAICS (web query uses synonyms so gets more hits)
#' @seealso [latlon_from_naics()]  [frs_from_naics()]  [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]
#'
#' @return a subset of the [naicstable] data.table (not just the codes column)
#' 
#' @details Finding the right NAICS/SIC and finding all the right 
#'   sites is complicated. See discussion of [latlon_from_naics()].
#'   
#' @examples # Also see vignettes for many more examples, and discussion.
#'   naics_categories()
#'   
#'   naics_from_any("textile mills", children = F)
#'   naics_from_any("textile mills", children = T)
#' 
#'   frs_from_naics("textile mills", children = FALSE)
#'   frs_from_naics("textile mills", children = TRUE)
#'   
#'   \dontrun{
#'   naics_from_any(naics_categories(3))[order(name),.(name,code)][1:10,]
#'   naics_from_any(naics_categories(3))[order(code),.(code,name)][1:10,]
#'   naics_from_code(211)
#'   naicstable[code==211,]
#'   naics_subcodes_from_code(211)
#'   naics_from_code(211,  children = TRUE)
#'   naicstable[n3==211,]
#'   NAICS[211][1:3] # wrong
#'   NAICS[NAICS == 211]
#'   NAICS["211 - Oil and Gas Extraction"]
#'
#'  naics_from_any("plastics and rubber")[,.(name,code)]
#'  naics_from_any(326)
#'  naics_from_any(326, children = T)[,.(code,name)]
#'  naics_from_any("plastics", children=T)[,unique(n3)]
#'  naics_from_any("pig")
#'  naics_from_any("pig ") # space after g
#'
#'  # naics_from_any("copper smelting")
#'  # naics_from_any("copper smelting", website_scrape=TRUE)
#'  # browseURL(naics_from_any("copper smelting", website_url=TRUE) )
#'
#'  a = naics_from_any("plastics")
#'  b = naics_from_any("rubber")
#'  fintersect(a,b)[,.(name,code)] #  a AND b
#'  funion(a,b)[,.(name,code)]     #  a OR  b
#'  naics_subcodes_from_code(funion(a,b)[,code])[,.(name,code)]   #  plus children
#'  naics_from_any(funion(a,b)[,code], children=T)[,.(name,code)] #  same
#'
#'  NROW(naics_from_any(325))
#' #[1] 1
#'  NROW(naics_from_any(325, children = T))
#' #[1] 54
#'  NROW(naics_from_any("chem"))
#' #[1] 20
#'  NROW(naics_from_any("chem", children = T))
#' [1] 104
#' }
#' 
#' @export
#'
naics_from_any <- function(query, children = FALSE, ignore.case = TRUE, fixed = FALSE,
                           website_scrape = FALSE, website_url = FALSE) {

  # find naicstable data.table rows by vector of text queries and/or numeric NAICS codes
  # returns subset of naicstable, not in any particular order and number of rows may be longer than number of query terms

  if (any(is.na(query))) {warning( 'query contains NA value(s)')}

  isnum <- suppressWarnings( !is.na(as.numeric(query)) )

  if (website_url) {
    return(naics_url_of_code(query))
  }
  if (website_scrape) {
    return(naics_findwebscrape(query))
  }

  query_codes <- query[isnum]
  if (length(query_codes) != 0) {
    via_codes <- naics_from_code(query_codes, children = children)
  } else {
    via_codes <- NULL
  }
  query_text <- query[!isnum]
  if (length(query_text) != 0) {
    via_text  <- naics_from_name(query_text,  children = children, ignore.case = ignore.case, fixed = fixed)
  } else {
    via_text <- NULL
  }
  results <- data.table::rbindlist(list(via_codes, via_text))
  # if (children) {
  #   # add subcategories
  #   results <- naics_subcodes_from_code(results$code)
  # }
  return(results)
}
################################################################## #


#' NAICS - See the names of industrial categories and their NAICS code
#'
#' Easy way to list the 2-digit NAICS (17 categories), or other level
#' @details
#'  Also see <https://www.naics.com/search/>
#'
#' There are this many NAICS codes roughly by number of digits in the code:
#'
#'   table(nchar(NAICS))
#'
#'    2    3    4    5    6
#'
#'   17   99  311  709 1057
#'
#'   See <https://www.census.gov/naics/>
#'
#' @param digits default is 2, for 2-digits NAICS, the top level, but could be up to 6.
#' @param dataset Should default to the dataset called NAICS, installed with this package.
#'   see [NAICS]  Check attr(NAICS, 'year')
#' @return matrix with 1 column of 2-digit codes and rownames that look like
#'    "22 - Utilities" etc.
#' @examples  naics_categories()
#' @seealso [naics_from_any]  [NAICS]
#'
#' @export
#'
naics_categories <- function(digits=2, dataset=EJAM::NAICS) {

  cat("Also see https://www.naics.com/search/ \n")
  if (is.null(dataset)) {warning('missing NAICS dataset'); return(NA)}
  cbind(cbind(dataset[nchar(as.character(dataset)) == digits]))
}

# make separate columns for the codes, titles, indented code-title strings,
#  and pulldown selection code-title (was the names of code vector).

# indent <- data.frame(title = gsub(".* - ", "", names(NAICS)), code = NAICS, stringsAsFactors = FALSE, row.names = NULL)
#
# cbind(paste0(
#   stringr::str_pad(ndent$code, width = 6, side = 'right'),
#   ifelse(nchar(ndent$code) == 2, ' ** ', "   "),
#   stringr::str_pad(
#     stringr::str_pad(
#       ndent$title, pad = ifelse(nchar(ndent$code) == 4, '.', ifelse(nchar(ndent$code) < 4, '*', ' ')),
#       width = -3 + (2 * nchar(ndent$code)) + nchar(ndent$title),
#       side = 'left'
#       ),
#     width = ifelse(nchar(ndent$code) < 4, 75, 1),
#     pad = '*',
#     side = 'right'
#   )
# ))
########################################################################### #


#' NAICS - Validate NAICS uploads
#'
#' @description Validates and prepares echo uploads
#'
#' @param naics_enter vector of naics
#' @param naics_select single value
#' @return boolean value
#'
#' @keywords internal
#' @export
#'
naics_validation <- function(naics_enter, naics_select) {

  if (all(nchar(naics_enter) > 0) | length(naics_select) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
################################################################## #


#' NAICS - query NAICS codes and also see all children (subcategories) of any of those
#'
#' Used by naics_find()
#' @details
#' - Starts with shortest (highest level) codes. Since tied for nchar, these branches have zero overlap, so do each.
#' - For each of those, get its `children = all` rows where `parentcode == substr(allcodes, 1, nchar(parentcode))`
#' - Put together list of all codes we want to include so far.
#' - For the next longest set of codes in original list of codes, do same thing.
#' - continue until done for 5-digit ones to get 6-digit children.
#' - Take the `unique(allthat)`
#' 
#' `table(nchar(as.character(NAICS)))`
#' 
#' `   2    3    4    5    6`
#' 
#' `  17   99  311  709 1057`
#' 
#' @param codes vector of numerical or character
#' @param allcodes Optional (already loaded with package) - dataset with all the codes
#' @param quiet whether to avoid printing results to console
#' @return vector of codes and their names
#' @seealso [naics_find()] [NAICS]
#' @examples
#'   naics2children(211)
#'   naics_find(211, exactnumber=TRUE)
#'   naics_find(211, exactnumber=TRUE, add_children = TRUE)
#'   NAICS[211][1:3] # wrong
#'   NAICS[NAICS == 211]
#'   NAICS["211 - Oil and Gas Extraction"]
#'
#' @export
#'
naics2children <- function(codes, allcodes=EJAM::NAICS, quiet = FALSE) {

  codes <- suppressWarnings( { as.numeric(codes)}) # becomes NA if text that cannot be coerced into number
  if (any(is.na(codes))) {warning("codes should be numeric NAICS codes or text that can be interpreted as numeric, but some are NA values or character that cannot be coerced to numeric")}
  if (any(nchar(codes[!is.na(codes)]) < 2 | nchar(codes[!is.na(codes)]) > 6)) warning("codes should be 2-digit to 6-digit NAICS code(s)")

  # if (missing(allcodes)) {allcodes <- NAICS} # data from this package
  codes <- as.character(codes)
  kidrows <- NULL
  for (digits in 2:5) {
    sibset <- codes[nchar(codes) == digits]
    kidrows <- union(kidrows, which(substr(allcodes, 1, digits) %chin% sibset))
    # if that were a data.table then funion() could be used which is faster
  }
  x <- c(codes, allcodes[kidrows])
  x <- x[!duplicated(x)]
  x <- allcodes[allcodes %in% x] # cannot use %chin% unless using as.character(allcodes). fast enough anyway.
  if (!quiet) {cat(paste0('\n', names(x)), '\n')}
  invisible(x)
}
################################################################## #


#' NAICS - find subcategories of the given overall NAICS industry code(s)
#'
#' Given 3-digit NAICS code, for example, get all NAICS that start with those digits.
#'
#' @details  similar idea was naics2children() but this is more robust
#' See [naics_from_any()] which uses this
#' @param mycodes NAICS codes vector, of 2 to 6 digits each. See <https://naics.com>
#'
#' @return a subset of the [naicstable] data.table (not just the codes column)
#' @seealso [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]  [naics_from_any()]
#' @examples
#'   naics_categories()
#'
#' @export
#'
naics_subcodes_from_code <- function(mycodes) {

  mycodes <- suppressWarnings( { as.numeric(mycodes)}) # becomes NA if text that cannot be coerced into number
  if (any(is.na(mycodes))) {warning("mycodes should be numeric NAICS codes or text that can be interpreted as numeric, but some are NA values or character that cannot be coerced to numeric")}
  if (any(nchar(mycodes[!is.na(mycodes)]) < 2 | nchar(mycodes[!is.na(mycodes)]) > 6)) warning("mycodes should be 2-digit to 6-digit NAICS code(s)")

  len <- nchar(mycodes)
  cnames  <- paste0("n", 1:6)
  results <- list()
  results[[1]] <- NULL
  for (digits in 2:6) {
    mycolname <- cnames[digits]
    myvalues <- unlist(as.vector(naicstable[ , ..mycolname])) # this seems like a crazy workaround, but can't see how to subset data.table by specifying mycolname == 1123 when the column name is stored in mycolname
    results[[digits]] <-  naicstable[myvalues %in% mycodes[len == digits], ] # subset(naicstable, mycolname %in% mycodes[len == digits] )
  }
  results <- data.table::rbindlist(results)
  return(results)
}
################################################################## #


#' NAICS - search for industry names by NAICS code(s), 2-6 digits long each
#'
#' See [naics_from_any()] which uses this
#'
#' @param mycodes vector of numeric NAICS codes. see <https://naics.com>
#' @param children logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits
#' @seealso [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]  [naics_from_any()]
#'
#' @return a subset of the [naicstable] data.table (not just the codes column)
#'
#' @keywords internal
#'
naics_from_code <- function(mycodes, children = FALSE) {

  mycodes <- suppressWarnings( { as.numeric(mycodes)}) # becomes NA if text that cannot be coerced into number
  if (any(is.na(mycodes))) {warning("mycodes should be numeric NAICS codes or text that can be interpreted as numeric, but some are NA values or character that cannot be coerced to numeric")}

  # find naicstable data.table rows by exact matches on numeric NAICS codes vector
  # results <- naicstable[match(mycodes, naicstable$code), ] # this would preserve sort order better BUT ONLY RETURNS 1st match !!!
  results <- naicstable[code %in% mycodes, ] # this does not preserve order of mycodes queried, but cannot use match which would return only 1st match. 
  if (children) {
    # add subcategories
    results <- naics_subcodes_from_code(results$code)
  }
  return(results)
}
################################################################## #


#' NAICS - Search for industry names and NAICS codes by query string
#'
#' query by parts of words, etc. in the industry name.
#'
#' See [naics_from_any()] which uses this
#' @param mynames query string, vector of NAICS industry names or any regular expression or partial words. See <https://naics.com>
#' @param children logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits
#' @param ignore.case see [grepl()]
#' @param fixed should it be an exact match? see [grepl()]
#' @seealso [naics_findwebscrape()] [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]  [naics_from_any()]
#' @examples
#'  data.table::fintersect(naics_from_any( "manufac"), naics_from_any("chem"))
#' @return a subset of the [naicstable] data.table (not just the codes column)
#'
#' @keywords internal
#'
naics_from_name <- function(mynames, children = FALSE, ignore.case = TRUE, fixed = FALSE) {

  # find naicstable data.table rows by text search in NAICS industry names via grepl()
  if (any(is.na(mynames) | !(is.character(mynames)) | is.numeric((mynames)))) {warning( 'mynames should be non-NA character vector of text to look for in industry title(s) like "concrete"')}
  hits <- vector()
  results <- NULL
  for (i in 1:length(mynames)) {
    hits <- c(hits, which(grepl(mynames[i], naicstable$name, ignore.case = ignore.case,  fixed = fixed)) )
  }
  results <- naicstable[unique(hits),]
  if (children) {
    # add subcategories
    results <- naics_subcodes_from_code(results$code)
  }
  return(results)
}
################################################################## #


#' NAICS - Get URL for page with info about industry sector(s) by NAICS
#'
#' See (https://naics.com) for more information on NAICS codes
#'
#' @param naics vector of one or more NAICS codes, like 11,"31-33",325
#' @seealso [naics_from_any()] [naics_findwebscrape()]
#' @return vector of URLs as strings like https://www.naics.com/six-digit-naics/?v=2017&code=22
#'
#' @export
#'
naics_url_of_code <- function(naics) {

  paste0("https://www.naics.com/six-digit-naics/?v=2017&code=", naics)
}
################################################################## #


#' for query term, show list of roughly matching NAICS, scraped from web
#'
#' This finds more than just [naics_from_any()] does, since that needs an exact match
#'   but this looks at naics.com website which lists various aliases for a sector.
#'
#' @param query text like "gasoline" or "copper smelting"
#' @seealso [naics_from_any()]  [url_naics.com()]
#' @return data.frame of info on what was found, naics and title
#' @examples
#'  # naics_from_any("copper smelting")
#'  # naics_from_any("copper smelting", website_scrape=TRUE)
#'  # browseURL(naics_from_any("copper smelting", website_url=TRUE) )
#'
#'   url_naics.com("copper smelting")
#'   \dontrun{
#'   naics_findwebscrape("copper smelting")
#'   browseURL(url_naics.com("copper smelting"))
#'   browseURL(naics_url_of_code(326))
#'   }
#'
#' @export
#'
naics_findwebscrape <- function(query) {

  myurl <- url_naics.com(query)

  htm <- rvest::read_html(myurl)
  x <- htm |> rvest::html_elements(css = ".first_child a") |> rvest::html_text2()
  x <- data.frame(matrix(x, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  names(x) <- c("code", "name")
  x
}
################################################################## #


#' NAICS - Script to download NAICS file with code and name of sector
#'
#' See source code. Mostly just a short script to get the 2017 or 2022 codes and names.
#' See <'https://www.census.gov/naics/?48967'>
#' @param year which vintage of NAICS codes to use, 2012, 2017, or 2022
#' @param urlpattern full url of xlsx file to use, but with YYYY instead of year
#' @param destfile full path and name of file to save as locally
#'
#' @return names list with year as an attribute
#'
#' @keywords internal
#'
naics_download <- function(year=2017, urlpattern='https://www.census.gov/naics/YYYYNAICS/2-6%20digit_YYYY_Codes.xlsx', destfile= paste0('~/Downloads/', year, 'NAICS.xlsx')) {
  # this can be used to create the NAICS dataset as for this package
  # See \url{https://www.census.gov/naics/}
  if (!(year %in% c(2012, 2017, 2020))) {warning('only works for 2012, 2017, 2020')
    return(NULL)
  }
  url <- gsub('YYYY',year, urlpattern)
  if (year == 2012) {url <- gsub('6%20', '', url)}
  # 'https://www.census.gov/naics/2022NAICS/2-6%20digit_2022_Codes.xlsx'
  # 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
  # 'https://www.census.gov/naics/2012NAICS/2-digit_2012_Codes.xls'
  download.file(
    url = url,
    destfile = destfile
  )
  x <- readxl::read_xlsx(path = destfile, skip = 2, col_names = c('n','code','title','b','c','d'))
  mynames <- paste(x$code, ' - ', x$title, sep = '')
  mycodes <- as.numeric(as.list(x$code))
  # mynames[(is.na(mycodes))]  # remove the ones that are ranges instead of being a 2-digit or longer code
  # ###  "31-33 - Manufacturing"  "44-45 - Retail Trade"   "48-49 - Transportation and Warehousing"
  NAICS        <- as.list(mycodes[!is.na(mycodes)])
  names(NAICS) <- mynames[!is.na(mycodes)]
  # table(as.numeric(sapply((NAICS), FUN=nchar)))
  # head(cbind(NAICS[substr(NAICS,1,2)=='31']))

  # save as NAICS dataset for package, but with year attribute indicating vintage:
  NAICS <- structure(NAICS, year = year)
  # attr(NAICS, 'year')
  # [1] 2017 # for example

  ################# #  ################# #  ################# #  ################# #
  ################# #  ################# #  ################# #  ################# #
  # ADDED IN 2023:
  # CREATES PLACEHOLDERS AT 2-DIGIT LEVEL SO THEY SHOW UP IN LISTING
  # SINCE THESE WERE NOT LISTED AT 2 DIGIT LEVEL IN ORIGINAL NAICS LIST USED HERE

  extrarows <- c(
    31,32,33,
    44,45,
    48,49
  )
  names(extrarows) <- c(
    "31 - Manufacturing",
    "32 - Manufacturing",
    "33 - Manufacturing",

    "44 - Retail Trade",
    "45 - Retail Trade",

    "48 - Transportation and Warehousing",
    "49 - Transportation and Warehousing"
  )

  NAICS <- c(NAICS, extrarows)
  # usethis::use_data(NAICS, overwrite = TRUE)

  # code	industry_title
  # 11	Agriculture, Forestry, Fishing and Hunting
  # 21	Mining
  # 22	Utilities
  # 23	Construction
  # 31-33	Manufacturing
  # 42	Wholesale Trade
  # 44-45	Retail Trade
  # 48-49	Transportation and Warehousing
  # 51	Information
  # 52	Finance and Insurance
  # 53	Real Estate Rental and Leasing
  # 54	Professional, Scientific, and Technical Services
  # 55	Management of Companies and Enterprises
  # 56	Administrative and Support and Waste Management and Remediation Services
  # 61	Educational Services
  # 62	Health Care and Social Assistance
  # 71	Arts, Entertainment, and Recreation
  # 72	Accommodation and Food Services
  # 81	Other Services (except Public Administration)
  # 92	Public Administration




  ################# #

  cat('To update source code R package, try usethis::use_data(NAICS.rdata) or just save(NAICS, file = \'yourpath/EJAM/data/NAICS.rdata\') \n')
  # usethis::use_data(NAICS.rdata, overwrite = TRUE)
  # ### not ### save(NAICS, file = './data/NAICS.rdata')

  return(NAICS)
}
################################################################## #



#' NAICS - Try to extract which NAICS could be affected by a rule published in the Federal Register
#' by reading the NAICS listed near the top of the preamble - DRAFT WORK IN PROGRESS
#'
#' @param naics_text_copy_from_fr
#'
#' @keywords internal
#'
naics_from_federalregister <- function(naics_text_copy_from_fr) {



  # WORK IN PROGRESS






  # some are formatted as a small table, as in https://www.federalregister.gov/d/2022-27522/p-59

  # this works for this particular example but formatting is likely inconsistent across FR notices.
  #
  # Example copied from https://www.federalregister.gov/documents/2023/05/03/2023-09184/methylene-chloride-regulation-under-the-toxic-substances-control-act-tsca
  #
  #     naics_text_copy_from_fr <- "
  # Other Chemical and Allied Products Merchant Wholesalers (NAICS code 424690);
  #
  # Crude Petroleum Extraction (NAICS code 211120);
  #
  # All Other Basic Organic Chemical Manufacturing (NAICS code 325199);
  #
  # Other Chemical and Allied Products Merchant Wholesalers (NAICS code 424690);
  #
  # Petroleum Bulk Stations and Terminals (NAICS code 424710);
  #
  # Other Basic Inorganic Chemical Manufacturing (NAICS code 325180);
  #
  # Testing Laboratories (NAICS code 541380);
  #
  # Hazardous Waste Treatment and Disposal (NAICS code 562211);
  #
  # Solid Waste Combustors and Incinerators (NAICS code 562213);
  #
  # Materials Recovery Facilities (NAICS code 562920);
  #
  # Paint and Coating Manufacturing (NAICS code 325510);
  #
  # Air and Gas Compressor Manufacturing (NAICS code 333912);
  #
  # Gasket, Packing, and Sealing Device Manufacturing (NAICS code 339991);
  #
  # Residential Remodelers (NAICS code 236118);
  #
  # Commercial and Institutional Building Construction (NAICS code 236220);
  #
  # Plumbing, Heating, and Air-Conditioning Contractors (NAICS code 238220);
  #
  # Painting and Wall Covering Contractors (NAICS code 238320);
  #
  # All Other Miscellaneous Manufacturing (NAICS code 339999);
  #
  # Automotive Parts and Accessories Stores (NAICS code 441310);
  #
  # All Other Miscellaneous Store Retailers (except Tobacco Stores) (NAICS code 453998);
  #
  # Other Support Activities for Air Transportation (NAICS code 488190);
  #
  # All Other Automotive Repair and Maintenance (NAICS code 811198);
  #
  # Commercial and Industrial Machinery and Equipment (except Automotive and Electronic) Repair and Maintenance (NAICS code 811310);
  #
  # Footwear and Leather Goods Repair (NAICS code 811430);
  #
  # Adhesive Manufacturing (NAICS code 325520);
  #
  # All Other Miscellaneous Chemical Product and Preparation Manufacturing (NAICS code 325998);
  #
  # Audio and Video Equipment Manufacturing (NAICS code 334310);
  #
  # Reupholstery and Furniture Repair (NAICS code 811420);
  #
  # All Other Rubber Product Manufacturing (NAICS code 326299);
  #
  # All Other Miscellaneous Textile Product Mills (NAICS code 314999);
  #
  # All Other Miscellaneous Fabricated Metal Product Manufacturing (NAICS code 332999);
  #
  # [there was a intToUtf8(8226) bullet here]  Oil and Gas Field Machinery and Equipment Manufacturing (NAICS code 333132);
  #
  # Bare Printed Circuit Board Manufacturing (NAICS code 334412);
  #
  # Other Electronic Component Manufacturing (NAICS code 334419);
  #
  # All Other Miscellaneous Electrical Equipment and Component Manufacturing (NAICS code 335999);
  #
  # Printing Machinery and Equipment Manufacturing (NAICS code 333244);
  #
  # Petroleum Refineries (NAICS code 324110);
  #
  # Petroleum Lubricating Oil and Grease Manufacturing (NAICS code 324191);
  #
  # Painting and Wall Covering Contractors (NAICS code 238320);
  #
  # Welding and Soldering Equipment Manufacturing (NAICS code 333992);
  #
  # New Car Dealers (NAICS code 441110);
  #
  # Used Car Dealers (NAICS code 441120);
  #
  # Drycleaning and Laundry Services (except Coin-Operated) (NAICS code 812320); and
  #
  # Doll, Toy, and Game Manufacturing (NAICS code 339930)."

  xx <- naics_text_copy_from_fr  # rm(naics_text_copy_from_fr)
  xx <- gsub("\n\n", "\n", xx)
  xx <- gsub(")\\.", ")",  xx)
  xx <- gsub("; and", ";", xx)
  xx <- gsub(";\n", "\n",  xx)
  xx <- gsub(paste0(intToUtf8(8226), " "), "",     xx) # need to get the intToUtf8(8226)
  # cat(xx)
  xx <- strsplit(xx,"\n")
  # xx
  z <- data.frame(do.call(
    rbind,
    lapply(
      xx,
      function(z) strsplit(z," \\(NAICS code ")
    )[[1]]
  ))
  # head(z)
  names(z) <- c("naicsname", "naicscode")
  z$naicscode <- as.numeric(trimws(gsub(")", "", z$naicscode)))
  z$naicsname <- trimws(z$naicsname)
  # head(z)
  return(z)
}
################################################################## #

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NAICS_FUNCTIONS.R
\name{naics_from_any}
\alias{naics_from_any}
\title{NAICS - General way to search for industry names and NAICS codes}
\usage{
naics_from_any(
  query,
  children = FALSE,
  ignore.case = TRUE,
  fixed = FALSE,
  website_scrape = FALSE,
  website_url = FALSE
)
}
\arguments{
\item{query}{query string(s) and/or number(s), vector of NAICS codes or industry names or any regular expression or partial words}

\item{children}{logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits}

\item{ignore.case}{see \code{\link[=grepl]{grepl()}}}

\item{fixed}{should it be an exact match? see \code{\link[=grepl]{grepl()}}}

\item{website_scrape}{whether to scrape info from the NAICS website to return a table of codes and names that match (web query uses synonyms so gets more hits)}

\item{website_url}{whether to return the URL of the webpage with info on the NAICS (web query uses synonyms so gets more hits)}
}
\value{
a subset of the \link{naicstable} data.table (not just the codes column)
}
\description{
Find industry names and codes by searching for queried code(s) or text
}
\details{
Finding the right NAICS/SIC and finding all the right
sites is complicated. See discussion of \code{\link[=latlon_from_naics]{latlon_from_naics()}}.
}
\examples{
# Also see vignettes for many more examples, and discussion.
  naics_categories()
  
  naics_from_any("textile mills", children = FALSE)
  naics_from_any("textile mills", children = TRUE)

  frs_from_naics("textile mills", children = FALSE)
  frs_from_naics("textile mills", children = TRUE)
  
  \donttest{
  naics_from_any(naics_categories(3))[order(name),.(name,code)][1:10,]
  naics_from_any(naics_categories(3))[order(code),.(code,name)][1:10,]
  naics_from_code(211)
  naicstable[code==211,]
  naics_subcodes_from_code(211)
  naics_from_code(211,  children = TRUE)
  naicstable[n3==211,]
  NAICS[211][1:3] # wrong
  NAICS[NAICS == 211]
  NAICS["211 - Oil and Gas Extraction"]

 naics_from_any("plastics and rubber")[,.(name,code)]
 naics_from_any(326)
 naics_from_any(326, children = T)[,.(code,name)]
 naics_from_any("plastics", children=T)[,unique(n3)]
 naics_from_any("pig")
 naics_from_any("pig ") # space after g

 # naics_from_any("copper smelting")
 # naics_from_any("copper smelting", website_scrape=TRUE)
 # browseURL(naics_from_any("copper smelting", website_url=TRUE) )

 a = naics_from_any("plastics")
 b = naics_from_any("rubber")
 fintersect(a,b)[,.(name,code)] #  a AND b
 funion(a,b)[,.(name,code)]     #  a OR  b
 naics_subcodes_from_code(funion(a,b)[,code])[,.(name,code)]   #  plus children
 naics_from_any(funion(a,b)[,code], children=T)[,.(name,code)] #  same

 NROW(naics_from_any(325))
#[1] 1
 NROW(naics_from_any(325, children = T))
#[1] 54
 NROW(naics_from_any("chem"))
#[1] 20
 NROW(naics_from_any("chem", children = T))
#[1] 104
}

}
\seealso{
\code{\link[=latlon_from_naics]{latlon_from_naics()}}  \code{\link[=frs_from_naics]{frs_from_naics()}}  \code{\link[=naics_subcodes_from_code]{naics_subcodes_from_code()}} \code{\link[=naics_from_code]{naics_from_code()}}  \code{\link[=naics_from_name]{naics_from_name()}}
}

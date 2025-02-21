% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixmapheadernamescolname.R
\name{fixmapheadernamescolname}
\alias{fixmapheadernamescolname}
\title{utility to convert aliases to proper colnames of map_headernames
used by varinfo() and fixcolnames()}
\usage{
fixmapheadernamescolname(
  x,
  alias_list = list(rname = "r", longname = c("long", "longnames", "full",
    "description"), shortlabel = c("short", "shortname", "shortnames", "labels",
    "label"), acsname = "acs", apiname = "api", csvname = c("csv"), oldname =
    c("original", "old", "oldnames"))
)
}
\arguments{
\item{x}{character vector of colnames of map_headernames, or aliases like "long"}

\item{alias_list}{optional named list where canonical names (colnames in map_headernames)
are the names of vectors of alternative names}
}
\value{
vector where aliases are replaced with actual colnames and unmatched ones left as-is
}
\description{
utility to convert aliases to proper colnames of map_headernames
used by varinfo() and fixcolnames()
}
\examples{
  fixmapheadernamescolname(c('long', 'csv', 'api', 'r'))

}
\seealso{
\code{\link[=fixnames_aliases]{fixnames_aliases()}}

\code{\link[=varinfo]{varinfo()}} \code{\link[=fixnames_aliases]{fixnames_aliases()}}
}
\keyword{internal}

####################################################### #
# TO TRY plumber API ON LOCAL SERVER ####
####################################################### #

if (1 == 0) {

  ####################################################### #
  # you could START UP THE API this way below,
  # or simply open the plumber.R file in RStudio and click "Run API" button.

  # use the pr() function to translate this R file into a Plumber API
  # The pr object encapsulates all the logic represented in your plumber.R file.
  # To bring the API to life use the pr_run() method.
  # You should see a message about your API running on your computer on port 8000.
  # The API will continue running in your R session until you press the Esc key.
  #
  #   setwd(mysource()) # my own
  # If in a package, plumber.R MUST BE PUT IN /inst/plumber/  of SOURCE package,
  # since installing deletes folders like EJAM/plumber/
  #  and moves all in /inst/   to the root of installed version!
  # But while testing it is confusing since the location of the
  # plumber.R file differs from where it will be in the installed package.
  # This would refer to where it will be once installed:
    fname_installed <- system.file("plumber/plumber.R", package = "EJAM")
  # what was in inst folder of source pkg, installed version puts in root folder
  # This would refer to where it is during testing:
    fname_source <- "./inst/plumber/plumber.R"
  library(EJAM)
  library(plumber)
  root <- pr(fname_source)
  pr_run(root)

    ####################################################### #

  # If you’re running this code locally on your personal machine,
  # you can do what is below in a separate R session than the one running the API:

  # BUT YOU HAVE TO USE THE RIGHT FULL IP:PORT WHICH CHANGES EACH TIME !

  PORT = 3035

  ##############  #
  # try echo

  #  http://127.0.0.1:7705/echo?msg=asdf

  # browseURL(paste0("http://localhost:", PORT, "/echo"))



  ##############  #
  # try ejamit

  ##############  #
  url2 = paste0("http://127.0.0.1:", PORT, "/ejamit?lon=-101&lat=36&radius=1")
  req2 = httr2::request(url2)
  out2 = httr2::req_perform(req2)
  class(out2)
  str(out2)
  print(out2)
  ##############  #

  browseURL(url2)
  #  paste0("http://127.0.0.1:", PORT, "/ejamit?lat=31.34653&lon=-92.40151&radius=1.2&attachment=false&test=false")

  ##############  #

  #  31.34653 -92.40151
  browseURL(paste0("http://localhost:", PORT, "/ejamit"))


  url2 = paste0("http://127.0.0.1:", PORT, "/ejamit?lon=-101&lat=36&radius=1")
  browseURL(url2)


  url2csv = paste0("http://127.0.0.1:", PORT, "/ejamit_csv?lon=-101&lat=36&radius=1")
  browseURL(url2csv)


  rm(url2, req2, out2)

  ##############  #
  # try getblocksnearby
  #
  # browseURL(paste0("http://localhost:", PORT, "/getblocksnearby"))

  url3 = paste0("http://127.0.0.1:", PORT, "/getblocksnearby?lon=-101&lat=39&radius=1")

  req3 = httr2::request(url3)
  out3 = httr2::req_perform(req3)
  class(out3)
  str(out3)
  print(out3)

  browseURL(url3)


  ##############  #
  # try doaggregate
  #
  # browseURL(paste0("http://localhost:", PORT, "/doaggregate"))

}
# ####################################################### # # ####################################################### #
# ####################################################### # # ####################################################### #
# Help/notes on setting up API:
#
# browseURL("https://www.rplumber.io/articles/quickstart.html")
# browseURL("https://www.rplumber.io/articles/routing-and-input.html")
# browseURL("https://httr.r-lib.org/articles/api-packages.html")
# ####################################################### #
#  input formats
# parser_csv(): CSV parser. See readr::read_csv() for more details.
# parser_read_file(): Helper parser that writes the binary body to a file and reads it back again using read_fn. This parser should be used when reading from a file is required.
# parser_feather(...) See arrow::read_feather() for more details.
# parser_json(...) See jsonlite::parse_json() for more details. (Defaults to using simplifyVectors = TRUE)
# parser_read_file(read_fn = readLines)
# parser_multi(): Multi part parser. This parser will then parse each individual body with its respective parser. When this parser is used, req$body will contain the updated output from webutils::parse_multipart() by adding the parsed output to each part. Each part may contain detailed information, such as name (required), content_type, content_disposition, filename, (raw, original) value, and parsed (parsed value). When performing Plumber route argument matching, each multipart part will match its name to the parsed content.
####################################################### #
#  output formats
# # as_attachment()  and  serializer_headers()
####################################################### #
# you can leverage the @serializer contentType annotation
# which does no serialization of the response but specifies the contentType header.
# You can use this annotation when you want more control over the response that you send.
#
# #* @serializer contentType list(type="application/pdf")
# #* @get /pdf
# function(){
#   tmp <- tempfile()
#   pdf(tmp)
#   plot(1:10, type="b")
#   text(4, 8, "PDF from plumber!")
#   text(6, 2, paste("The time is", Sys.time()))
#   dev.off()
#
#   readBin(tmp, "raw", n=file.info(tmp)$size)
# }
#
# Running this API and visiting http://localhost:8000/pdf
# will download the PDF generated from R
# (or display the PDF natively, if your client supports it).

####################################################### #
# defining routers that serve a directory of static files:
# Static file routers are just a special case of Plumber routers created using pr_static().
# For example
#
# pr() %>%
#   pr_static("/assets", "./myfiles") %>%
#   pr_run()
# This will make the files and directories stored in the ./myfiles directory
# available on your API under the /assets/ path.

####################################################### #
# plumber.maxRequestSize
# Maximum length in bytes of request body. Body larger than maximum are rejected with http error 413. 0 means unlimited size. Defaults to 0.
# PlumberEndpoint$new()  creates a new endpoint from code
#
# pr_set_serializer() - Sets the default serializer of the router.
# pr_set_error() - Sets the error handler which gets invoked if any filter or endpoint generates an error.
# pr_set_404() - Sets the handler that gets called if an incoming request can’t be served by any filter, endpoint, or sub-router.


#' Set up EJAM (do slow initialization steps when package is attached)
#'
#' Download datasets, load to memory, index block locations
#'
#' @details This uses [dataload_dynamic()] and [indexblocks()]
#'
#' This function `.onAttach()` gets run when the package EJAM is attached,
#' which happens when library(EJAM) or require(EJAM) is used.
#' And if [devtools::load_all()] is used, which might mean it loads un-updated local copies
#' rather than the updated source copies in EJAM/data/ but presumably load_all() then replaces those by reading all from /data/
#'
#' This code would not get run if a server ran app.R as a regular shiny app (because of _disable_autoload.R ?)
#' and just used dataload or source to read the /R/*.R source files
#' rather than loading and attaching the EJAM package. see app.R   ***
#'
#' @param libname na
#' @param pkgname na
#'
#' @noRd
#'
.onAttach <- function(libname, pkgname) {

  # These instead could be set in the golem-config.yml file

  asap_download   <- TRUE  # download large datasets now?           Set to FALSE while Testing/Building often
  asap_index <- TRUE  # build index those now?                 Set to FALSE while Testing/Building often
  asap_bg    <- FALSE  # load now vs lazyload blockgroup data?  Set to FALSE while Testing/Building often

  # startup msg shown at library(EJAM) or when reinstalling from source ####
  packageStartupMessage("Now running .onAttach(), as part of attaching the EJAM package.")

  # get location of logo etc. so that ejam2report() using generate_html_header() can find logo  even without launching shiny app?
  source(system.file("global_defaults_package.R", package = "EJAM"), local = FALSE)

  # packageStartupMessage(
  #
  #   "\n
  #     Developers may want to modify the .onAttach() or even .onLoad() function,
  #     to control timing of the slow steps needed to load or create EJAM data,
  #     to have the delays occur at preferred times automatically
  #     (or only manually when a coder initiates them interactively or via their code).
  #
  #     These occur as EJAM starts:
  #
  #     1- Download the large datasets stored online (quaddata, blockpoints, frs, etc.)
  #        EJAM uses download_latest_arrow_data() for this. Also see dataload_dynamic()
  #        quaddata is >150 MB on disk and >200 MB RAM, while all others are smaller on disk.
  #        blockid2fips is roughly 600 MB in RAM because it stores 8 million block FIPS as text.
  #
  #     2- Build the national index of all block point locations
  #        EJAM function indexblocks() does this, using quaddata - see ?indexblocks
  #
  #     3- Load into memory some datasets installed with EJAM (blockgroupstats, usastats, etc.)
  #        EJAM function dataload_from_package() can do this - see ?dataload_from_package and ?datapack()
  #        Otherwise these are only lazyloaded at the moment they are needed, making a user wait.
  #        blockgroupstats (>60 MB on disk, >200 MB in RAM) and usastats, statestats are essential.
  #        frs-related tables are huge and not always required - needed to look up regulated sites by ID.
  #
  #     These are the times at which you may want them to happen:
  #
  #     - when the EJAM package is loaded and/or attached
  #       i.e., every time the source package is rebuilt it is loaded; but it is attached less often,
  #       as when a coder uses require( ) in RStudio or script
  #
  #     - when the shiny app launches and runs the global_defaults_*.R scripts
  #        i.e., only once a new user opens the app and their session starts,
  #        and when a coder uses run_app(), either after library( ), or by using EJAM function run_app()
  #
  #     - once the app or coder actually needs a given dataset that is available for lazyLoad, which
  #       works only for data in EJAM/data/ like frs.rda, frs_by_programid.rda, frs_by_sic.rda, etc.
  #       See utils::data( package = 'EJAM' )
  #
  #     - only if manually obtained by coder via functions noted above.
  #     \n\n"
  # )

  # download BLOCK (not blockgroup) data, etc ####

  if (asap_download) {

    if (length(try(find.package("EJAM", quiet = T))) == 1) { # if it has been installed. but that function has to have already been added to package namespace once
      dataload_dynamic(varnames = c("blockpoints", "blockwts", "quaddata"),
                         folder_local_source = app_sys('data'),
                         onAttach = TRUE) # use default local folder when trying dataload_from_local()
      # EJAM function ... but does it have to say EJAM :: here? trying to avoid having packrat see that and presume EJAM pkg must be installed for app to work. ***
    }

    #################### #
    #   blockid2fips was used only in  state_from_blockid(), which is no longer used by testpoints_n(),
    #     so not loaded unless/until needed.
    #     Avoids loading the huge file "blockid2fips" (100MB) and just uses "bgid2fips" (3MB) as needed, that is only 3% as large in memory.
    #     blockid2fips was roughly 600 MB in RAM because it stores 8 million block FIPS as text.
    ######################### #
  }

  # create index of all US block points, to enable fast queries ####

  if (asap_index) {

    if (length(try(find.package("EJAM", quiet = T))) == 1) { # if it has been installed. but that function has to have already been added to package namespace once

      indexblocks()   # EJAM function works only AFTER shiny does load all/source .R files or package attached
    }
  }

  # load blockgroupstats etc. from package? ####
  ## This only makes sense if they cannot be lazyloaded (impossible since .onAttach() is running?),
  ##  or if you want to preload them to avoid a user waiting for them to load when they are needed,
  ##  but lazyloading blockgroupstats and statestats and usastats should be pretty fast and forcing data()
  ##  to happen here is a bit slow if you have to reload the pkg many times like when iterating, building documentation etc.
  ##  And it slightly delays the shiny app launch.

  if (asap_bg) {

    if (length(try(find.package("EJAM", quiet = T))) == 1) { # The first time you try to install the package, it will not have access to EJAM :: etc. !

      dataload_from_package() # EJAM function works only AFTER shiny does load all/source .R files or package attached
    }

    # load BLOCKGROUP (not block) data (EJScreen data), etc. from package
    # see ?dataload_from_package()
    # This loads some key data, while others get lazy loaded if/when needed.
    # data(list=c("blockgroupstats", "usastats", "statestats"), package="EJAM")
    # # would work after package is installed
    # data(list=c("frs", "frs_by_programid ", "frs_by_naics"),  package="EJAM")
    # # would be to preload some very large ones not always needed.
  }

  packageStartupMessage('For help using the EJAM package in RStudio:
                          ?EJAM
                        To launch shiny app locally:
                          run_app()
                        ')

}

############################################################# # ############################################################# #
{
  # Notes on initial setup of pkgdown site ####
  #

  # Note the URL of the repository publishing the pkgdown site belongs
  # in DESCRIPTION and also must be separately placed 
  # in _pkdgown.yml
  #

  # To specify which branch site gets published from:
  #    https://github.com/OWNERGOESHERE/REPONAME/settings/pages ( REPONAME may be EJAM )
  #    to get likely URL from DESCRIPTION file:  EJAM:::repo_from_desc(get_full_url = T)
  # see https://pkgdown.r-lib.org/articles/pkgdown.html#configuration
  # see https://usethis.r-lib.org/reference/use_pkgdown.html
  ### also possibly of interest:
  # devtools::build_manual()          # for a  pdf manual
  # postdoc::render_package_manual()  # for an html manual
  ############################################################# #
  #
  ##   Initial setup of authorization tokens:
  #
  ## 1st confirm personal access token PAT exists and not expired
  ##  (to allow use of github API to create new branch gh-pages, create github action, etc.)
  #     To check on PATs:
  # usethis::gh_token_help() and
  # usethis::git_sitrep() # git situation report
  ##    To make a PAT:
  # usethis::create_github_token()
  ##    To register a PAT, see
  # credentials::set_github_pat()
  ##    https://usethis.r-lib.org/articles/git-credentials.html#git-credential-helpers-and-the-credential-store
  ##    Windows more or less takes care of this for you now, in conjunction with github.
  ############################################# #
  #
  ##   Initial setup of github pages website using pkgdown:
  #
  # usethis::use_github_pages(branch = "main", path = "/docs")
  ##   does
  # usethis::use_pkgdown()   and does other steps,
  ##   but note it replaces/deletes any existing _pkgdown.yml file
  #
  #   Traditional (not pkgdown) vignettes are no longer recommended by roxygen2 pkg docs:
  # see   help(vignette_roclet, package = "roxygen2")
  # Can turn them off in RStudio "Build" - "Configure Build Options" - "Build Tools" - "Generate Docs with Roxygen"
  #   and by adding   --no-build-vignettes to the "Build Source Package" field in your project options.
}
############################################################# # ############################################################# #


# REBUILDS PACKAGE DOCUMENTATION AND vignettes  (articles) using pkgdown


# Define update_pkgdown() ####

update_pkgdown = function(
    doask              = FALSE,
    dotests            = FALSE,
    testinteractively  = FALSE, ## maybe we want to do this interactively even if ask=F ?
    doyamlcheck        = TRUE, ## dataset_pkgdown_yaml_check() does siterep but also check internal v exported, listed in pkgdown reference TOC etc.
    dodocument         = TRUE,  ## in case we just edited help, exports, or func names,
    ##   since doinstall=T via this script omits document()
    doinstall          = TRUE,  ## but skips document() and vignettes
    doloadall_not_library = TRUE, ## (happens after install, if that is being done here)
    dobuild_site      = TRUE     ## use build_site() or stop?

) {

  ############################################################# # ############################################################# #
  ############################################################# # ############################################################# #

  # setup ####

  if (!interactive()) {doask <- FALSE}
  golem::detach_all_attached()
  library(devtools) # library() stops with error where require() would only warn
  library(pkgdown)
  ############################################################# #

  # ask what to do ####

  if (doask && interactive() && rstudioapi::isAvailable()
      && missing("dotests")
  ) {dotests <- utils::askYesNo("Do you want to run tests 1st?")}
  if (is.na(dotests)) {stop('stopped')}

  if (doask && interactive() && rstudioapi::isAvailable() &&
      dotests
      && missing("testinteractively")
  ) {testinteractively <- utils::askYesNo("Do you want to answer questions about the tests to run?")}
  if (is.na(testinteractively)) {stop('stopped')}

  if (doask && interactive()  && rstudioapi::isAvailable()
      && missing("doyamlcheck")
  ) {doyamlcheck <- utils::askYesNo("Use dataset_pkgdown_yaml_check() to see which functions are missing in function reference etc.? Note it will run load_all() first so that pkgdown::pkgdown_sitrep() can check the latest source versions of docs not just installed version")}
  if (is.na(doyamlcheck)) {stop('stopped')}

  if (doask && interactive()  && rstudioapi::isAvailable()
      && missing("dodocument")
  ) {dodocument <- utils::askYesNo("Do document() now since just installing via this script wont do document() ?")}
  if (is.na(dodocument)) {stop('stopped')}

  if (doask && interactive()  && rstudioapi::isAvailable()
      && missing("doinstall")
  ) {doinstall <- utils::askYesNo("Do you want to re-install the package? This wont redo document()")}
  if (is.na(doinstall)) {stop('stopped')}

  if (doask && interactive()  && rstudioapi::isAvailable()
      && missing("doloadall_not_library")
  ) {doloadall_not_library  <- utils::askYesNo("do load_all() instead of library(EJAM) ?")}
  if (is.na(doloadall_not_library)) {stop('stopped')}
  
  #################### #

  # UNIT TESTS ####

  if (dotests) {
    cat('doing unit tests \n')
    print(Sys.time())
    source("./tests/test_interactively.R")
    # rstudioapi::documentOpen("./tests/test_interactively.R")
    test_interactively(ask = doask & interactive() & testinteractively )
    print(Sys.time())
    # also see  devtools::test()
  }
  #################### #
  
  cat("\n\n   ------------- STARTED -------------- \n")
  print(Sys.time())
  cat("\n\n")  
  #################### #
  
  # checkDocFiles() ####
  
  tools::checkDocFiles(dir = ".")
  
  # ? check() ?
  # devtools::check()
  ##   automatically builds and checks a source package, using all known best practices.
  # devtools::check_man()
  # devtools::check_built() checks an already-built package.

  #################### #

  # _pkgdown.yml check ####

  if (doyamlcheck) {
    #cat('Using load_all() 1st, before using dataset_pkgdown_yaml_check() ... \n')
    #devtools::load_all(quiet = T, helpers = F, export_all = T)
    #    dataset_pkgdown_yaml_check() will not work without the unexported dataset_pkgdown_yaml_check() available
    cat('Using dataset_pkgdown_yaml_check() which includes pkgdown_sitrep(), which reports status of all checks ... \n')
    missing_from_yml <- EJAM:::dataset_pkgdown_yaml_check() #  needs EJAM::: if haven't just done load_all(export_all=T)
    # that prints some results to console.
    # `pkgdown::pkgdown_sitrep()` does, among other things, 
    #   confirm the URL for publishing the pkgdown site listed in _pkgdown.yml 
    #   matches what is in DESCRIPTION
    #    at  EJAM:::repo_from_desc('github.io', get_full_url = TRUE)
  
    if (doask && interactive()  && rstudioapi::isAvailable()) {
      cat('\n\n')
      yn <- utils::askYesNo("Halt now to edit/fix _pkgdown.yml etc. ?")
      if (is.na(yn) || yn == TRUE) {
        if (file.exists('_pkgdown.yml')) {rstudioapi::documentOpen('_pkgdown.yml')}
        message('stopped to fix _pkgdown.yml')
        cat('\n\n MISSING according to dataset_pkgdown_yaml_check() are the following: \n\n')
        print(dput(missing_from_yml))
        return(missing_from_yml)
        # stop('stopped to fix _pkgdown.yml')
        }
    }
  }
  #################### # #################### # #################### # #################### #

  # README & DOCUMENT ####

  if (dodocument) {
    cat('rendering README.Rmd to .md  \n')
    print(Sys.time())
    rmarkdown::render("README.Rmd")  # renders .Rmd to create a  .md file that works in github as a webpage

    # build_rmd() would take a couple minutes as it installs the package in a temporary library
    # build_rmd() would just be a wrapper around rmarkdown::render() that 1st installs a temp copy of pkg, then renders each .Rmd in a clean R session.
    #################### # #################### # #################### # #################### #
    cat('detaching packages  \n')
    golem::detach_all_attached()
    library(devtools) # library() stops with error where require() would only warn
    library(pkgdown) # library() stops with error where require() would only warn

    # MAYBE NEED TO DELETE ALL IN THE man/ FOLDER TO REMOVE OBSOLETE .Rd files like no longer documented or renamed functions ?
warning("You might need to delete all of /man/*.* to be sure there is nothing obsolete like renamed or deleted or no-longer-documented functions")
    
    cat('trying to do document() \n')
    document()
  }
  #################### # #################### # #################### # #################### #

  # INSTALL ####

  if (doinstall) {

    cat('doing install()  \n')  # there may be problems with this step being done from this function?
    print(Sys.time())
    system.time({

      # 4+ minutes for install()

      # Usually just use devtools::load_all()  during development, not re-install every time you edit source.

      # BUT, using devtools::install() will ensure anything that uses the INSTALLED version will work!

      # note, If you want to build/install using RStudio buttons, not the function install(), need to
      #   1st confirm you already turned off traditional vignette-building...  see   help(vignette_roclet, package = "roxygen2")
      #   That button includes a step that is the same as   devtools::document()

      devtools::install(

        quick = TRUE,   # USUALLY LEAVE IT AS TRUE
        # # quick=T is MUCH faster but skips docs, vignettes, etc., building 'EJAM_x.xx.xx.tar.gz' or the .zip binary, etc.
        # # quick=F is SLOW!  takes a few minutes!

        upgrade = FALSE,
        dependencies = FALSE, # skip checking/installing all dependencies here

        build_vignettes = FALSE,
        ## old-style vignettes were in  doc folder, but pkgdown-style are in   docs folder,

        build = FALSE,
        ## build = TRUE means it converts a package source directory into a single bundled file...
        ##   If binary = FALSE this creates a tar.gz package that can be installed on any platform, provided they have a full development environment (although packages without source code can typically be installed out of the box).
        ##   If binary = TRUE, the package will have a platform specific extension (e.g. .zip for windows), and will only be installable on the current platform, but no development environment is needed.

        quiet = FALSE
      )
      #################### #
      cat('detaching packages - RESTART R IF THIS FAILS  \n') # got Error: lazy-load database '....EJAM/R/EJAM.rdb' is corrupt
      golem::detach_all_attached()
      # rstudioapi::restartSession() might be needed.
      library(devtools) # library() stops with error where require() would only warn
      library(pkgdown)
    })
  }
  #################### # #################### # #################### # #################### #
  # rmost() ####
  ########### but rstudio build button makes it try to load data and it connects to pins but does not use those yet-
  # tries to use local copies and fails to get .arrow files from local path supposed to be ~/../Downloads/......
  # so it loads the .rda from aws that are older and not all files are there.
  ## why did it not use the pins versions since it did connect? and why not found in that local path???
  ## so did rm(list=ls()) and tried to continue from library( ) above .
  # cat('doing rm() for most objects  \n')
  # EJAM:::rmost(notremove = c('doask', 'dotests', 'testinteractively',   'dodocument', 'doinstall', 'doloadall_not_library', 'dobuild_site', "dataload_pin_available"))
  # doask              = FALSE,
  # dotests            = FALSE,
  # testinteractively  = FALSE, ## maybe we want to do this interactively even if ask=F ?
  # doyamlcheck        = TRUE, ## dataset_pkgdown_yaml_check() does siterep but also check internal v exported, listed in pkgdown reference TOC etc.
  # dodocument         = TRUE,  ## in case we just edited help, exports, or func names,
  # ##   since doinstall=T via this script omits document()
  # doinstall          = TRUE,  ## but skips document() and vignettes
  # doloadall_not_library = TRUE, ## (happens after install, if that is being done here)
  # dobuild_site      = TRUE     ## use build_site() or just stop?
  #################### # #################### # #################### # #################### #
    
  # LOAD ALL FROM SOURCE  ####
    
  print(Sys.time())
  if (doloadall_not_library) {
    cat('detaching packages, then doing load_all() \n')
    golem::detach_all_attached()
    library(devtools) # library() stops with error where require() would only warn
    library(pkgdown)
    devtools::load_all() # got error lazy-load database ... corrupt
  } else {
    cat('doing library(EJAM) \n')
    x = try( library(EJAM) ) # library() stops with error where require() would only warn
    if (inherits(x, "try-error")) {stop("cannot do library(EJAM) ... try restarting R")}
    rm(x)
  }

  #################### # #################### # #################### # #################### #

  ## 2 options for how to keep site updated, from pkgdown intro https://pkgdown.r-lib.org/articles/pkgdown.html
  #
  # A) If you’re using GitHub, we recommend setting up pkgdown and GitHub actions
  # (e.g., https://github.com/r-lib/actions/tree/v2-branch/examples#build-pkgdown-site )
  # to automatically build and publish your site:
  #  Run this ONCE EVER to have github actions re-publish your site regularly:
  #
  #   usethis::use_pkgdown_github_pages()  # only ONCE
  #
  # B) But, if not using GitHub (or if GitHub Actions have trouble rendering vignettes to html
  #   due to lacking access to pins board etc.)
  #   then you'll have to run this manually EVERY TIME you want to update the site:

  #   pkgdown::build_site()

  # # ~ ####
  # ** BUILD SITE (HTML FILES) ####

  if (dobuild_site) {
    cat("Doing build_site()  \n")
    print(Sys.time())

    # MAYBE NEED TO DELETE ALL IN THE docs/ FOLDER TO REMOVE OBSOLETE .html files like no longer used vignettes ?
    warning("You might need to delete all of /docs/*.*  to be sure there is nothing obsolete like renamed or deleted or no-longer-documented functions?")
    

    pkgdown::build_site(
      examples = FALSE, lazy = TRUE,
      devel = FALSE,
      install = FALSE, new_process = FALSE
    )

    ### NOTES:
    #
    # https://pkgdown.r-lib.org/reference/build_site.html
    # build_site() is a convenient wrapper around six functions:
    #
    # init_site()
    # build_home()
    # build_reference() & index - ** THIS TAKES FOREVER **   (could perhaps do as bkgd job)
    # build_articles()  - THIS TRIES TO RUN THE CODE IN VIGNETTES/ARTICLES AND RENDER THEM
    # build_tutorials() - NA
    # build_news()
    # build_redirects() / sitemap/ search index
    #
    # build_site_github_pages() is for use in a github action, and would do more steps:
    #  build_site(), but then also gets metadata about package and does
    #  clean_site() and
    #  build_github_pages()

  } else {
    # pkgdown::build_site_github_pages() is meant to be used as part of github actions
    #   # https://pkgdown.r-lib.org/reference/build_site_github_pages.html
  }
  
  print(Sys.time())
  
  ################################################################## #
  # # ~ ####
  # remember to push so gh actions publish it ####
  if (TRUE) {
    cat( '\n\n NOW COMMIT AND PUSH THE NEW FILES \n\n')
    cat("Github actions in the repo will deploy from the docs folder to gh pages. \n")
  }
  ################################################################## #
  # note: how to build pkg as a single file ####
  ## Building converts a package source directory into a single bundled file.
  ## If binary = FALSE (default) this creates a tar.gz package that can be installed on any platform,
  ##    except note if the pkg has source code [that needs to be compiled, meaning C?] they must have a full development environment (Rtools, etc.?).
  ## If binary = TRUE, the package will have a platform specific extension (e.g. .zip for windows),
  ##    and will only be installable on the current platform, e.g., Windows only, (but no development envt needed, even if pkg had C, which needs to be compiled platform-specific).
    ## To build a package you can use  rstudio  menu, build ...
    ##     or in RStudio console,
    ##  build(".")
    ##  ?build
  ################################################################## #
}
############################################################# # ############################################################# #
# ~ ####
# HOW TO USE THIS FUNCTION ####

cat(
  "
To use this function:

update_pkgdown(doask = TRUE)

or change from any of these defaults:

update_pkgdown(
  doask              = FALSE,
  dotests            = FALSE,
  testinteractively  = FALSE, 
  doyamlcheck        = TRUE,  ## dataset_pkgdown_yaml_check() does siterep but also check pkgdown reference TOC etc.
  dodocument         = TRUE,  ## document() -- in case we just edited help, exports, or func names
  doinstall          = TRUE,  ## installs but skips possibly redoing document()
  doloadall_not_library = TRUE, ## (after installs, if that is being done here)
  dobuild_site      = TRUE      ## does build_site() to create new pkgdown site html files in /docs/
)

")
# cat("\n  Current defaults: \n\n")
# x = (EJAM:::args2(update_pkgdown)); rm(x)
############################################################# # ############################################################# #

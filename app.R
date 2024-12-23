# Launch the ShinyApp (Do not remove this comment)

# This app.R file is used by the RStudio Connect server to launch the app since the 
#                 EJAM
#  app is a package unlike a typical shiny app,
#  and run_app() is loaded as an exported function that actually runs the app,
#  and while shiny normally sources all files in the /R folder, 
#  here _disable_autoload.R is used to avoid that 
#  since they are already loaded below via pkgload::load_all() which sources R files and loads data files. 
#
#  This was created using  golem::add_rstudioconnect_file()
#  But note that this add_rstudioconnect_file() function says 
#  one needs to say pkgload::myfunction() to refer to the package functions  
#  Need to clarify when/why/if... Within server code but not when used as non-shiny functions 
#
# To deploy, run:   rsconnect::deployApp()
# Or use the blue button on top of this file

# rm(list = ls())
# golem::detach_all_attached()


options( "golem.app.prod" = FALSE)

# EJAM::
library(EJAM)
run_app(isPublic = TRUE)  # if EJAM is not installed and loaded and attached, how would a server running app.R know what this function is??
  # shiny does not actually source all the .R files until when, exactly? I dont think it would have by now? ***
  
  # aka app_run_EJAM() 
    # which does a lot... including  source(system.file("global.R", package = "EJAM")) # source('./inst/global.R') 
    # and then shiny::shinyApp()


# add parameters here (if any)?

# maybe it should just say run_app() so it runs whatever version was loaded here by load_all() 
# and not rely partly on the package being an installed package.

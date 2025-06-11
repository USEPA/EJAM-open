

# devtools::load_all()  # need this if not yet done by testing setup 

## source app-related scripts ?
# source('R/app_config.R')
# source('R/app_ui.R')
# source('R/app_server.R')
cat("NOTE: global_defaults_*.R are required - be aware of whether installed or local source version will be used by tests in test-ui_and_server.R \n")
# and  update_global_defaults_or_user_options() # is used by get_global_defaults_or_user_options()
global_defaults_or_user_options <- EJAM:::get_global_defaults_or_user_options(
  user_specified_options = list()
)


cat("\n NEED MORE UNIT TESTS OF SHINY APP IN test-ui_and_server.R \n\n")

# Configure   to fit your need.
# testServer() function makes it possible to test code in server functions and modules, without needing to run the full Shiny application


test_that("app ui", {

  if (!exists("app_ui")) {
    cat("app_ui() is an unexported function -- cannot be tested without devtools::load_all() to test the local source version,
      or using ::: to test the installed version")
  }
  skip_if_not(exists("app_ui"), message = "unexported function app_ui() not found, skipping test")
  ui <- app_ui() # unexported function, so would require using ::: or devtools::load_all()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(app_ui)
  for (i in c("request")) {
    expect_true(i %in% names(fmls))
  }
})

test_that("app server is a function", {

  if (!exists("app_server")) {
    cat("app_server() is an unexported function -- cannot be tested without devtools::load_all() to test the local source version,
      or using ::: to test the installed version")
  }
  skip_if_not(exists("app_server"), message = "unexported function app_server() not found, skipping test")
  server <- app_server # unexported function, so would require using ::: or devtools::load_all()
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals( app_server) # unexported function, so would require using ::: or devtools::load_all()
  for (i in c("input", "output", "session")) {
    expect_true(i %in% names(fmls))
  }
})

test_that(
  "app_sys works and finds golem-config.yml", {

    if (!exists("app_sys")) {
      cat("app_sys() is an unexported function -- cannot be tested without devtools::load_all() to test the local source version,
      or using ::: to test the installed version")
    }
    skip_if_not(exists("app_sys"), message = "unexported function app_sys() not found, skipping test")
    expect_true(
      file.exists(
      app_sys("golem-config.yml") # this gets path to source version of .yml ## unexported function, so would require using ::: or devtools::load_all()
      )
      # != ""   #  source/EJAM/inst/golem-config.yml = installed/EJAM/golem-config.yml
    )
  }
)

test_that(
  "golem-config works and app is set as 'production' not  'dev' ", {
    if (!exists("get_golem_config")) {
      cat("get_golem_config() is an unexported function -- cannot be tested without devtools::load_all() to test the local source version,
      or using ::: to test the installed version")
    }
    skip_if_not(exists("app_sys"), message = "unexported function app_sys() not found, skipping test")
    config_file <- app_sys("golem-config.yml") # this gets path to source version of .yml # unexported function, so would require using ::: or devtools::load_all()
    #  source/EJAM/inst/golem-config.yml = installed/EJAM/golem-config.yml
    skip_if(config_file == "", message = "golem-config.yml file not found, skipping test")

    skip_if_not(exists("get_golem_config"), message = "get_golem_config not found, skipping test")
    expect_true(
     get_golem_config( # unexported function, so would require using ::: or devtools::load_all()
        "app_prod",
        config = "production",
        file = config_file
      )
    )
    expect_false(
     get_golem_config( # unexported function, so would require using ::: or devtools::load_all()
        "app_prod",
        config = "dev",
        file = config_file
      )
    )
  }
)

################################################# #
### Configure this test to fit your need.
### testServer() function makes it possible to test code in server functions and modules, without needing to run the full Shiny application
### but seems to throw an error when running this test file via  test_file("./tests/testthat/test-ui_and_server.R")
###  cannot get this testServer to work without an error when running the test in console interactively
  
# 
# devtools::load_all()
# 

# testServer(app = app_server, expr = {  # unexported function, so would require using ::: or devtools::load_all()
# 
#   suppressWarnings({
#   ## Set and test an input
#    session$setInputs(bt_rad_buff = 1, max_miles = 10, default_miles = 3.14,
#        ss_choose_method = "upload", ss_choose_method_upload = "latlon")
#   ### stopifnot(input$bt_rad_buff == 1)
#     expect_equal(input$bt_rad_buff, 1)
#   })
# 
#   ### Example of tests you can do on the server:
#   ### - Checking reactiveValues
# 
#   expect_equal(r$lg, 'EN')
# 
# ## - Checking output
# 
#   expect_equal(output$txt, "Text")
# 
# 
# })
################################################# #
#
# ## this is not finished yet
# 
# # https://shiny.posit.co/r/reference/shiny/1.7.2/testserver
# # Configure this test to fit your need

test_that(
  "app can start and inputs can be set",
  {
    testServer(app = app_server, expr = {

      golem::expect_running(sleep = 5)
      print(current_upload_method())

        suppressWarnings({
        ## Set and test an input
         session$setInputs(bt_rad_buff = 3.14, max_miles = 10, default_miles = 3.14,
             ss_choose_method = "upload", ss_choose_method_upload = "latlon")
        ### stopifnot(input$bt_rad_buff == 1)
          # expect_equal(input$bt_rad_buff, 1)
          # expect_equal(input$max_miles, 10)
        })

    })

  }
)
################################################# #

# # TESTING A MODULE ####
#
# if (!exists("mod_ejscreenapi_server")) {
#   cat("mod_ejscreenapi_server() is an unexported function -- cannot be tested without devtools::load_all() to test the local source version,
#       or using ::: to test the installed version")
# }
# # # attempting to be able to test a module... not working yet... need session, etc.
#
# test_that("mod_ejscreenapi_server  receives its input", {
#   skip_if_not(exists("mod_ejscreenapi_server"), message = "mod_ejscreenapi_server() not found, skipping test")
#    # unexported function, so would require using ::: or devtools::load_all()
#
#   shiny::testServer(mod_ejscreenapi_server, {
#     session$setInputs(pointsfile = list(datapath= system.file("testdata/latlon/"testpoints_5.xlsx")     )
#     expect_equal( output$count, 3)
#   })
# })


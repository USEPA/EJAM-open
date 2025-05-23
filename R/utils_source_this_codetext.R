
# see also  calc_ejam() 

# source_this_codetext()  - allows execution of any code in the text
# source_this_codetext_careful() - allows execution of only functions explicitly allowed
#   ONLY ALLOWS EXTREMELY LIMITED OPERATIONS AS CODED unless eval_envir = globalenv()

######################################## #


######################################## #
## example showing how it works
#
# formula_varname(formulas_d)
# 
# bgdf <- data.frame(blockgroupstats)
# newdf <- ejscreen::ejscreen.acs.calc(bgdf,
#                                      keep.old = c('bgid', 'pop', 'hisp'),
#                                      keep.new = "all",
#                                      formulas = formulas_d
#                                      )
# round(t(newdf[1001:1002, ]), 3)
# 
# cbind(
#   newdf[11001:11020, c('hisp', 'pop', 'pcthisp')],
#   check = (newdf$hisp[11001:11020] / newdf$pop[11001:11020])
# )
# ## note the 0-100 percentages in blockgroupstats versus the 0-1 calculated percentages
#  rm(bgdf, newdf)
######################################## #
###   example:
# source_this_codetext_careful("(a+b)^c - d", list(a = 1, b = 2, c = 3, d = 4))
# # [1] 23
# formulas_d[1] 
# source_this_codetext_careful(
#   "1 * ifelse(pop == 0, 0, over64 / pop)", 
#   data_list = list(pop = blockgroupstats$pop[10001:10003], over64 = blockgroupstats$over64[10001:10003])
# )
# source_this_codetext_careful("list.files()", list())
# # Error in list.files() : could not find function "list.files"
# 
# source_this_codetext_careful("list.files()", list(),
#    eval_envir = rlang::new_environment(list("list.files" = list.files)))
# 
# # succeeds in listing my files if i explicitly allow it
######################################## #
### example using just 10 block groups from 1 county in Delaware 
#  c1 <- fips2countyname(fips_counties_from_state_abbrev('DE'), includestate = F)[1]
#  bgdf = data.frame(EJAM::blockgroupstats[ST == "DE" & countyname == c1, ])[1:10, ]
# 
#  newdf <- calc_ejam(bgdf, keep.old = "",
#    formulas = c(
#      "my_custom_recalc_demog <- (pctlowinc + pctmin)/2",
#      "mystat2  = 100 * pctlowinc"))
# cbind(Demog.Index = bgdf$Demog.Index, newdf, pctlowinc = bgdf$pctlowinc)
#
# newdf <- calc_ejam(bgdf, formulas = formulas_d)
# newdf
######################################## #




source_this_codetext_careful <- function(text_expression, data_list = NULL, eval_envir = NULL) {
  
  # DRAFT WORK NOT COMPLETED - 
  
  # # A safer implementation was in this example, which 
  #  allows only arithmetic functions explicitly enabled:
  # 
  #  # found at https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
  #  ##### eval_text_expression <- function(text_expression, data_list, eval_envir = NULL) {
  
  # argument checks
  stopifnot(is.character(text_expression) && length(text_expression) == 1)
  if (!is.null(data_list)) {
    stopifnot(is.list(data_list))
    stopifnot(length(data_list) == 0 || (!is.null(names(data_list)) && all(names(data_list) != "")))
    stopifnot(all(!(lapply(data_list, typeof) %in% c('closure', 'builtin'))))
  } else {
    
  }
  stopifnot(is.null(eval_envir) || is.environment(eval_envir))
  # default environment for convenience
  if (is.null(eval_envir)) {
    arithmetic_funcs <- list("+" = `+`, "-" = `-`, "*" = `*`, "/" = `/`, "^" = `^`, "(" = `(`, "==" = `==`)
    
    ### EXAMPLES OF OTHER FUNCTIONS TO ENABLE ??? 
    arithmetic_funcs <- c(arithmetic_funcs, list("sum" = sum, "min" = min, "max" = max, "sqrt" = sqrt, "ifelse" = ifelse)) 
    
    eval_envir = rlang::new_environment(data = arithmetic_funcs, parent = rlang::empty_env())
  }
  # load any data objects into evaluation environment, then evaluate expression
  if (!is.null(data_list)) {
    eval_envir <- list2env(data_list, envir = eval_envir)
  }
  eval(parse(text = text_expression, keep.source = FALSE), eval_envir)
}
######################################## #


################################################################ #


source_this_codetext <- function(codetext, env = parent.frame()) {
  
  # useful tool because it is hard to remember how to do this:   eval(parse(text = 
  # and that can be useful because we...
  
  # may want to store formulas for indicators as a table of metadata 
  # (rather than formulas being buried in code like doaggregate 
  # or even pulled out into a function focused on just that)
  
  # and/or want to allow user to specify a custom indicator or summary stat via formula they provide as text,
  # so EJAM could then aggregate and report the new stat alongside the built-in indicators.
  
  # see datacreate_formulas.R 
  
  return(
    eval(parse(text = codetext), envir = env)
  )
  
  ## EJAM::calc_ejam() which uses calc_byformula() 
  ## was based on 
  ## ejscreen::ejscreen.acs.calc() which used analyze.stuff::calc.fields() 
  ##
  # https://rdrr.io/github/ejanalysis/ejscreen/man/ejscreen.acs.calc.html
  # or more generally here
  #  https://rdrr.io/github/ejanalysis/analyze.stuff/man/calc.fields.html
  #  stored at
  #  https://github.com/ejanalysis/analyze.stuff
  #  https://github.com/ejanalysis/analyze.stuff/blob/36afe6b102cb2cef90b87a48dfea9479b1a2447a/R/calc.fields.R
  #  devtools::install_github("ejanalysis/analyze.stuff")
  #  ?analyze.stuff::calc.fields()
  # 
  # example using just 10 block groups from 1 county in Delaware 
  #   c1 <- fips2countyname(fips_counties_from_state_abbrev('DE'), includestate = F)[1]
  #   bgdf = data.frame(EJAM::blockgroupstats[ST == "DE" & countyname == c1, ..names_d])[1:10, ]
  #    newdf <-  ejscreen::ejscreen.acs.calc(bgdf, keep.old = "", keep.new = c("my_custom_stat", "mystat2"), formulas = c(
  #      "my_custom_stat <- (pctlowinc + pctmin)/2", 
  #      "mystat2  = 100 * pctlowinc"))
  # newdf
  
}
################################################################ #

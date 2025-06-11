
################################################################### #
if (FALSE) {

  # see styler and lintr packages
  # styler makes the changes. lintr just flags them.
  
  # https://lintr.r-lib.org/
  # https://styler.r-lib.org/
  
  ### RStudio addins from lintr and styler packages can lint/style 
  # a single active open source file, or entire package. 
  #
  # https://styler.r-lib.org/articles/styler.html
  #
  # styler provides the following API to format code:
  #   
  #   style_file() styles .qmd, .R, .Rmd, .Rmarkdown, .Rnw, and .Rprofile files.
  # 
  # style_dir() styles all these files in a directory.
  # 
  # style_pkg() styles the source files of an R package.
  # 
  # RStudio Addins for styling the active file, styling the current package and styling the highlighted selection, see help("styler_addins").
  # 
  
  # !diagnostics off   ## would  disable diagnostics within a document
  
  
  
  ###########  PICK SOME USEFUL linters in lintr package:
  
  #      see bundles of useful ones here:
  
  #  ?lintr::linters
  
  # library(lintr)
  #
  # > names(linters_with_tags('common_mistakes'))
  # [1] "duplicate_argument_linter" "equals_na_linter"          "length_test_linter"       
  # [4] "missing_argument_linter"   "missing_package_linter"    "redundant_equals_linter"  
  # [7] "sprintf_linter"            "unused_import_linter"     
  #  names(linters_with_tags('correctness'))
  
  # commented_code_linter    -- good suggestion, and there is LOTS of this everywhere!
  
  # infix_spaces_linter  -- is like the lint_equals() function drafted below ! and also flags 1/3 etc.
  # commas_linter   -- like some of the misc code drafted below !
  
  # assignment_linter  -- good suggestion, changes = to <- where appropriate.
  # T_and_F_symbol_linter -- good suggestion
  # line_length_linter  -- good suggestion
  
  ## quotes_linter -- says to only use double-quotes but disagree.
  ## object_usage_linter gets many many false hits due to data.table (and some datasets)
  
  mine = c(
    linters_with_tags('common_mistakes'),
    list( # correctness minus object usage
      duplicate_argument_linter(),
      equals_na_linter(),
      missing_argument_linter(),
      namespace_linter(),
      package_hooks_linter(),
      sprintf_linter()
    )
  )
  # ### but not "object_usage_linter" 
  
  ###########    USE THOSE LINTERS:
  
  ## to line one file, using 1 linter
  #
  # lint('./R/ejamit.R', linters = list(line_length_linter())) 
  
  ## to lint one file, using linters tagged with these tags:
  #
  # lint('./R/ejamit.R', linters = linters_with_tags(tags = c(
  #    "common_mistakes", "correctness", "robustness"
  # )))
  
  ## to lint one file, using a custom list of linters
  #
  # lint('./R/ejamit.R', linters = mine)     
  
  ## to lint ALL files in the R folder, using a custom list of linters
  #
  # lint_dir("./R", linters = mine)
  
  ###########  
}
################################################################### #
################################################################### #
################################################################### #




########### as alternative or supplement to lintr pkg, 
## draft code below would make some relevant changes:
# define this one internal function I like. others can be sourced if wanted.

# remove trailing space(s) at end of line **** PRETTY MUCH ALWAYS WANT THIS

lint_trailing_spaces <- function(x) {
  
  x <- gsub(" *$", "", x)
  
  return(x)
}
################################################### #

## could do something like this but the styler addin in RStudio already does this kind of thing

lint_current_doc <- function(mylintfunction = lint_trailing_spaces) {
  ## to just edit the currently open/active source doc in RStudio:
  idx = rstudioapi::documentId(allowConsole = FALSE)
  txt = rstudioapi::getSourceEditorContext(id = idx)$contents
  txt <- mylintfunction(txt)
  rstudioapi::setDocumentContents(txt, id = idx)
}
################################################### #

if (FALSE) {
  
## SCRIPT TO USE DRAFT LINTING FUNCTION(S) TO ACTUALLY EDIT ALL .R FILES:
#
# Use a github diff to see what effects it has, confirming each change is ok

  lint_current_doc()
  
  lint_current_doc()
  
rfiles <- file.path("./R", list.files("./R", pattern = "\\.R$"))

for (f1 in rfiles) {
  
  cat('linting file: ', f1, '\n')
  
  x <- readLines(f1)
  
  x <- lint_trailing_spaces(x)
  
  #                         x <- lint_misc(x) # or others
  
  writeLines(x, f1)
}


}
################################################### #

#  draft functions but they overlap the lintr pkg functions:

if (FALSE) {
  
  # lintr pkg commas_linter()   -- is like some of the misc code drafted below !
  lint_misc <- function(x) {
    
    ################# #  but the commas_linter  could flag things like these 
    x = gsub("big.mark = ','", "222specialsavethis222", x)
    x = gsub('big.mark = ","', '222specialsavethis222', x)
    
    # ADD space after comma if not already there  *** ALMOST ALWAYS OK
    x <- gsub(",([^ ])", ", \\1", x)
    
    # remove Extra spaces after comma (BUT this is sometimes used to make code look readable)
    x <- gsub(",  *", ", ", x)
    
    # remove all space(s) before comma (BUT this is sometimes used to make code look readable)
    x <- gsub(" *,", ",", x)
    
    x = gsub("222specialsavethis222", "big.mark = ','", x)
    x = gsub('222specialsavethis222', 'big.mark = ","', x)
    ################# #
    
    # remove space(s) before close parentheses?? but indenting gets messed up and would need to do ctrl-I
    #  x <- gsub(" *)", ")", x)
    ## and the spaces_inside_linter  can flag those
    
    # remove space(s) after open parentheses  (BUT this is sometimes used to make code look readable)
    x <- gsub("\\( *", "(", x)
    
    # ADD space before and after single = sign, if not already there (BUT this is sometimes?? used to make code look readable)
    x <- lint_equals(x)
    
    # use  <-  not  =  for assignment (this has many false positives due to multi-line lists of parameters in functions)
    ### x <- gsub("^([^(]*)=", "\1<-", x)
  }
  ################################################### #
  
  # lintr pkg infix_spaces_linter()  -- is like the lint_equals() function drafted here ! and also flags "1/3" vs "1 / 3", etc.
  lint_equals <- function(x) {
    
    x = gsub(":=", "999temporarysavethis999", x) # avoid messing with this special data.table function
    x = gsub("!=", "111temporarysavethis111", x) # avoid messing with this special function
    
    cat('see lintr package and see the RStudio add-in that can be assigned e.g., ctrl-option-L  \n')
    x <- gsub("([^ |=])==", "\\1 ==", x)
    x <- gsub("==([^ |=])", "== \\1", x)
    #
    x <- gsub("([^ |=])=", "\\1 =", x)
    x <- gsub("=([^ |=])", "= \\1", x)
    
    x = gsub("999temporarysavethis999", ":=", x)
    x = gsub("111temporarysavethis111", "!=", x)
    
    return(x)
    
    #   ### example / test:
    #
    #   testjunk <-
    #     "
    #
    # #  **lorem blah blah *
    # #
    # # x= 1
    # # x =1
    # #
    # blah <- function(x) {
    #
    #   x=sdf  ;   x=sdf
    #
    #   x=s; y=f
    #
    #   x =4; x =4,
    #
    #   y= 5 ;   y= 7
    #
    #   y==5 ;  y==5
    #
    #   y ==5 ;  y ==5
    #
    #   y== 5  ; y== 5
    #
    #   y == 5 ;  y == 5
    #
    # # = == = ==  ==2= =2== ==2== =2=
    #
    #   #   |==================================================|
    #   "
    #
    #   cat(testjunk, '\n')
    #
    #   cat(lint_equals(testjunk), '\n')
    #
  }
  ################################################### #
}

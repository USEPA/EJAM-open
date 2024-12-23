
lint_misc <- function(x) {
  
  # add space after comma if not already there
  x <- gsub(",([^ ])", ", \1", x)
  
  # remove extra spaces after comma
  x <- gsub(",  ", ", ", x)
  
  # remove space before comma
  x <- gsub(" ,", ",", x)
  
  # remove space before close parentheses
  x <- gsub(" )", ")", x)
  
  # add space before and after single = sign, if not already there
  x <- lint_equals(x)
  
  # remove trailing space at end of line
  x <- gsub(" $", "", x)
  
  # use  <-  not  =  for assignment (this has many false positives due to multi-line lists of parameters in functions)
  ### x <- gsub("^([^(]*)=", "\1<-", x)
}
################################################################### #


lint_equals <- function(x) {
  cat('see lintr package and see the RStudio add-in that can be assigned e.g., ctrl-option-L  \n')
  x <- gsub("([^ |=])==", "\\1 ==", x)
  x <- gsub("==([^ |=])", "== \\1", x)
  # 
  x <- gsub("([^ |=])=", "\\1 =", x)
  x <- gsub("=([^ |=])", "= \\1", x)
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
################################################################### #

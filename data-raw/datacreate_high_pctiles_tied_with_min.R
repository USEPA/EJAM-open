
#   create high_pctiles_tied_with_min

datacreate_high_pctiles_tied_with_min <- function(usastats, statestats) {
  
  # if reported pctile per lookup function is <= these high_pctiles_tied_with_min, 
  # then report instead zero as the percentile.
  
  ######################################## #
  
  # by state ####
  
  states <- unique(statestats$REGION)
  x <- data.table::copy(statestats)
  data.table::setDT(x)
  
  datacols <- setdiff(names(x), c('PCTILE', 'REGION'))
  
  high_pctiles_tied_with_min <- list(rep(0, length(states)))
  
  for (i in 1:length(states)) {
    
    # for the list of columns, 
    y <- x[REGION == states[i], lapply(.SD, FUN = function(x) {
      max(as.numeric(PCTILE[x == min(x, na.rm = T)]), na.rm = T)
    } ), .SDcols = datacols]
    y[y == 100] <- 0 # when all the values are tied, such as when data is just missing, report 0 percentile always, not 100th percentile.
    high_pctiles_tied_with_min[[i]] <- y
  }
  
  ######################################## #
  
  # for USA overall ####
  
  x <- data.table::copy(usastats)
  setDT(x)
  datacols <- setdiff(names(x), c('PCTILE', 'REGION'))
  i <- length(states) + 1
  y <- x[ , lapply(.SD, FUN = function(x) {
    max(as.numeric(PCTILE[x == min(x, na.rm = T)]), na.rm = T)
  } ), .SDcols = datacols]
  y[y == 100] <- 0 # when all the values are tied, such as when data is just missing, report 0 percentile always, not 100th percentile.
  high_pctiles_tied_with_min[[i]] <- y
  
  names(high_pctiles_tied_with_min) <- c(states, "USA")
  return(high_pctiles_tied_with_min)
}
##################################################################################### #

# use function ####

high_pctiles_tied_with_min <- datacreate_high_pctiles_tied_with_min(usastats, statestats)

# metadata and use_data ####

high_pctiles_tied_with_min <- metadata_add(high_pctiles_tied_with_min)
usethis::use_data(high_pctiles_tied_with_min, overwrite = TRUE)

dataset_documenter("high_pctiles_tied_with_min",
                   "high_pctiles_tied_with_min (DATA) internal data used to handle cases where multiple places are tied for the lowest indicator score")

cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())
##################################################################################### #

# to examine results


if (1 == 0) {
  {
    # to spot check these results in states: 
    
    myvar = sample(datacols, 1) #  'pctlingiso'
    mystate = sample(states, 1) # 'DE'
    
    p <- high_pctiles_tied_with_min[[mystate]][ , ..myvar]
    p <- unlist(p)
    # (this will not show all rows where pctile was set to 0 because all the pctiles are zero for that indicator)
    print(
      statestats[statestats$REGION == mystate, c('REGION', 'PCTILE', myvar)][1:(p + 2) , ]
    )
    cat("highest percentile tied with minimum value for ", myvar, " in ", mystate, " is ", p,'\n')
  }
  
  
  {
    # to spot check these results in USA:  
    
    myvar = sample(datacols, 1) #  'pctnhaiana' 
    mystate = "USA"
    p <- high_pctiles_tied_with_min[[mystate]][ , ..myvar]
    p <- unlist(p) 
    print(
      usastats[ , c('REGION', 'PCTILE', myvar)][1:(p + 2) , ]
    )
    cat("highest percentile tied with minimum value for ", myvar, " in ", mystate, " is ", p,'\n')
  }
  ###############################################
  
  # To see what variables in what states or USA have a bunch of tied values at minimum that is NOT zero, and then ties at zero:
  
  # STATES
  
  datacols <- setdiff(names(statestats), c('PCTILE', 'REGION')); states <- unique(statestats$REGION)
  for (myvar in datacols) {
    # cat(myvar,":   -----------------------------------------\n")
    for (mystate in states) {
      
      if (mystate == states[1] & myvar == datacols[1]) {cat("\n\n what states or USA have a bunch of tied values at minimum that is NOT zero, and then ties at zero? \n\n")}
      z = statestats[mystate == statestats$REGION, myvar]
      if (testing & mystate == "PR" & myvar == 'lowlifex') { cat('lowlifex in PR ? \n')}
      if (length(z) > 1 & !all(is.na(z))) {
        if ((z[1] == z[2]) & (z[1] != 0)) {cat("in ", mystate, " for ", myvar, " = ", z[1], '\n')}
      }}
    # cat("\n")
  }
  datacols <- setdiff(names(statestats), c('PCTILE', 'REGION')); states <- unique(statestats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == statestats$REGION, myvar]
  if ((z[1] == z[2]) & (z[1] == 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
  
  # USA
  
  datacols <- setdiff(names(usastats),  c('PCTILE', 'REGION')); states <- unique(usastats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == usastats$REGION, myvar]
  if ((z[1] == z[2]) & (z[1] != 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
  
  datacols <- setdiff(names(usastats),  c('PCTILE', 'REGION')); states <- unique(usastats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == usastats$REGION, myvar]
  if ((z[1] == z[2]) & (z[1] == 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
}
##################################################################################### #

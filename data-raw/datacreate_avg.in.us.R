
datacreate_avg.in.us <- function(usastats = NULL, 
                                 longlist = unique(c(
                                   names_e,
                                   names_d, 
                                   names_d_subgroups_nh, names_d_subgroups_alone, 
                                   names_d_language, 
                                   names_d_extra
                                 )
                                 )
) {
  
  if (is.null(usastats)) {stop("requires usastats be passed here explicitly, to be sure the right (updated) version is used")}
  miss = setdiff(longlist, names(usastats)) 
  if (length(miss) > 0)  {
    cat("\n    Variable names not found in usastats:\n\n")
    print(miss)
    warning("some of longlist of indicator names are not found among colnames(usastats) -- 1st blockgroupstats must be updated, then usastats must be updated, then avg.in.us can be updated.")
  }
  longlist = longlist[longlist %in% names(usastats)]
  # a data.frame of 1 row per variable:
  # print(
  #   usastats_means(varnames = longlist, dig = 6)
  # )
  
  # a data.frame of 1 col per variable, and easier to view in RStudio console
  avg.in.us <-   usastats[ usastats$PCTILE == "mean",  longlist ]
  return(avg.in.us)
}

# use function ####
avg.in.us <- datacreate_avg.in.us(usastats = usastats,  # must be the updated/new usastats
                                  longlist = unique(c(  # must use the updated/new indicator variable names
                                    names_e, 
                                    names_d, names_d_subgroups_nh, names_d_subgroups_alone
                                  ))
)

# metadata and use_data ####

avg.in.us <- metadata_add(avg.in.us)
usethis::use_data(avg.in.us, overwrite = TRUE)
# documentation ####
dataset_documenter('avg.in.us', 
                   title = "avg.in.us (DATA) national averages of key indicators, for convenience",
                   description = "also available via [usastats] and created by /data-raw/datacreate_avg.in.us")

cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())


################################################################################ #

if (1 == 0) {

  print(
    setdiff(names_these, names(usastats))
  )
  
  #  unique(union(names_these, c(names_d_subgroups_alone, names_d_subgroups_nh))) %in% names(usastats)
  #  unique(union(names_these, c(names_d_subgroups_alone, names_d_subgroups_nh))) %in% names(statestats)
  
  longlist <- unique(c(names_e, names_d, names_d_subgroups_nh, names_d_subgroups_alone))
  
  # a data.frame of 1 row per variable:
  print(
    usastats_means(varnames = longlist, dig = 6)
  )
  
  # a data.frame of 1 col per variable, and easier to view in RStudio console
  avg.in.us <-   usastats[ usastats$PCTILE == "mean",  longlist ]
  
  print(
    setdiff(names_these, names(avg.in.us))
  )
  
  ## if we wanted raw EJ index averages, but nobody ever looks at raw EJ, only at percentile, so ratio of raw/avgraw is not so useful to people
  # ejrawnames <- c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state)
  # manynames  <- c(names_these, ejrawnames)
  # avg.in.us  <-   usastats[ usastats$PCTILE == "mean", intersect(manynames,   names( usastats))]
  
  
  # note the regular name not avg. name is used in the usastats table
  
  ################################################################################ #
  
  #  ALSO COULD USE A FUNCTION TO LOOK UP THE MEANS WHEN NEEDED. 
  # 
  # avg.in.us                # This is a data.frame, 1 row, where colnames are indicators
  # avg.in.us[names_e]          # subset is a data.frame!
  # unlist(avg.in.us[names_e])  # to make it a vector
  # 
  # usastats_means()        # This is a matrix, with 1 col, and indicator names are rownames 
  # usastats_means(names_e)     # subset is a matrix        and indicator names are rownames
  # usastats_means()[names_e, ] # subset is a named vector  and indicator names are  names
  # 
  # help(usastats_query)
}

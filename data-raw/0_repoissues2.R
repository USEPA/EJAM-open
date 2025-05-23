

#' download issues list from a github repo - not just open ones, using a different approach and format
#' this is a rough draft. also see repoissues()
#' @details
#' repoissues() collects less info, in a different format (tibble where some cells are pipe-separated lists),
#'   for only open issues.
#' 
#' repoissues2() collects a lot more info, in a different format (data.frame where some cells are R lists),
#'   for open + closed issues.
#' 
#' repname = "OWNERNAME/REPONAME" # e.g., "USEPA/EJAM" # or "USEPA/EJAM-open" # or EJAM:::repo_from_desc()
#' 
#' x1 = repoissues(repname, savedetails = T, savebasics = T, folder = "~/../Downloads")
#' 
#' x2 = repoissues2(repname, savedetails = T, savebasics = T, folder = "~/../Downloads")
#' 
#' @param repo_name owner/reponame
#' @param savedetails  if TRUE, will save issues_details tibble as .rda (BUT NOT AS .csv) with all info provided by githubr pkg function get_issues(), instead of just returning it invisibly
#' @param savebasics if TRUE, will save tibble of issues (just the key info) as .rda (BUT NOT AS .csv), instead of just returning it invisibly
#' @param folder folder without filename, for .rda files to be saved in
#' @param how_many assume no more than this many exist and stop if you hit that cap
#' @param git_pat NULL means try to use the one already set up
#' @param verbose how much to print
#'
#' @returns invisibly returns named list of 2 tibbles, issues_details (huge) and issues (fewer columns)
#'   where issues_details$labels is something like a list of lists of data.frames, for example.
#' 
#' @export
#' @keywords internal
#'
repoissues2 <- function(repo_name = EJAM:::repo_from_desc(), savedetails = FALSE, savebasics = FALSE, folder = ".", how_many = 1000, git_pat = NULL, verbose = TRUE) {
  
  library(dplyr)
  library(magrittr)
  library(lubridate)
  library(httr) # https://httr.r-lib.org/reference/content.html
  library(jsonlite)
  library(tibble)
  
  if (verbose) {message(paste("Checking for remote git repository:", repo_name))}
  auth_arg <- githubr::get_git_auth(git_pat = git_pat, quiet = !verbose)
  
  git_pat <- try(auth_arg$password, silent = TRUE)
  issues = NULL
  if (!grepl("Error", git_pat[1])) {
    repo_exists <- githubr::check_git_repo(repo_name, git_pat)
    if (repo_exists) {
      
      if (how_many == "all") {how_many <- 1000} # just get up to the first 1000 ? assume no more? what if only 150??
      npages <- ceiling(how_many / 100) # how_many %/% 100 + ifelse(how_many %% 100 > 0, 1, 0)
      
      pages = list()
      cumcount = 0
      ## loop over   batches of 100
      for (i in 1:npages) {
        
        my_issues = GET(paste0('https://api.github.com/repos/', repo_name, '/issues'), 
                        query = list(state = 'all', per_page = 100, page = i), 
                        authenticate(auth_arg$username, git_pat))
        jsondata <- content(my_issues, type = "text")
        github_df <- fromJSON(jsondata, flatten = TRUE)
        pages[[i]] <- github_df
        cumcount = cumcount + NROW(pages[[i]])
        cat("So far got ", cumcount, " rows (issues) \n")
        if (NROW(pages[[i]]) == 100 && i == npages) {
          # last batch is 100, so must have been more than assumed for how_many
          warning("probably there were more that were not fetched ! need a larger value than default for how_many")
        }
        if (NROW(pages[[i]]) < 100 && i < npages) {
          # this batch was not full 100 so abort since must be done and npages was an overestimate
          break() # stops the loop
        }
      }
      cat("\n\n")
      ### assemble pages here 
      issues <- bind_rows(pages)
      # clean it up
      issues <- as_tibble(issues)
      
      issues_details <- issues %>% 
        rename(pr_key = pull_request.url, 
               reported_by = user.login, 
               developer = assignee.login, 
               release = milestone.title)
      
      
      issues <- issues_details %>%
        select(-ends_with("url"), -starts_with("user"), 
               -starts_with("assignee"), -starts_with("milestone")
               ) %>%
        mutate(created_at = ymd_hms(created_at), 
               closed_at = ymd_hms(closed_at))
      
      ## these tibbles contain elements that are lists, too, so cannot write to csv
      ## and works better to view as tibble not convert to data.frame
      # issues_details <- as.data.frame(issues_details)
      # issues <- as.data.frame(issues)
    }
    else {
      stop("That repo doesn't exist or you don't have the credentials to see it.")
    }
  }
  
  when <- Sys.Date()
  repo_name_only <- gsub(".*/", "", repo_name)
    
  if (savedetails) {
    cat("\n")
    fname <- paste0("r2_", when, "_", repo_name_only, "_issues_details.rda")
    fname <- file.path(folder, fname)
    save(issues_details, file = fname)
    cat('saved issues_details as a tibble in', normalizePath(fname), '\n')
    
    ## these tibbles contain elements that are lists, too, so cannot write to csv
    ## unless you reformatted or dropped the list type columns:
    # i2 = issues_details[, !(names(issues_details) %in% c('assignees', 'labels'))]
    i2 = issues_details
    i2$labels <- sapply(i2$labels, function(z) paste0(as.vector(z$name), collapse = "|"))
    i2$assignees <- sapply(i2$assignees, function(z) paste0(as.vector(z$id), collapse = "|"))
    fname <- paste0("r2_", when,  "_", repo_name_only, "_issues_details.csv")
    fname <- file.path(folder, fname)
    write.csv(i2, row.names = FALSE, file = fname)
    cat('saved issues_details in', normalizePath(fname), '\n')
    cat("Dropped some info aboutassignees and reformatted labels info that was stored as lists of data.frames\n")
    cat("\n")
  }
  
  if (savebasics) {
    cat("\n")
    fname <- paste0("r2_", when, "_", repo_name_only,  "_issues.rda")
    fname <- file.path(folder, fname)
    save(issues, file = fname)
    cat('saved basic info about issues as a tibble in', normalizePath(fname), '\n')
    
    i2 = issues
    i2$labels <- sapply(i2$labels, function(z) paste0(as.vector(z$name), collapse = "|"))
    # i2$assignees <- sapply(i2$assignees, function(z) paste0(as.vector(z$id), collapse = "|"))
    fname <- paste0("r2_", when,  "_", repo_name_only, "_issues.csv")
    fname <- file.path(folder, fname)
    write.csv(i2, row.names = FALSE, file = fname)
    cat('saved issues in', normalizePath(fname), '\n')
    cat("Reformatted labels info that was stored as lists of data.frames\n")
    cat("\n")
    
  }
  
  
  invisible(list(issues = issues, issues_details = issues_details))
}

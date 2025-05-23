# a way to save open issues in a data.frame - source this whole file
# install/load the githubr pkg and gh pkg
# but we are avoiding making EJAM depend on the githubr or gh package so this is not in the /R/ folder


#' download/save issues list from a github repo
#' this is a rough draft. also see repoissues2()
#' @details
#' repoissues() collects less info, in a different format (data.frames where some cells are pipe-separated lists),
#'   for only open issues.
#' 
#' repoissues2() collects a lot more info, in a different format (tibbles where some cells are R lists),
#'   for open + closed issues.
#'   
#' repname = "OWNERNAME/REPONAME" # e.g., "USEPA/EJAM" # or "USEPA/EJAM-open"
#'   # as specified in DESCRIPTION file, for example
#'   
#' x1 = repoissues(repname, savedetails = T, savebasics = T, folder = "~/../Downloads")
#' 
#' x2 = repoissues2(repname, savedetails = T, savebasics = T, folder = "~/../Downloads")
#' 
#' @param repo owner/reponame
#' 
#' @param savedetails  if TRUE, will save issues_details list as .rda (BUT NOT AS .csv) with all info provided by githubr pkg function get_issues(), instead of just returning it invisibly
#' @param savebasics if TRUE, will save issues (just the key info) data.frame as .rda and .csv, instead of just returning it invisibly
#' @param folder folder without filename, for .rda or .csv files to be saved in
#' @param printhigh set FALSE if you want to print most recent ones. TRUE means only if "high" found within labels
#' @param print_data_structure_details Set to TRUE if you want to see lots of details about how each field is parsed from the lists obtained via get_issues()
#' 
#' @return invisibly returns data.frames of issues and issues_details
#'
#' @export
#' @keywords internal
#'
#'
repoissues <- function(repo = EJAM:::repo_from_desc(), savedetails = FALSE, savebasics = FALSE, folder = ".", printhigh = TRUE, print_data_structure_details = FALSE) {
  

  # check for necessary packages here without using a way that might make the pkg appear to depend on them.
  # Using code here to attach via library or require or the :: notation might make this package seem to depend on it which it should not have to since this is an optional helper.
  # A stop() happens here even if the pkg is installed and loaded but not yet attached,
  # because loadedNamespaces() check if loaded not necessarily attached. Also, requireNamespace() unlike require() does not attach a namespace; it just loads to the search path using loadNamespace(),
  if (!("githubr" %in% loadedNamespaces()) || !exists('get_issues')) {
    stop("for the get_issues() function to work in repoissues(), you have to install and then manually load the 'githubr' package using require() or library()")
  }
  if (!("gh" %in% loadedNamespaces()) || !exists('gh')) {
    stop("for the gh() function to work in repoissues(), you have to install and then manually load the 'gh' package using require() or library()")
  }
  cat('Please wait... this can take a minute...\n\n')
  issues_details <- issues <-  get_issues(repo)
  
  # show info about each field returned by get issues
  if (print_data_structure_details) {
    x = printfieldinfo(repo = repo, issues_details = issues_details)
  }
  ############################################## #
  
  # start saving the info in a simpler format and just key info
  
  # > names(issues)
  #
  # [1] "url"                      "repository_url"           "labels_url"               "comments_url"            
  # [5] "events_url"               "html_url"              "id"                       "node_id"                 
  # [9] "number"                   "title"                    "user"                     "labels"                  
  # [13] "state"                    "locked"                   "assignee"                 "assignees"               
  # [17] "milestone"                "comments"                 "created_at"               "updated_at"              
  # [21] "closed_at"                "author_association"       "sub_issues_summary"       "active_lock_reason"      
  # [25] "draft"                    "pull_request"      comments_txt?       "body"                     "closed_by"               
  # [29] "reactions"                "timeline_url"             "performed_via_github_app" "state_reason"       

  
  # url                      "character"
  # repository_url           "character"
  # labels_url               "character"
  #. comments_url             "character"
  # events_url               "character"
  # html_url                 "character"
  #. id                       "numeric"  
  # node_id                  "character"
  #. number                   "integer"  
  #. title                    "character"
  
  #. user                     "list"     xxxxxxx    EXAMPLE of 1:
    # c(
    #   "login", "id", "node_id", "avatar_url", "gravatar_id", "url", 
    #   "html_url", "followers_url", "following_url", "gists_url", "starred_url", 
    #   "subscriptions_url", "organizations_url", "repos_url", "events_url", 
    #   "received_events_url", "type", "user_view_type", "site_admin"
    # )
        #. login               "ejanalysis"                                                    
        # id                  8205979                                                         
        # node_id             "MDQ6VXNlcjgyMDU5Nzk="                                          
        # avatar_url          "https://avatars.githubusercontent.com/u/8205979?v=4"           
        # gravatar_id         ""                                                              
        # url                 "https://api.github.com/users/ejanalysis"                       
        # html_url            "https://github.com/ejanalysis"                                 
        # followers_url       "https://api.github.com/users/ejanalysis/followers"             
        # following_url       "https://api.github.com/users/ejanalysis/following{/other_user}"
        # gists_url           "https://api.github.com/users/ejanalysis/gists{/gist_id}"       
        # starred_url         "https://api.github.com/users/ejanalysis/starred{/owner}{/repo}"
        # subscriptions_url   "https://api.github.com/users/ejanalysis/subscriptions"         
        # organizations_url   "https://api.github.com/users/ejanalysis/orgs"                  
        # repos_url           "https://api.github.com/users/ejanalysis/repos"                 
        # events_url          "https://api.github.com/users/ejanalysis/events{/privacy}"      
        # received_events_url "https://api.github.com/users/ejanalysis/received_events"       
        # type                "User"                                                          
        # user_view_type      "public"                                                        
        # site_admin          FALSE      
  
  # labels                   "list"     xxxxxxx    EXAMPLE of 1:
    #  c("id", "node_id", "url", "name", "color", "default", "description"
        #. id          4538805065                                          
        # node_id     "LA_kwDOIAVeUs8AAAABDoirSQ"                         
        # url         "https://api.github.com/repos/OWNERNAME/REPONAME/labels/bug"
        #. name        "bug"                                               
        #. color       "d73a4a"                                            
        # default     TRUE                                                
        #. description "Something isn't working" 
  
  #. state                    "character".
  # locked                   "logical"  
  #? assignee                 "NULL"    
  
  # assignees                "list"     xxxxxxx   EXAMPLE of 1:
        # login               "USERNAME-HERE"                                                    
        #?1? id                  3285721                                                             
        # node_id             "MDQ6VXNlcjMyODU3MjE="                                              
        # avatar_url          "https://avatars.githubusercontent.com/u/3285721?v=4"               
        # gravatar_id         ""                                                                  
        # url                 "https://api.github.com/users/USERNAME-HERE"                       
        # html_url            "https://github.com/USERNAME-HERE"                                 
        # followers_url       "https://api.github.com/users/USERNAME-HERE/followers"             
        # following_url       "https://api.github.com/users/USERNAME-HERE/following{/other_user}"
        # gists_url           "https://api.github.com/users/USERNAME-HERE/gists{/gist_id}"       
        # starred_url         "https://api.github.com/users/USERNAME-HERE/starred{/owner}{/repo}"
        # subscriptions_url   "https://api.github.com/users/USERNAME-HERE/subscriptions"         
        # organizations_url   "https://api.github.com/users/USERNAME-HERE/orgs"                  
        # repos_url           "https://api.github.com/users/USERNAME-HERE/repos"                 
        # events_url          "https://api.github.com/users/USERNAME-HERE/events{/privacy}"      
        # received_events_url "https://api.github.com/users/USERNAME-HERE/received_events"       
        # type                "User"                                                              
        # user_view_type      "public"                                                            
        # site_admin          FALSE      
      
  # milestone                "NULL"     
  #. comments                 "integer"  
  #. created_at               "character"
  #. updated_at               "character"
  # closed_at                "NULL"     
  # author_association       "character"
  
  # sub_issues_summary       "list"     xxxxxxx   EXAMPLE of 1:
        # total             0   
        # completed         0   
        # percent_completed 0   
  
  # active_lock_reason       "NULL"     
  #. body                     "character"
  # closed_by                "NULL"   
  
  # reactions                "list"     xxxxxxx   EXAMPLE of 1:
        # url         "https://api.github.com/repos/OWNERNAME/REPONAME/issues/715/reactions"
        # total_count 0                                                             
        # +1          0                                                             
        # -1          0                                                             
        # laugh       0                                                             
        # hooray      0                                                             
        # confused    0                                                             
        # heart       0                                                             
        # rocket      0                                                             
        # eyes        0
  
  # timeline_url             "character"
  # performed_via_github_app "NULL"     
  # state_reason             "NULL"  
  
  
  assignee1 = rep(NA, length(issues$id))
  for (i in 1:length(issues$id)) {
    if (0 < length(issues$assignees[[i]])) {
      assignee1[i] =  (issues$assignees[[i]][[1]]$login ) 
    } else {
      assignee1[i] =  ("none")
    } 
  }
  
  body = rep("", length(issues$id))
  # this would work but saves reactions url when there is no actual body text:
  # body[1 == sapply(sapply(issues$body, function(x) x[[1]]), length)] <- unlist(sapply(issues$body, function(x) x[[1]]))
  #  this works and usually saves empty char when no actual body text:
  for (i in 1:length(issues$id)) {
    if (class(issues_details$body[[i]]) == 'character') {
      body[i] <- issues_details$body[[i]]
    } else {
      NA
    }
  }
  
  issues <- data.frame(
    
    rowid =   1:length(issues_details$id),
    number = sapply(issues_details$number, function(x) x[[1]]),
    state  = sapply(issues_details$state,  function(x) x[[1]]), # only gets open ones 
    title  = sapply(issues_details$title,  function(x) x[[1]]),
    
    ############################ #
    # labels_as_list = issues_details$labels, # a list that is a bit complicated
    
    labels_name = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$name), collapse = "|")
    }),
    labels_id = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$id), collapse = "|")
    }),
    labels_color = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$color), collapse = "|")
    }),
    labels_description  = sapply(seq_along(issues_details$labels), function(x) {
      paste0(sapply(issues_details$labels[x][[1]], function(y) y$description), collapse = "|")
    })
    ############################ #
    
  )  
  
  comments_txt = rep("", length(issues_details$id))
  for (i in 1:length(issues_details$id)) {
    issuenum = unlist(issues_details$number)[i]
    
    # VERY SLOW WAY TO DOWNLOAD ALL COMMENTS FOR ONE ISSUE AT A TIME, LOOPED OVER ISSUES:
    
    x = gh(paste0("https://api.github.com/repos/", repo, "/issues/", issuenum, "/comments") )
    
    comments_txt[i] <- paste0(sapply(x, function(z) z$body), collapse = "|")
    
  }
  
  
  reform = function(z) {
    sapply(z, function(x) {
      if (length(x[[1]]) > 0 ) {
        
        paste0(sapply(x[[1]], function(z) x[[1]]  ), collapse = " | ") 
        
        # x[[1]]
        
      } else {
        ""
      }
    })
  }
  
  more = data.frame( 
    
    commentcount = reform(issues_details$comments),
    body      = body,
    comments_txt = comments_txt,
    draft = reform(issues_details$draft),
    comments_url = reform(issues_details$comments_url),
    
    created_at     = reform(issues_details$created_at),
    updated_at     = reform(issues_details$updated_at),
    # closed_at      = reform(issues_details$closed_at), # never
    
    user = sapply(issues_details$user, function(x) x[['login']]),
    assignee1 = assignee1
  )
  more$created_at = substr(more$created_at, 1, 10)
  more$updated_at = substr(more$updated_at, 1, 10)
  
  
  issues = data.frame(issues, more)
  
  issues <- issues[order(issues$number, decreasing = TRUE),]
  
  when <- Sys.Date()
  cat("\n\n----------------------------------------------------------------------------------------------\n\n")
  
  if (savedetails) {
    cat("\n")
    fname <- paste0("r1_", when, "_issues_details.rda")
    fname <- file.path(folder, fname)
    save(issues_details, file = fname)
    cat('saved detailed info about open issues as a list in', normalizePath(fname), '\n')
    
    # cannot save a csv of details list
  }
  
  if (savebasics) {
    cat("\n")
    fname <- paste0("r1_", when, "_issues.rda")
    fname <- file.path(folder, fname)
    save(issues, file = fname)
    cat('saved basic info about open issues as a data.frame in', normalizePath(fname), '\n')
    
    fname <- paste0("r1_", when, "_issues.csv")
    fname <- file.path(folder, fname)
    write.csv(issues, row.names = FALSE, file = fname)
    cat('saved basic info about open issues as a data.frame in', normalizePath(fname), '\n')
    cat("\n")
  }
  
  # print out some of the table
  recentones <- issues[ , c( 'number' ,'state' ,'title', 'commentcount', 'updated_at', 'labels_name')]
  recentones <- recentones[order(recentones$updated_at, decreasing = TRUE), ]
  
  if (printhigh) {
    cat("\n Examples of high urgency issues most recently updated:\n\n")
    recentones <- recentones[grepl("high", recentones$labels_name, ignore.case = T), 
                        c('number' ,'state' ,'title', 'commentcount', 'updated_at')]
  } else {
    cat("\n Examples of issues most recently updated:\n\n")
    recentones <- recentones[, c('number' ,'state' ,'title', 'commentcount', 'updated_at')]
    recentones <- head(recentones, 10)
  }
  rownames(recentones) <- NULL
  print(recentones)
  
  invisible(list(issues = issues, issues_details = issues_details))
}
################################################ #



############################################## #

## helper function to just look at the structure of data on issues:

print1field = function(fieldname, issues_details, egn = 29) {
  
  fields = names(issues_details)
  i = which(fields == fieldname)
  
  cat('\n---------------------------------- ', i,'\n')
  cat(fields[i], '   ')
  cat(paste0('(', class(issues_details[[fields[i]]]), 'of', 
             length(issues_details[[fields[i]]]), 'issues) \n'))
  
  islist =   ('list' %in% unique(sapply(issues_details[[fields[i]]], class)))
  if (islist) {
    cat(fields[i], 'info can be a list for each issue: \n\n')
    
    if (length(issues_details[[fields[i]]]) < egn) {egn <- 1}
    
    if ((length(issues_details[[fields[i]]]) > 1 ) &&
        'list' %in% class( (issues_details[[fields[i]]][[egn  ]])[[1]]) ) {
      cat(' and 1 issue can have multiple', fields[i],' so the issue has a list of lists, \n e.g., 1 issue has multiple labels but each label has a few fields like name, description, color, etc. \n')
      
      x = do.call(cbind, issues_details[[fields[i]]][[egn]] )
      
      ## see example for just 1  issue, multiple assignees or whatever 
      colnames(x) <- paste0(
        paste0('issue_', issues_details[[fields[which(fields  %in% 'number')]]][[egn  ]]),
        '_', fields[i], '_', 1:NCOL(x)
      )
      # return all?
      y <- issues_details[[fields[i]]]
      
    } else {
      
      x = cbind(issues_details[[fields[i]]][[egn  ]])
      ## see example for just 1 issue
      if (NCOL(x) == 1) { # should be the case
        colnames(x) =  paste0('issue_', issues_details[[fields[which(fields  %in% 'number')]]][[egn  ]])
      }
      # return all even though printed only some
      y = cbind(issues_details[[fields[i]]])
      # if (NCOL(x) == 1) { # should be the case
      #   colnames(x) =  paste0('issue_', issues_details[[fields[which(fields  %in% 'number')]]] )
      # } 
    }
    
    print(x)
    x <- y
    
  } else {
    
    cat("1st 2 issues: \n")
    x = data.frame(unlist( issues_details[[fields[i]]]  ))
    if (NCOL(x) == 1) {
      colnames(x) = fields[i]
    }
    print(head(x, 2))
  }
  return(x)
  
}
################################################ #

# helper function to print structure info 

printfieldinfo = function(repo, issues_details, fieldnames = NULL) {
  
  # check for necessary packages here without using a way that might make the pkg appear to depend on them.
  # Using code here to attach via library or require or the :: notation might make this package seem to depend on it which it should not have to since this is an optional helper.
  # A stop() happens here even if the pkg is installed and loaded but not yet attached,
  # because loadedNamespaces() check if loaded not necessarily attached. Also, requireNamespace() unlike require() does not attach a namespace; it just loads to the search path using loadNamespace(),
  if (!("githubr" %in% loadedNamespaces()) || !exists('get_issues')) {
    stop("for the get_issues() function to work in repoissues(), you have to install and then manually load the 'githubr' package using require() or library()")
  }
  if (!("gh" %in% loadedNamespaces()) || !exists('gh')) {
    stop("for the gh() function to work in repoissues(), you have to install and then manually load the 'gh' package using require() or library()")
  }
  
  if (missing(issues_details)) {
    if (!missing(repo)) {
      issues_details <-  get_issues(repo)
    } else {
      stop('must specify repo or provide output of get_issues()')
    }
  } else {
    if (!missing(repo)) {
      message(paste0('ignoring repo=',repo,' and using issues_details'))
    }
  }
  
  if (is.null(fieldnames) | missing(fieldnames)) {
    fieldnames = names(issues_details)
  } else {
    if (!all(fieldnames %in% names(issues_details))) {stop('all fieldnames must be in names(issues_details')}
  }
  
  out = list()
  for (fieldname in (fieldnames)) {
    n = which(fieldnames == fieldname)
    out[[n]] <- print1field(fieldname = fieldname, issues_details = issues_details, egn = 29)
  }
  
  names(out) <- fieldnames
  return(out)
  
  cat("\n\n")
}
################################################ #

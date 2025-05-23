
# helper to get the owner and name of the code repo and the documentation repo as specified in the DESCRIPTION file

#' @noRd
repo_from_desc <- function(domain = c("github.com", "github.io")[1], get_full_url = FALSE) {
  both_urls <- desc::desc(file = system.file("DESCRIPTION", package = "EJAM"))$get("URL")
  both_urls <- as.vector(unlist(strsplit(gsub(" |\n", "", both_urls), ",")))
  one_url <- grep(domain, both_urls, value = T)
  if (get_full_url) {
    return(one_url)
  }
  
  owner_slash_repo_or_just_docs_repo <- gsub(
    paste0("(.*", domain, "/)(.*)"), "\\2", one_url)
  return(owner_slash_repo_or_just_docs_repo)
}
# repo_from_desc('github.com') # for code repo, returns something like "ownername/reponame"
# repo_from_desc('github.io')  # for documentation site, returns something like "reponame"

# owner_repo <- repo_from_desc()      # like "OWNERNAME/REPONAME"
# repo <- gsub(".*/", "", owner_repo)



# comma <- function(x) format(x, digits = 2, big.mark = ",")


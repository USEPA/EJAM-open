################################################################################################## #
# Note the package here is called 'EJAM' even though a repo can be called something other than 'EJAM'
# and still contain/offer an installable copy of a branch/version/release of the EJAM pkg.

# The public repo 'USEPA/ejscreen-multisite-open' main branch is a copy of
# the internal repo 'USEPA/EJAM' branch called 'ejscreen-multisite-open'
# and they are similar to the USEPA/EJAM branch called 'PUBLIC-EJSCREEN' but cleaned up somewhat. 

# The public repo 'USEPA/EJAM-open' main branch would be a copy of 
# the internal repo 'USEPA/EJAM' branch called 'EJAM-open' 
# and they are similar to the USEPA/EJAM branch called 'main' but cleaned up somewhat.

################################################################################################## #


#       SCRIPT USED TO REDEPLOY SHINY APP AFTER UPDATES/EDITS
#
# 1) Source the steps below (to install the pkg from github, then update the manifest file) and then
# 2) commit changes (updated manifest file) to git repo and then
# 3) either manually update deployed app via posit connect server management page, 
# or just wait until it periodically updates by itself.

## To deploy, 1st reinstall package to this machine, from specified repo and branch (needed for manifest)
#
## You might be able to do that now simply via something like this:
# xyz.tar.gz <- "https://github.com/USEPA/ejscreen-multisite-open/archive/refs/tags/v2.32-public.tar.gz" # for the public app. 
# install.packages(xyz.tar.gz)  # probably does NOT require PAT set up/ authentication if simply downloading source bundle from a public repo.
## or else do it like this:

devtools::install_github(
  # repo = 'USEPA/ejscreen-multisite-open', ref = 'main', # install the        public app from the public repo - requires a PAT; but install.packages() is easier
    repo = 'USEPA/EJAM-open',               ref = 'main', # install full pkg/internal app from the public repo - requires a PAT; but install.packages() is easier
  # repo = 'USEPA/EJAM', ref = 'PUBLIC-EJSCREEN', # install the public   app from the internal repo - requires access,token,authentication
  #repo = 'USEPA/EJAM', ref = 'deploy-posit',  # install the internal app from the internal repo - requires access,token,authentication
  
  build_vignettes = FALSE,
  build_manual = FALSE,
  dependencies = TRUE, # to ensure it checks for the packages in Suggests not just Imports
  build = FALSE,
  upgrade = "never"
)

## get list of files found in EJAM root directory
all_files <- rsconnect::listDeploymentFiles(getwd())

## exclude certain subfolders from being searched for dependencies
deploy_files <- all_files[-c(grep('dev/',          all_files),
                             grep('docs/',         all_files),
                             grep('data-raw/',     all_files),
                             grep('.github/',      all_files),
                             grep('tests/',        all_files),
                             grep('vignettes/',    all_files),
                             grep('.arrow',        all_files),
                             grep('.gitignore',    all_files),
                             grep('.Rhistory',     all_files),
                             grep('.Rbuildignore', all_files),
                             grep('EJAM.rproj',    all_files)
)
]

print(deploy_files)

## check dependency list
x <- rsconnect::appDependencies(appFiles = deploy_files) # roughly 220 

print(dim(x))

## update manifest.json file
rsconnect::writeManifest(appFiles = deploy_files)

cat("Now commit that updated manifest file, push to github, 
    and if server is set to deploy from that repo and branch it will detect the changes and redeploy \n")

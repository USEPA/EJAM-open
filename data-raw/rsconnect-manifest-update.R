################################################################################################## #
# Note the package here is called 'EJAM' even though a repo can be called something other than 'EJAM'
# and still contain/offer an installable copy of a branch/version/release of the EJAM pkg.

################################################################################################## #

#       SCRIPT USED TO REDEPLOY SHINY APP AFTER UPDATES/EDITS
#
# 1) Source the steps below (to install the pkg from github, then update the manifest file) and then
# 2) commit changes (updated manifest file) to git repo and then
# 3) either manually update deployed app via posit connect server management page,
# or just wait until it periodically updates by itself.

############################################ #

## 1st reinstall package to this machine, from specified repo and branch (needed for manifest)

## You might be able to do that now simply via something like this:

# install_url()  # as explained at paste0(repo_from_desc('github.io', get_full_url=T), "/articles/1_installing.html")

# or

devtools::install_github(

  ###  uncomment one of these:
   repo = EJAM:::repo_from_desc(), ref = 'main',
  # repo = 'USEPA/EJAM-open',       ref = 'main',  # install full pkg/internal app from the public repo - requires a PAT; but install.packages() is easier
  #####       but note that the isPublic=TRUE option should be checked before deploying (may be a default value in run_app() within file app.R, or be set as param passed)

  build_vignettes = FALSE,
  build_manual = FALSE,
  dependencies = TRUE, # to ensure it checks for the packages in Suggests not just Imports
  build = FALSE,
  upgrade = "never"
)
############################################ #

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


# Note: compare frsprogramcodes, epa_programs, epa_programs_defined, etc.

## DOWNLOAD DEFINITIONS OF ACRONYMS OF EPA PROGRAMS IN FRS (REMOVING ANY NONPUBLIC ONES)

epa_programs_defined_download_update = function(urlbase = "https://www.epa.gov/sites/default/files/2021-05/",
                                                webname = "frs_program_abbreviations_and_names.xlsx") {
## Definitions list is linked from
# browseURL("https://www.epa.gov/frs/frs-data-sources")

## For frequency of update and if updated via API/etc. see
# browseURL("https://frs-public.epa.gov/ords/frs_public2/frs_html_public_pages.frs_refresh_stats")

if (basename(getwd()) != "EJAM") {stop('must be in source package root folder')}
localdir = "./data-raw"


localname = webname
webpath = paste0(urlbase, webname)
print(webpath)
localpath = file.path(localdir, localname)

## download MANUALLY AT THIS POINT saved in EJAM/data-raw/


## tried download ---------------------------------------
# download.file(webpath, localpath)
if (!file.exists(localpath)) {stop("failed to download from", webpath, "to", localpath)}
# opening after download.file() FAILS even though it seems to download and a manual download makes it work.
## ---------------------------------------------------


frsinfo = openxlsx::read.xlsx(localpath, 1)
if (!is.data.frame(frsinfo)) {stop("failed to read xlsx from", localpath)}

# colnames(frsinfo)
# c('PGM_SYS_ACRNM',	'PGM_SYS_NAME',	'PGM_SYS_DESC', "Comments")

# DROP INTERNAL-ONLY ONES

epa_programs_defined <- frsinfo[frsinfo$Comments != "This data set is available only on the EPA intranet.", ]
epa_programs_defined$Comments <- NULL

return(epa_programs_defined)

}
#################################### # 


epa_programs_defined <- epa_programs_defined_download_update()
rm(epa_programs_defined_download_update) # cleanup 

epa_programs_defined <- metadata_add(epa_programs_defined)
usethis::use_data(epa_programs_defined, overwrite = TRUE)

dataset_documenter(varname = "epa_programs_defined", 
                   title = "Full names and definitions for acronyms of EPA programs in Facility Registry Services (FRS)", 
                   seealso = "[epa_programs]")

cat("Done updating and documenting the dataset 'epa_programs_defined' \n")

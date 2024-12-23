

#' Save spatial data.frame as shapefile.zip
#'
#' @param shp a spatial data.frame as from [shapefile_from_any()] or [sf::st_read()]
#' @param file full path to and name of the .zip file to create
#'
#' @return normalized path of the cleaned up file param (path and name of .zip)
#' @examples
#' # shp <- shapes_from_fips(fips = name2fips(c('tucson,az', 'tempe, AZ')))
#' shp <- testshapes_2
#' \dontrun{
#' fname <- file.path(tempdir(), "myfile.zip")
#' fpath <- shape2zip(shp, fname)
#' file.exists(fpath)
#' zip::zip_list(fpath)
#' # read it back in
#' shp2 <- shapefile_from_any(fpath)
#' }
#' 
#' @export
#'
shape2zip <- function(shp, file = "shapefile.zip") {
  
  folder = dirname(file)
  fname = basename(file)
  fname_noext <- gsub( paste0("\\.", tools::file_ext(fname), "$"), "", fname) 
  if (tools::file_ext(fname_noext) == "shp") {
    warning("file name cannot end in x.shp.zip for example, so just x.zip will be used")
    fname_noext <- gsub(".shp$", "", fname_noext)
  } 
  fname.zip = paste0(fname_noext, '.zip')
  fname.shp = paste0(fname_noext, '.shp')
  fnames.all = paste0(fname_noext, c(".shp", '.dbf', '.prj', '.shx'))
  
  tds = tempdir()
  for (fil in file.path(tds, fnames.all)) {
    if (file.exists(fil)) {file.remove(fil)}
  }
  # if (file.exists(file.path(tds, fname.shp))) {file.remove(file.path(tds, fname.shp))}
  sf::st_write(
    obj = shp,
    dsn = file.path(tds, fname = fname.shp),
    append = FALSE, delete_layer = TRUE
  )
  ## fname_noext_found <- gsub( paste0("\\.", tools::file_ext(fname), "$"), "", dir(tds, pattern = fname))  # this would be the version found
  ## fnames <- dir(tds, pattern = fname_noext_found)
  # fnames <- dir(tds, pattern = fname_noext)
  # fnames <- fnames[!grepl("zip$", fnames)]
  fnames <- fnames.all
  
  suppressWarnings({
    zipfullpath <- normalizePath(paste0(normalizePath(folder), "/", fname.zip))
    if (file.exists(zipfullpath)) {file.remove(zipfullpath)}
  })
  zip(zipfullpath, files = file.path(tds, fnames), extras = c('-j', '-D'))
  # unzip from tempdir to folder specified by parameter.
  # -D should prevent storing Directory info, 
  # -j is supposed to use no path info so files are all in root of .zip and there are not folders inside the .zip
  
  zipfullpath <- gsub('\\\\', '/', zipfullpath) # R format using / not \\, so user could copy paste what results from cat(zipfullpath) 
  if (!file.exists(zipfullpath)) {warning('failed to create file at ', zipfullpath)} else {cat('saved', zipfullpath, '\n')}
  return(zipfullpath)
}
############################################################# #


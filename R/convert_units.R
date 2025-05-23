#' @title Convert units of distance or area
#'
#' @description `convert_units` converts distance or area from specified units to other specified units.
#' 
#' @details
#' This function takes a number, or vector of numbers, representing distance/length or area
#' in one type of specified units, such as miles, and returns the corresponding number(s)
#' converted to some other units, such as kilometers. Units can be specified in various ways.
#' All inputs must be in the same units. All outputs must be in a single set of units as well.
#' 
#' NOTE: For some purposes, Census Bureau does this: 
#' 
#' "The ANSI standard for converting square kilometers into square miles was used 
#' ( 1 square mile = 2.58998811 square kilometers)." 
#' see <https://www.census.gov/geo/reference/state-area.html>
#' but the conversions in this function use 2.5899881034 not 2.58998811 sqkm/sqmi. 
#' The difference is only 6.6 per billion (roughly 1 in 152 million), which is less than one tenth of a square kilometer out the entire USA.
#'
#' @param x A number or vector of numbers to be converted.
#' @param from A string specifying original units of input parameter. Default is 'km' which is kilometers.
#'   Note all must be in the same units.
#'   Units can be specified as any of the following: 
#'   c(
#'   'millimeter', 'millimeters', 'centimeter', 'centimeters', 'meter', 'meters', 'kilometer', 'kilometers',
#'   "mm", "cm", "m", "km",
#'   "sqmm", "sqcm", "sqm", "sqkm", 
#'   "mm2", "cm2", "m2", "km2", 
#'   'inch', 'inches', 'foot', 'feet', 'yard', 'yards', 'mile', 'miles',
#'   "in", "ft", "yd", "mi", 
#'   "sqin", "sqft", "sqyd", "sqmi",
#'   "in2", 'ft2', 'yd2', 'mi2'
#'   )
#'   Note that m2 is for square meters not square miles.
#' @param towhat A string specifying new units to convert to. Default is 'mi' which is miles.
#' @return Returns a number or vector of numbers then length of the input x, 
#'   with each element corresponding to an input element converted to new units.
#' @examples
#' convert_units(1, 'mi', 'km')
#' convert_units(c(1e6, 1), 'sqm', 'sqkm')
#' 
#' @export
#' 
convert_units <- function(x, from='km', towhat='mi') {

  mult <- structure(list(
    from = c("mm", "cm", "m", "km", "sqmm", "sqcm", "sqm", "sqkm", "in", "ft", "yd", "mi", "sqin", "sqft", "sqyd", "sqmi"), 
    mm = c(1, 10, 1000, 1e+06, NA, NA, NA, NA, 25.4, 304.8, 914.4, 1609344, NA, NA, NA, NA), 
    cm = c(0.1, 1, 100, 1e+05, NA, NA, NA, NA, 2.54, 30.48, 91.44, 160934.4, NA, NA, NA, NA), 
    m = c(0.001, 0.01, 1, 1000, NA, NA, NA, NA, 0.0254, 0.3048, 0.9144, 1609.344, NA, NA, NA, NA), 
    km = c(1e-06, 1e-05, 0.001, 1, NA, NA, NA, NA, 2.54e-05, 0.0003048, 0.0009144, 1.609344, NA, NA, NA, NA), 
    sqmm = c(NA, NA, NA, NA, 1, 100, 1e+06, 1e+12, NA, NA, NA, NA, 645.16, 92903.04, 836127.36, 2.589988e+12), 
    sqcm = c(NA, NA, NA, NA, 0.01, 1, 10000, 1e+10, NA, NA, NA, NA, 6.4516, 929.0304, 8361.2736, 25899881103), 
    sqm = c(NA, NA, NA, NA, 1e-06, 1e-04, 1, 1e+06, NA, NA, NA, NA, 0.00064516, 0.09290304, 0.83612736, 2589988.11034), 
    sqkm = c(NA, NA, NA, NA, 1e-12, 1e-10, 1e-06, 1, NA, NA, NA, NA, 6.4516e-10, 9.290304e-08, 8.3612736e-07, 2.5899881034), 
    `in` = c(0.0393700787402, 0.393700787402, 39.3700787402, 39370.0787402, NA, NA, NA, NA, 1, 12, 36, 63360, NA, NA, NA, NA), 
    ft = c(0.00328083989501, 0.0328083989501, 3.28083989501, 3280.83989501, NA, NA, NA, NA, 0.083333333, 1, 3, 5280, NA, NA, NA, NA), 
    yd = c(0.00109361329834, 0.0109361329834, 1.09361329834, 1093.61329834, NA, NA, NA, NA, 0.027777778, 0.333333333, 1, 1760, NA, NA, NA, NA), 
    mi = c(6.21371192237, 6.21371192237e-06, 0.000621371192237, 0.621371192237, NA, NA, NA, NA, 1.57828e-05, 0.000189394, 0.000568182, 1, NA, NA, NA, NA), 
    sqin = c(NA, NA, NA, NA, 0.0015500031, 0.15500031, 1550.0031, 1550003100, NA, NA, NA, NA, 1, NA, NA, NA), 
    sqft = c(NA, NA, NA, NA, 1.07639104167e-05, 0.00107639104167, 10.7639104167, 10763910.4167, NA, NA, NA, NA, NA, 1, NA, NA), 
    sqyd = c(NA, NA, NA, NA, 1.1959852573e-06, 0.00011959852573, 1.1959852573, 1195985.2573, NA, NA, NA, NA, NA, NA, 1, NA), 
    sqmi = c(NA, NA, NA, NA, 3.86102158542e-13, 3.861021585e-11, 3.86102158542e-07, 0.386102158542, NA, NA, NA, NA, NA, NA, NA, 1)), 
    .Names = c("from", "mm", "cm", "m", "km", "sqmm", "sqcm", "sqm", "sqkm", "in", "ft", "yd", "mi", "sqin", "sqft", "sqyd", "sqmi"), 
    row.names = c(NA, 16L), class = "data.frame"
  )
  ###################################################################### # 
  from   <- fixnames_aliases(from,   na_if_no_match = TRUE, ignore.case = TRUE)
  towhat <- fixnames_aliases(towhat, na_if_no_match = TRUE, ignore.case = TRUE)
  ###################################################################### # 
# ## older way
  
#   if (!(from %in% mult$from)) {
#     if (from %in% c('km2', 'kilometer2','kilometers2', 'sq kilometers', 'sq kilometer','sqkilometers', 'sqkilometer',  'squarekilometers', 'squarekilometer', 'square kilometers', 'square kilometer')) {from <- 'sqkm'}
#     if (from %in% c('m2', 'meter2','meters2', 'sq meters', 'sq meter','sqmeters', 'sqmeter',  'squaremeters', 'squaremeter', 'square meters', 'square meter'))  {from <- 'sqm'}
#     if (from %in% c('cm2'))  {from <- 'sqcm'}
#     if (from %in% c('mm2'))  {from <- 'sqmm'}
#     
#     if (from %in% c('kilometer', 'kilometers')) {from <- 'km'}
#     if (from %in% c('meter', 'meters')) {from <- 'm'}
#     if (from %in% c('centimeter', 'centimeters')) {from <- 'cm'}
#     if (from %in% c('millimeter', 'millimeters')) {from <- 'mm'}
#     
#     if (from %in% c('mi2', 'mile2','miles2', 'sq miles', 'sq mile','sqmiles', 'sqmile',  'squaremiles', 'squaremile', 'square miles', 'square mile')) {from <- 'sqmi'}
#     if (from %in% c('yd2')) {from <- 'sqyd'}
#     if (from %in% c('ft2')) {from <- 'sqft'}
#     if (from %in% c('in2')) {from <- 'sqin'}
#     
#     if (from %in% c('mile', 'miles')) {from <- 'mi'}
#     if (from %in% c('yard', 'yards')) {from <- 'yd'}
#     if (from %in% c('foot', 'feet')) {from <- 'ft'}
#     if (from %in% c('inch', 'inches')) {from <- 'in'}
#   }
# 
#   if (!(towhat %in% names(mult))) {
#     if (towhat %in% c('km2', 'kilometer2','kilometers2', 'sq kilometers', 'sq kilometer','sqkilometers', 'sqkilometer',  'squarekilometers', 'squarekilometer', 'square kilometers', 'square kilometer')) {towhat <- 'sqkm'}
#     if (towhat %in% c('m2', 'meter2','meters2', 'sq meters', 'sq meter','sqmeters', 'sqmeter',  'squaremeters', 'squaremeter', 'square meters', 'square meter'))  {towhat <- 'sqm'}
#     if (towhat %in% c('cm2'))  {towhat <- 'sqcm'}
#     if (towhat %in% c('mm2'))  {towhat <- 'sqmm'}
#     
#     if (towhat %in% c('kilometer', 'kilometers')) {towhat <- 'km'}
#     if (towhat %in% c('meter', 'meters')) {towhat <- 'm'}
#     if (towhat %in% c('centimeter', 'centimeters')) {towhat <- 'cm'}
#     if (towhat %in% c('millimeter', 'millimeters')) {towhat <- 'mm'}
#     
#     if (towhat %in% c('mi2', 'mile2','miles2', 'sq miles', 'sq mile','sqmiles', 'sqmile',  'squaremiles', 'squaremile', 'square miles', 'square mile')) {towhat <- 'sqmi'}
#     if (towhat %in% c('yd2')) {towhat <- 'sqyd'}
#     if (towhat %in% c('ft2')) {towhat <- 'sqft'}
#     if (towhat %in% c('in2')) {towhat <- 'sqin'}
#     
#     if (towhat %in% c('mile', 'miles')) {towhat <- 'mi'}
#     if (towhat %in% c('yard', 'yards')) {towhat <- 'yd'}
#     if (towhat %in% c('foot', 'feet')) {towhat <- 'ft'}
#     if (towhat %in% c('inch', 'inches')) {towhat <- 'in'}
#   }
  ###################################################################### # 
  if (!(from %in% mult$from))     {stop(paste(from, 'not found'))}
  if (!(towhat %in% names(mult))) {stop(paste(towhat, 'not found'))}
  
  return( x * mult[ match(from, mult$from), towhat] )
}

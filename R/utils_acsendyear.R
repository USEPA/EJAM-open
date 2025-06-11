
#' helper - try to report the end year of the ACS 5-year survey as reported by current EJAM metadata for the package,
#'   or try to infer the possible year
#'
#' @param guess_as_of optional alternative data to use if guessing what is available
#'  as of this date, e.g., "2025-01-01" -- must be Date class like Sys.Date()
#' @param guess_always optional, set TRUE to ignore metadata and just guess at year.
#'
#' @returns a single year like "2022" as character string, meaning ACS5 for 2018-2022
#'
#' @keywords internal
#'
acsendyear <- function(guess_as_of = Sys.Date(), guess_always = FALSE) {

  ## tests
  # acsendyear()
  # acsendyear(guess_always = TRUE)
  # acsendyear(Sys.Date(), guess_always = TRUE)
  #
  # acsendyear(as.Date("2021-01-01"))
  # acsendyear(as.Date("2021-01-01"), guess_always = TRUE)
  #
  # acsendyear("2021-01-01")
  # acsendyear("2021-01-01", guess_always = TRUE)
  #
  # acsendyear("2021")
  # acsendyear("2021", guess_always = TRUE)
  #
  # acsendyear(as.Date("2025-11-30"), guess_always = TRUE)
  # acsendyear(as.Date("2025-12-31"), guess_always = TRUE)
  #
  # acsendyear(as.Date("2026-01-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-02-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-03-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-04-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-05-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-06-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-07-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-08-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-09-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-10-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-11-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-12-01"), guess_always = TRUE)
  # acsendyear(as.Date("2026-12-31"), guess_always = TRUE)
  # acsendyear(as.Date("2027-01-01"), guess_always = TRUE)
  #
  #
  # acsendyear(as.Date("2099-01-01"), guess_always = TRUE)

  if (!guess_always) {
    # if !guess_always, try metadata approach first, and then only if metadata approach fails,
    #     try to use guess_as_of
    #
    #  try use year that was in metadata that should record/report the years of the ACS data currently being used.
    yr <- as.vector(gsub("^....-", "", get_metadata_mapping()$acs_version)) # unexported func in EJAM
    if (length(yr) == 0 || is.null(yr)) {
      warning("cannot find metadata that reports the year being used, so trying to guess at what years might be available in the package vs from census")
    }
  }

  if (length(yr) == 0 || is.null(yr) || guess_always) {

    # if guess_always, do not try metadata approach at all, just
    #     try to use guess_as_of

    ## try to use guess_as_of

    if (is.character(guess_as_of)) {
      # try to interpret just text date or just year
      guess_as_of <- try({as.Date(guess_as_of)}, silent = TRUE)
      if (inherits(guess_as_of, "try-error")) {
        if (length(guess_as_of) && nchar(guess_as_of)[1] == 4 && guess_as_of[1] > 2019 && guess_as_of[1] < 2041) {
          guess_as_of <- as.Date(paste0(guess_as_of, "-01-01"))
          warning("only year was specified, so assuming you meant", as.character(guess_as_of))
        } else {
          warning('invalid guess_as_of parameter ignored')
          guess_as_of <- Sys.Date()
        }
      }
    }

    message("Guessing based on what may be the case as of", as.character(guess_as_of), "\n")

    # assume 2018-2022 becomes available at end of 2023, so in 11/2023 it reports 2021 endyr, in 12/31/2023 reports 2021 still.
    likely_already_published_yr = substr(  guess_as_of - 365 * 2, 1, 4)
    message("It is likely that ACS data has already been released for the 5-year survey period of ", likely_already_published_yr, " but not later periods")

    # Typically until 1/2025 had seen Agency release new acs data roughly 2.5 years after end of ACS end year,
    # e.g., 2018-2022 ACS was still used until at least mid-2025? but
    # may take more or less time in future depend who updates ACS dataset used here
    yr <- substr( guess_as_of - 365 * 2.6, 1, 4)
    message("It is a guess that ACS data may already be incorporated into this package for the 5-year survey period of ", yr, " but not later periods")
  }

  return(yr)
}

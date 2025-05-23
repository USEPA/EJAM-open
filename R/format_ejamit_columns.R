#' Format ejamit results for use in tables, charts, etc.
#' 
#' Applies rounding, sigfig, and percentage info to display columns of ejamit using map_headernames
#' 
#' @param df data table of ejamit results, 
#' @param nms, name(s) of columns referring to EJAM indicators, such as "Demog.Index'
#' @return a named vector with formatted values, corresponding to valid column names provided
#'
#' @examples
#'   # x <- ejamit(testpoints_10, radius = 1)
#'   x <- testoutput_ejamit_10pts_1miles
#'   format_ejamit_columns(x$results_overall, 'Demog.Index')
#'   format_ejamit_columns(x$results_overall, c('Demog.Index', 'no2'))
#'   format_ejamit_columns(x$results_overall, names_d)
#' 
#' @export
#'
format_ejamit_columns <- function(df, nms=c()) {
  
  if (missing(df)) {
    warning('Please provide a data frame for df')
    return(NULL)
  }
  
  if (!is.data.frame(df)) {
    warning('Please provide a data frame for df')
    return(NULL)
  }
  
  if (missing(nms)) {nms <- names(df)}
  
  
  if (any(!(nms %in% names(df)))) {
    # warning('Some names not found in df. Ignoring those.')
    nms <- nms[nms %in% names(df)]
  }
    nms <- nms[nms %in% map_headernames$rname]

  if (length(nms) == 0 | is.null(nms)) {
    warning('No valid names found. Please provide names of columns using the nms argument.')
    return(df)
  }
  
  if (length(nms) == 0) {return(df)}
  suppressWarnings({
  decimal_num <- as.numeric(fixcolnames(nms, 'rname','decimals'))#varinfo(nms, 'decimals')$decimals)
  sigfig_num  <- as.numeric(fixcolnames(nms, 'rname','sigfigs')) #varinfo(nms, 'sigfigs')$sigfigs) # some are NA values which creates a warning while as.numeric is coercing to NA
  })
  is.percentage <- as.logical(fixcolnames(nms, 'rname','pct_as_fraction_ejamit')) 
  is.percentage[is.na(is.percentage)] <- FALSE

  for (i in seq_along(nms)) {
    colname <- nms[i]
    cur_value <- df[[colname]]
    
    if (!is.numeric(cur_value)) {
      # warning(paste("Skipping non numeric:", colname))
      next
    }
    
    if (is.percentage[i] == TRUE) {
      if (is.na(decimal_num[i])) {
        df[[colname]] <-  
          scales::label_percent(
            scale = 100, accuracy = 1)(cur_value)
      } else {
        df[[colname]] <- scales::label_percent(
          scale = 100, accuracy = 1 / (10^decimal_num[i]))(cur_value)
      }
      
    } else {
      if (!is.na(decimal_num[i])) {
        df[[colname]] <- round(cur_value, digits = decimal_num[i]) 
      }
      if (!is.na(sigfig_num[i])) {
        df[[colname]] <- signif(df[[colname]], digits = sigfig_num[i])
      }
      df[[colname]] <- scales::label_comma(
        accuracy = ifelse(is.na(decimal_num[i]), 1,
                          1/(10^decimal_num[i])))(df[[colname]])
    }
  }
  
  df[is.na(df)] <- "N/A"
  return(df)
  
  #names(formatted_values) <- nms
  #return(formatted_values)
  #return(data.frame(raw = raw_values, fmt = formatted_values))
  #return(df)
  #df_display[, nms] <- formatted_values
  #return(df_display)
}

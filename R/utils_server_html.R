build_invalid_alert <- function(invalid_count, method='upload'){

  if (invalid_count > 0) {
    if(method == 'upload'){
      end_txt <- ' invalid location(s) in your dataset.'
    } else if (method == 'dropdown'){
      end_txt <- ' selected sites without associated lat/lon information.'
    }

    HTML(
      paste0(
        '<section class="usa-site-alert usa-site-alert--emergency usa-site-alert--slim" aria-label="Site alert">
  <div class="usa-alert">
    <div class="usa-alert__body">
      <p class="usa-alert__text">
        <strong>', ' Warning! ','</strong>', 'There are ', prettyNum(invalid_count, big.mark = ","), end_txt,
        '</p>
    </div>
  </div>
</section>'))

  } else {
    HTML(NULL)
  }
}

#############################################################################  #

# possibly obsolete/ unused ***

build_summary_title <- function(analysis_title, upload_method, radius, nsites, pop){
  ## paste header information together
  title_text <- paste0('<div style="font-weight: bold; font-size: 11pt; text-align: center;">',
                       analysis_title, '<br>')

  ## exclude radius info from header text when using FIPS
  if (upload_method != 'FIPS') {
    title_text <- paste0(title_text, 'Residents within ', radius, ' miles of any of the ')
  }

  title_text <- paste0(title_text,
                       prettyNum(nsites, big.mark = ","),
                       ' sites analyzed<br>',
                       #    "in the xxx source category or sector<br>",
                       'Population: ', prettyNum(pop, big.mark = ","), '</div>'
  )
  return(HTML(title_text))
}
#############################################################################  #

## use build_sitecount_text function
## update invalid_alert count in each upload place?

build_sitecount_text <- function(placetype, data_upload){

  num_locs <- NROW(data_upload)

  if(placetype == 'FIPS'){
    #num_na <- 0 # we do not keep track of invalid FIPS uploaded

    msg <- HTML(paste0(
      "<span style='border: 1px solid #005ea2; padding: 10px;'>Total location(s) uploaded by FIPS: <strong>",
      prettyNum(num_locs, big.mark = ","),"</strong></span>"
    ))

  } else if(placetype == 'SHP'){

    msg <- HTML(paste0(
      "<span style='border: 1px solid #005ea2; padding: 10px;'>Total shape(s) uploaded: <strong>",
      prettyNum(num_locs, big.mark = ","),"</strong></span>"
    ))

  } else {

    msg <- HTML(paste0(
      "<span style='border: 1px solid #005ea2; padding: 10px;'>Total location(s) uploaded: <strong>", prettyNum(num_locs, big.mark = ","),"</strong></span>"
      #"<br>","Site(s) with invalid lat/lon values: <strong>", prettyNum(num_na, big.mark = ","), "</strong>","</span>"
    ))
  }

  return(msg)

}

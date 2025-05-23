
# unexported helper

calc_ratios_to_avg_for_ejscreenapi <- function(table_as_displayed, 
                                               names_e_FOR_RATIOS   = names_e, 
                                               names_e_RATIOS       = names_e_ratio_to_avg,
                                               names_e_RATIOS_STATE = names_e_ratio_to_state_avg,
                                               names_d_FOR_RATIOS =  c(names_d, names_d_subgroups), 
                                               names_d_RATIOS =  c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
                                               names_d_RATIOS_STATE = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg)) {
  
  ## also available:  names_health, names_climate, and 2 of the names_criticalservice (pctnobroadband, pctnohealthinsurance )
  
  if (!all(paste0("ratio.to.avg.",       names_e_FOR_RATIOS) == names_e_RATIOS)) {stop("names_e and names_e_ratio_to_avg are sorted differently")}
  if (!all(paste0("ratio.to.avg.",       names_d_FOR_RATIOS) == names_d_RATIOS)) {stop("d-related names are sorted differently")}
  if (!all(paste0("ratio.to.state.avg.", names_e_FOR_RATIOS) == names_e_RATIOS_STATE)) {stop("names_e and names_e_ratio_to_state_avg are sorted differently")}
  if (!all(paste0("ratio.to.state.avg.", names_d_FOR_RATIOS) == names_d_RATIOS_STATE)) {stop("d-related names are sorted differently")}
  
  ##  ratio to US avg ------------ -
  
  # colnames of table must be rnames or be specified here ! *** THIS PRESUMES VIA DEFAULT PARAMETERS WHAT IS THE SORT ORDER OF THE VARIABLES !
  usratios <- calc_ratios_to_avg(table_as_displayed, 
                                 zone.prefix = "", 
                                 evarnames = names_e_FOR_RATIOS, 
                                 dvarnames = names_d_FOR_RATIOS ) 
  eratios <- round(usratios$ratios_e, 4)
  dratios <- round(usratios$ratios_d, 4)
  # calc_ratios_to_avg() colnames returned are same as input, not renamed to say "ratio"
  names(eratios) <- names_e_RATIOS
  names(dratios) <- names_d_RATIOS
  table_as_displayed <- cbind(table_as_displayed, dratios, eratios)
  
  ##  ratio to STATE avg ------------- -
  
  st_ratios <- calc_ratios_to_avg(table_as_displayed, 
                                  zone.prefix = "state.", 
                                  evarnames = names_e_FOR_RATIOS,
                                  dvarnames = names_d_FOR_RATIOS ) 
  st_eratios <- round(st_ratios$ratios_e, 4)
  st_dratios <- round(st_ratios$ratios_d, 4)
  # calc_ratios_to_avg() colnames returned are same as input, not renamed to say "ratio"
  names(st_eratios) <- names_e_RATIOS_STATE  # but RATIO VARIABLES MUST BE SORTED IN SAME ORDER AS BASE LIST OF E OR D VARIABLES as checked above
  names(st_dratios) <- names_d_RATIOS_STATE
  table_as_displayed <- cbind(table_as_displayed, st_dratios, st_eratios)
  return(table_as_displayed)
}

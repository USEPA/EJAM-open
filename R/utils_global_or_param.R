global_or_param = function(vname) {
  param_passed_to_run_app = get_golem_options(vname)
  ifelse(
    !is.null(param_passed_to_run_app), 
    param_passed_to_run_app, 
    get(vname))
}

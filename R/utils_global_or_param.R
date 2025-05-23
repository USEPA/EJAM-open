global_or_param = function(vname) {
  param_passed_to_run_app = golem::get_golem_options(vname)
  if(!is.null(param_passed_to_run_app)) param_passed_to_run_app else get(vname)
}

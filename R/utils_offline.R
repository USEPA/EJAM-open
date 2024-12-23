
offline = function() {
  # internet available? ####
  hasinternet = !is.null(curl::nslookup("r-project.org", error = FALSE))
  return(!hasinternet)
}
################ # 

offline_warning = function(text = 'NO INTERNET CONNECTION AVAILABLE') {
  if (offline()) {warning(text)}
}
################ # 

offline_cat = function(text = 'NO INTERNET CONNECTION AVAILABLE\n') {
  if (offline()) {cat(text)}
}
################ # 

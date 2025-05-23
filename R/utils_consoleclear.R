
consoleclear <- function() {
  
  # a utility for developers
  
  if (interactive() & rstudioapi::isAvailable()) {rstudioapi::executeCommand("consoleClear")}
}

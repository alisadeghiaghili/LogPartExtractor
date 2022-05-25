extractLogNames <- function(logsPath) { 
  return(dir(logsPath, pattern = "\\.(log|txt)"))
}
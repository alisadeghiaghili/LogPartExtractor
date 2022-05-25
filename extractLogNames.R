extractLogNames <- function() { 
  logsPath <- selectLogsFolder()
  return(dir(logsPath, pattern = "\\.(log|txt)"))
}
extractFileContent <- function(logsPath, fileNames) {
  readLines(file.path(logsPath, fileNames)) %>% 
    tolower() %>% 
    return()
}
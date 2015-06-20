
pollutants <- function(directory, pollutant, id) {
  fileName <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "")
  filePath <- paste(directory, "/", fileName, sep = "")
  df <- read.csv(filePath)
  pollutantData <- df[[pollutant]]
  pollutantData[!is.na(pollutantData)]
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  pollutantLists <- sapply(id, function(id) pollutants(directory, pollutant, id))
  pollutants <- unlist(pollutantLists)
  mean(pollutants)
}
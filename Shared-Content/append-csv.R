#' Append Data to a CSV File
#'
#' This function appends data to an existing CSV file. If the file does not exist, it creates a new CSV file with the specified data.
#'
#' @param report A data frame. The data to be appended to the CSV file. Column names should not have spaces!!
#' @param path A character string. The file path to the CSV file.
#'
#' @details
#' This function checks if the specified CSV file exists. If it does not exist, the function creates a new CSV file with the provided data. If the CSV file does exist, the function appends the provided data to the existing file.
#'
#' @return None. This function is called for its side effects, which include writing to a file.

append.csv <- function(report, path){
  if(file.exists(path)==FALSE){
    write.csv(report, file = as.character(path))
  } else if(file.exists(path)==TRUE){
    temp <- read.csv(path)[,-1]
    temp <- bind_rows(temp, report)
    write.csv(temp, file = as.character(path))
  }
}
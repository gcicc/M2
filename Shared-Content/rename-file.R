#' Rename a file in a given directory
#'
#' This function renames a file within a specified directory.
#'
#' @param path A character string specifying the directory path where the file is located.
#' @param original_filename A character string specifying the original name of the file.
#' @param new_filename A character string specifying the new name for the file.
#'
#' @return A message indicating whether the file was renamed successfully or if the original file does not exist.
#' 
#' @examples
#' \dontrun{
#' rename_file("path/to/directory", "old_name.txt", "new_name.txt")
#' }
#'
#' @export
rename_file <- function(path, original_filename, new_filename) {
  original_filepath <- file.path(path, original_filename)
  new_filepath <- file.path(path, new_filename)
  
  if (file.exists(original_filepath)) {
    file.rename(original_filepath, new_filepath)
    message("File renamed successfully from ", original_filename, " to ", new_filename)
  } else {
    message("The file ", original_filename, " does not exist in the directory ", path)
  }
}
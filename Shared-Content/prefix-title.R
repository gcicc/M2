#' Title
#'
#' @param object ggplot or gt object
#' @param prefix text to precede existing title, e.g., "Table 1.1:", "Figure 2.4:"
#'
#' @return
#' @export
#'
#' @examples
prefix.title <- function(object, prefix="Table 1.1"){
  if(any(class(object) == "gt_tbl")){
    object$`_heading`$title <- paste(prefix, object$`_heading`$title)
    return(object)}
  if(any(class(object) == "ggplot")){
    object$labels$title <- paste(prefix, object$labels$title)
    return(object)}
}


add.to.labs <- function(object= figure.1.1, x=NULL, y=NULL, title=NULL, caption=NULL, subtitle=NULL){
  if(is.null(x) == FALSE) object$labels$x <- paste0(object$labels$x, x)
  if(is.null(y) == FALSE) object$labels$y <- paste0(object$labels$y, y)
  if(is.null(title) == FALSE) object$labels$title <- paste0(object$labels$title, title)
  if(is.null(subtitle) == FALSE) object$labels$subtitle <- paste0(object$labels$subtitle, subtitle)
  if(is.null(caption) == FALSE) object$labels$caption <- paste0(object$labels$caption, caption)
  return(object)
}


report.dataset <- function(path="~/R-Projects/celiac/data/062/CDSD_Histology_data.xlsx") {
  # Load necessary library
  
  # Read the Excel file
  temp <- read_excel(path)
  
  # Prepare the metadata
  source_info <- paste("Source:", path)
  num_rows <- paste("Number of rows:", nrow(temp))
  num_cols <- paste("Number of columns:", ncol(temp))
  col_names <- paste("Column names:\n", paste(names(temp), collapse = ", "))
  
  # Return the information as a list
  return(list(Source = source_info, Rows = num_rows, Columns = num_cols, Column_Names = col_names))
}

report.df <- function(df=working.df, df.name = "working.df") {
  # Load necessary library
  
  
  # Prepare the metadata
  source_info <- paste("Source:", df.name)
  num_rows <- paste("Number of rows:", nrow(df))
  num_cols <- paste("Number of columns:", ncol(df))
  col_names <- paste("Column names:\n", paste(names(df), collapse = ", "))
  
  # Return the information as a list
  return(list(Source = source_info, Rows = num_rows, Columns = num_cols, Column_Names = col_names))
}

replace_whitespace_colon_with_dash <- function(input_string) {
  result <- gsub("[^a-zA-Z0-9]", "-", input_string)
  return(result)
}


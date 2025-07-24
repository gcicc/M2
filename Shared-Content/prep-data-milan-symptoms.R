# Extract-Transform-Load script for ingesting study-specific data and returning 
# data frame using standardized data dictionary column definitions that are ready
# for joining with similarly-transformed ETL results of other data types. See
# https://en.wikipedia.org/wiki/Extract,_transform,_load for more description of
# generalized approach.

# Symptoms output requires the following:
# Column    type      Notes
# STUDYID   enum      key field for joining
# SITE      string    key field for joining
# SUBJID    string    key field for joining
# OBSDATE   datetime  key field for joining
# VISIT     string    information only, no logic should be performed using it
# GISS      enum      data column
# (other symptom columns will be added later)    

symptoms_milan_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Milan")) {
    # Milan has no symptomatalogy, so the dataframe should just have the standard column names but no data rows.
    stdcols <- c("STUDYID","SITE","SUBJID","OBSDATE","VISIT","GISS")
    # We need to create an empty dataframe with the correct columns types
    symptoms_df <- data.frame(
      STUDYID = character(),
      VISIT = character(),
      SITE = character(),
      SUBJID = character(),
      OBSDATE = as.POSIXct(character()),
      GISS = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }
  
  if (!is.null(log_dir)) {
    write.csv(symptoms_df,path(log_dir,"0 symptoms_df_after_etl_Milan.csv"),row.names=FALSE)
  }
  return(list(symptoms_df=symptoms_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

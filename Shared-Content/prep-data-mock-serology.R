# Extract-Transform-Load script for ingesting study-specific data and returning 
# data frame using standardized data dictionary column definitions that are ready
# for joining with similarly-transformed ETL results of other data types. See
# https://en.wikipedia.org/wiki/Extract,_transform,_load for more description of
# generalized approach.

# Serology output requires the following:
# Column    type      Notes
# STUDYID   enum      key field for joining
# SITE      string    key field for joining
# SUBJID    string    key field for joining
# OBSDATE   datetime  key field for joining
# VISIT     string    information only, no logic should be performed using it
# tTG_IgA   real      data column
# DGP_IgA   real      data column
# DGP_IgG   real      data column

serology_mock_etl <- function(provenance,log_dir=NULL) {
  # Data build based on Mock Study metrics
  if (str_contains(input_dir,"Mock-Study")) {
    raw <- path(input_dir,"Mock Serology.xlsx")
    serology_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(serology_df)," rows,",ncol(serology_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # THIS IS WHERE THE POTENTIALLY COMPLEX LOGIC GOES FOR A GIVEN REAL STUDY. HERE IN THE MOCK, THE 
  # INPUT FILE IS INTENTIONALLY SIMPLIIFED TO NEED NO LOGIC, BUT OF COURSE THIS IS NOT THE CASE
  # IN GENERAL

  if (!is.null(log_dir)) {
    write.csv(serology_df,path(log_dir,"0 serology_df_after_etl_Mock.csv"),row.names=FALSE)
  }
  return(list(serology_df=serology_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

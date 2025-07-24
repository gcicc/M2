# Extract-Transform-Load script for ingesting study-specific data and returning 
# data frame using standardized data dictionary column definitions that are ready
# for joining with similarly-transformed ETL results of other data types. See
# https://en.wikipedia.org/wiki/Extract,_transform,_load for more description of
# generalized approach.

# VCE output requires the following:
# Column              type      Notes
# STUDYID             enum      key field for joining
# SITE                string    key field for joining
# SUBJID              string    key field for joining
# OBSDATE             datetime  key field for joining
# VISIT               string    information only, no logic should be performed using it
# First5Percent_Mean  real      data column
# BVA                 real      data column (note that this is the real biomarker, but
#                               historically referred to as Tertile1_Mean)
# Tertile1_Max        real      data column
# Tertile2_Mean       real      data column
# Tertile2_Max        real      data column
# Tertile3_Mean       real      data column
# Tertile3_Max        real      data column
# All_Mean            real      data column
# All_Max             real      data column
# READER              string    key field for joining. Should be two letter human or "MLV"
#                               followed by explicit version if known. Note that ML is 
#                               reserved for the hypothetical human with intiials "ML".

vce_mock_etl <- function(provenance,log_dir=NULL) {
  # Data build based on Mock Study metrics
  if (str_contains(input_dir,"Mock-Study")) {
    raw <- path(input_dir,"Mock VCE-WideIQ_Std.xlsx")
    vce_df.WideIQ_Std <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(vce_df.WideIQ_Std)," rows,",ncol(vce_df.WideIQ_Std),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # THIS IS WHERE THE POTENTIALLY COMPLEX LOGIC GOES FOR A GIVEN REAL STUDY. HERE IN THE MOCK, THE 
  # INPUT FILE IS INTENTIONALLY SIMPLIIFED TO NEED NO LOGIC, BUT OF COURSE THIS IS NOT THE CASE
  # IN GENERAL

  if (!is.null(log_dir)) {
    write.csv(vce_df.WideIQ_Std,path(log_dir,"vce_df.WideIQ_Std_after_etl_Mock.csv"),row.names=FALSE)
  }

  # since this is mock, the IQ levels are directly given directly rather than produced by logic
  raw <- path(input_dir,"Mock VCE-NarrowIQ_Std.xlsx")
  vce_df.NarrowIQ_Std <- read_excel(raw)
  provenance <- sapply(append(provenance, paste(raw,":",nrow(vce_df.NarrowIQ_Std)," rows,",ncol(vce_df.NarrowIQ_Std),"columns\n")), unique)
  if (!is.null(log_dir)) {
    write.csv(vce_df.NarrowIQ_Std,path(log_dir,"0 vce_df.NarrowIQ_Std_after_etl_Mock.csv"),row.names=FALSE)
  }

  # return both IQ levels
  return(list(vce_dfs=list(WideIQ_Std=vce_df.WideIQ_Std,NarrowIQ_Std=vce_df.NarrowIQ_Std),AMR=NULL,provenance=provenance)) # a df for scores in standard columns, all study-specific logic incorporated. No marking in phase0.
}

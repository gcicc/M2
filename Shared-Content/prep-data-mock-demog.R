# Extract-Transform-Load script for ingesting study-specific data and returning 
# data frame using standardized data dictionary column definitions that are ready
# for joining with similarly-transformed ETL results of other data types. See
# https://en.wikipedia.org/wiki/Extract,_transform,_load for more description of
# generalized approach.

# Demographics output requires the following:
# Column    type    Notes
# STUDYID   enum    key field for joining
# SITE      string  key field for joining
# SUBJID    string  key field for joining
# ARM       enum    factor column, ordered to ensure recognition of intervention
# SEX       letter  data column
# DOB       date    data column
# RACE      enum    data column
# ETHNICITY enum    data column
# ENRDATE  date     data column
# RANDDATE  date    data column (blank if not randomized)

demog_mock_etl <- function(provenance,log_dir=NULL) {
  # Data build based on Mock Study metrics
  if (str_contains(input_dir,"Mock-Study")) {
    raw <- path(input_dir,"Mock Demog.xlsx")
    demog_df <- read_excel(raw)
    # For some reason a variable called ...9 comes in, so we remove it
    demog_df <- demog_df %>% dplyr::select(STUDYID, SITE, SUBJID,  ARM, SEX, DOB, RACE, ETHNICITY, ENRDATE, RANDDATE)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(demog_df)," rows,",ncol(demog_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # THIS IS WHERE THE POTENTIALLY COMPLEX LOGIC GOES FOR A GIVEN REAL STUDY. HERE IN THE MOCK, THE 
  # INPUT FILE IS INTENTIONALLY SIMPLIIFED TO NEED NO LOGIC, BUT OF COURSE THIS IS NOT THE CASE
  # IN GENERAL

  # We need control over ARM (and other columns) as a factor else lexicographic order rules the day in the linear models
  # used by some of the analyses... to ensure that the reference level is properly ordered relative
  # to the intervention. Without this we can't be sure how to interpret signs of coefficients...
  demog_df <- demog_df %>%
    dplyr::mutate(ARM=factor(ARM, levels=c("Placebo", "Drug"))) %>%
    dplyr::mutate(SEX=factor(SEX, levels=c("F","M"))) %>%
    dplyr::mutate(RACE=factor(RACE, levels=c("WHITE","NOT REPORTED","AMERICAN INDIAN OR ALASKA NATIVE","ASIAN","MULTIPLE"))) %>%
    dplyr::mutate(ETHNICITY=factor(ETHNICITY, levels=c("NOT HISPANIC OR LATINO","HISPANIC OR LATINO"))) 
  
  if (!is.null(log_dir)) {
    write.csv(demog_df,path(log_dir,"0 demog_df_after_etl_Mock.csv"),row.names=FALSE)
  }
  return(list(demog_df=demog_df,
              visit_levels=c("V1","V2"),
              interval_levels=c("V1:V2"),
              provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

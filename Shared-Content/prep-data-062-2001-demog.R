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

demog_062_2001_etl <- function(provenance,log_dir=NULL) {
  distribution.folder <- path(input_dir, "Data_from_Karthik_01_08_2025")
  if (str_contains(input_dir,"TAK-062-2001")) {
    raw <- path(distribution.folder,"Demographics_data - unblinded.xlsx")
    demog_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(demog_df)," rows,",ncol(demog_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # the transformations here include simple renames and calculation of date of birth
  names(demog_df)[names(demog_df) == 'Study Identifier'] <- 'STUDYID'
  names(demog_df)[names(demog_df) == 'Study Site Identifier'] <- 'SITEID'
  names(demog_df)[names(demog_df) == 'Subject Identifier for the Study'] <- 'SUBJID'
  names(demog_df)[names(demog_df) == 'Actual Arm Code'] <- 'ARM'
  names(demog_df)[names(demog_df) == 'Sex'] <- 'SEX'
  names(demog_df)[names(demog_df) == 'Race'] <- 'RACE'
  names(demog_df)[names(demog_df) == 'Ethnicity'] <- 'ETHNICITY'
  names(demog_df)[names(demog_df) == 'Date of Enrollment'] <- 'ENRDATE'
  names(demog_df)[names(demog_df) == 'Date of Randomization'] <- 'RANDDATE'
  names(demog_df)[names(demog_df) == 'Strata1 Values derived from CRF'] <- 'SEROSTATUS' # celiac serologic status 
  names(demog_df)[names(demog_df) == 'Strata2 Values derived from CRF'] <- 'HISTINJURY' # histologic injury
  names(demog_df)[names(demog_df) == 'Strata3 Values derived from CRF'] <- 'PPIHIST2ANT' # PPI history and H2 antagonist use
  demog_df <- demog_df %>%
    dplyr::rename(mixedSITE=`SITEID`) %>%
    dplyr::mutate(SITE=as.character(as.numeric(mixedSITE))) %>%
    dplyr::mutate(DOB=ENRDATE-years(Age)) %>%
    dplyr::select("STUDYID",
                  "SITE",
                  "SUBJID",
                  "ARM",
                  "SEX",
                  DOB,
                  "RACE",
                  ETHNICITY,
                  ENRDATE,
                  RANDDATE,
                  SEROSTATUS,
                  HISTINJURY,
                  PPIHIST2ANT)%>%
    dplyr::mutate(SEX=factor(SEX, levels=c("F","M"))) %>%
    dplyr::mutate(RACE=factor(RACE, levels=c("WHITE","NOT REPORTED","AMERICAN INDIAN OR ALASKA NATIVE","ASIAN","MULTIPLE"))) %>%
    dplyr::mutate(ETHNICITY=factor(ETHNICITY, levels=c("NOT HISPANIC OR LATINO","HISPANIC OR LATINO"))) %>%
    dplyr::mutate(SEROSTATUS=factor(SEROSTATUS, levels=c("Normal","Elevated"))) %>%
    dplyr::mutate(HISTINJURY=factor(HISTINJURY, levels=c("Mild to Moderate","Moderate to Severe"))) %>%
    dplyr::mutate(PPIHIST2ANT=factor(PPIHIST2ANT, levels=c("No","Yes")))
  
  # We need control over ARM as a factor else lexicographic order rules the day in the linear models
  # used by some of the analyses... to ensure that the reference level is properly ordered relative
  # to the intervention. Without this we can't be sure how to interpret signs of coefficients...
# PUT THIS IN AFTER UNBLINDING
  demog_df <- demog_df %>%
    dplyr::mutate(ARM=factor(ARM, levels=c("C1PGS", "C1T600GS", "SCRNFAIL", "NOTASSGN", "NOTTREAT"))) %>% 
    dplyr::mutate(ARM = dplyr::recode(ARM, "C1PGS" = "Placebo", "C1T600GS" = "TAK-062", "SCRNFAIL" = "Screen Failure", "NOTASSGN" = "Not Assigned", "NOTTREAT" = "Not Treated")) %>%
    dplyr::mutate(ARM = factor(ARM, levels = c("Placebo", "TAK-062", "Screen Failure", "Not Assigned", "Not Treated")))
  
  if (!is.null(log_dir)) {
    write.csv(demog_df,path(log_dir,"0 demog_df_after_etl_062-2001.csv"),row.names=FALSE)
  }
  return(list(demog_df=demog_df,
              visit_levels=c("VISIT 2 - Week -4","VISIT 5 - Week 12","VISIT 6 - Week 24"),
              interval_levels=c("VISIT 2 - Week -4:VISIT 5 - Week 12","VISIT 5 - Week 12:VISIT 6 - Week 24","VISIT 2 - Week -4:VISIT 6 - Week 24"),
              provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

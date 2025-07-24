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

demog_phase0_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Phase0")) {
    raw <- path(input_dir,"new vce_old vce_old vhcd_new vhcd_Mean Marsh Score_CDSD Weekly Avg.wide1.ValueChg.bl_d15 - wo top row.xlsx")
    demog_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(demog_df)," rows,",ncol(demog_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  demog_df <- demog_df %>% 
    separate(SUBJID,into=c("SITE","SUBJID"),sep="-") %>% 
    dplyr::rename(ARM=Treatment) %>%
    dplyr::mutate(ETHNICITY=NA) 
    
  demog_df <- demog_df[demog_df$Time == "Run-in", ] %>% # the demographics file has only one row per subject
    dplyr::mutate(ENRDATE=as.Date(as.character(Date),format="%Y%m%d")+days(6)) %>%
    dplyr::mutate(RANDDATE=ENRDATE+days(1))

  # need to open another file to fill in missing fields
  raw <- path(input_dir,"Phase_0_Reanalysis_2024-07-18","takeda-marcs-phase-0-analysis-main","data",
                                              "Takeda Phase 0 Study R7 (for Motilent).xlsx")
  more_fields_df <- read_excel(raw)
  provenance <- sapply(append(provenance, paste(raw,":",nrow(more_fields_df)," rows,",ncol(more_fields_df),"columns\n")), unique)

  more_fields_df <- more_fields_df %>% 
    separate(SUBJID,into=c("SITE","SUBJID"),sep="-") %>% 
    dplyr::rename(SEX=Sex) %>%
    dplyr::mutate(RACE=(ifelse(Race=='W',"WHITE",Race))) %>%
    dplyr::select("SITE",
                  "SUBJID",
                  "Age",
                  "RACE",
                  "SEX")
  
  demog_df <- demog_df %>%
    left_join(more_fields_df, by=c("SITE", "SUBJID")) %>%
    dplyr::mutate(DOB=ENRDATE-years(Age)) %>%
    dplyr::select("STUDYID",
                  "SITE",
                  "SUBJID",
                  "ARM",
                  "SEX",
                  "DOB",
                  "RACE",
                  "ETHNICITY",
                  "ENRDATE",
                  "RANDDATE")

  # We need control over ARM (and other columns) as a factor else lexicographic order rules the day in the linear models
  # used by some of the analyses... to ensure that the reference level is properly ordered relative
  # to the intervention. Without this we can't be sure how to interpret signs of coefficients...
  demog_df <- demog_df %>%
    dplyr::mutate(ARM=factor(ARM, levels=c("Gluten 3g", "Gluten 10g"))) %>%
    dplyr::mutate(SEX=factor(SEX, levels=c("F","M"))) %>%
    dplyr::mutate(RACE=factor(RACE, levels=c("WHITE","NOT REPORTED","AMERICAN INDIAN OR ALASKA NATIVE","ASIAN","MULTIPLE"))) %>%
    dplyr::mutate(ETHNICITY=factor(ETHNICITY, levels=c("NOT HISPANIC OR LATINO","HISPANIC OR LATINO"))) 
  
  if (!is.null(log_dir)) {
    write.csv(demog_df,path(log_dir,"0 demog_df_after_etl_Phase0.csv"),row.names=FALSE)
  }
  return(list(demog_df=demog_df,
              visit_levels=c("Week -6","Week -5","Week -4","Week -3","Week -2","Run-in","Week 0","Week 1","Day 15","Week 3","Week 4","Week 5","Day 42"),
              interval_levels=c("Run-in:Day 15","Day 15:Day 42","Run-in:Day 42"),
              provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

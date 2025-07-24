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

demog_milan_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Milan")) {
    raw <- path(input_dir,"ClinicalData.xls")
    demog_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(demog_df)," rows,",ncol(demog_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  names(demog_df)[names(demog_df) == 'Age_at VCE'] <- 'Age_at_VCE' # we'll need column names without spaces in the logic below

  demog_df <- demog_df %>% 
    dplyr::mutate(STUDYID="Milan") %>%
    dplyr::mutate(SITE="OnlyOneSiteInMilanStudy") %>%
    separate(Nr_patient,into=c("SUBJID","VISIT"),sep="_") %>% 
    dplyr::mutate(DOB=as.Date(ISOdate(DoBirth, 1, 1))) %>%
    dplyr::mutate(ARM="MilanWasASingleArmStudy") %>%
    dplyr::mutate(SEX=ifelse(`SEX (F=1)`==1,"F","M")) %>%
    dplyr::mutate(RACE="WHITE") %>%
    dplyr::mutate(ETHNICITY="NOT REPORTED")
    
  demog_df <- demog_df[demog_df$VISIT == 1, ] # the demographics file has only one row per subject

  demog_df <- demog_df %>% 
    dplyr::mutate(ENRDATE=DOB+years(Age_at_VCE)) %>%
    dplyr::mutate(RANDDATE=ENRDATE)
    
  demog_df <- demog_df %>%
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
    dplyr::mutate(ARM=factor(ARM, levels=c("MilanWasASingleArmStudy"))) %>%
    dplyr::mutate(SEX=factor(SEX, levels=c("F","M"))) %>%
    dplyr::mutate(RACE=factor(RACE, levels=c("WHITE","NOT REPORTED","AMERICAN INDIAN OR ALASKA NATIVE","ASIAN","MULTIPLE"))) %>%
    dplyr::mutate(ETHNICITY=factor(ETHNICITY, levels=c("NOT HISPANIC OR LATINO","HISPANIC OR LATINO"))) 
  
  if (!is.null(log_dir)) {
    write.csv(demog_df,path(log_dir,"0 demog_df_after_etl_Milan.csv"),row.names=FALSE)
  }
  return(list(demog_df=demog_df,
              visit_levels=c("V1","V2"),
              interval_levels=c("V1:V2"),
              provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

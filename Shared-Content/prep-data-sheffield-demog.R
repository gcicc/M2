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

demog_sheffield_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Sheffield")) {
    raw <- path(input_dir,"takeda.study.4.5.20.xlsx")
    demog_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(demog_df)," rows,",ncol(demog_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }


  # Need age at VCE
  demog_df <- demog_df %>% 
    dplyr::mutate(STUDYID="Sheffield") %>%
    dplyr::mutate(SITE="OnlyOneSiteInSheffieldStudy") %>%
    dplyr::rename(SUBJID = `...1`) %>%
    dplyr::mutate(SUBJID = as.character(SUBJID)) %>%
    dplyr::mutate(DOB=dmy(dob)) %>%
    dplyr::mutate(ARM="SheffieldWasASingleArmStudy") %>%
    dplyr::mutate(SEX=ifelse((gender=="f")==1,"F","M")) %>%
    dplyr::mutate(RACE="Not REPORTED") %>%
    dplyr::mutate(ETHNICITY="NOT REPORTED") %>% 
    dplyr::mutate(VISIT = 1)
    
  demog_df <- demog_df[demog_df$VISIT == 1, ] # the demographics file has only one row per subject
  
  

  demog_df <- demog_df %>% 
    dplyr::mutate(ENRDATE=dmy(dop.capsule.endoscopy)) %>% # using the date of the VCE as the enrollment date
    dplyr::mutate(RANDDATE=dmy(dop.capsule.endoscopy)) # using the date of the VCE as the randomization date
    
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
    dplyr::mutate(ARM=factor(ARM, levels=c("SheffieldWasASingleArmStudy"))) %>%
    dplyr::mutate(SEX=factor(SEX, levels=c("F","M"))) %>%
    dplyr::mutate(RACE=factor(RACE, levels=c("WHITE","NOT REPORTED","AMERICAN INDIAN OR ALASKA NATIVE","ASIAN","MULTIPLE"))) %>%
    dplyr::mutate(ETHNICITY=factor(ETHNICITY, levels=c("NOT HISPANIC OR LATINO","HISPANIC OR LATINO", "NOT REPORTED"))) 
  
  if (!is.null(log_dir)) {
    write.csv(demog_df,path(log_dir,"0 demog_df_after_etl_Sheffield.csv"),row.names=FALSE)
  }
  return(list(demog_df=demog_df,
              visit_levels=c("V1","V2"),
              interval_levels=c("V1:V2"),
              provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

# Extract-Transform-Load script for ingesting study-specific data and returning 
# data frame using standardized data dictionary column definitions that are ready
# for joining with similarly-transformed ETL results of other data types. See
# https://en.wikipedia.org/wiki/Extract,_transform,_load for more description of
# generalized approach.

# Histology output requires the following:
# Column    type      Notes
# STUDYID   enum      key field for joining
# SITE      string    key field for joining
# SUBJID    string    key field for joining
# OBSDATE   datetime  key field for joining
# VISIT     string    information only, no logic should be performed using it
# MO        enum      data column (multi-level)
# HLA_G     enum      data column (Y/N)
# HLA_DQ    enum      data column (multi-level)
# VHCD      real      data column
# IELCOUNT  integer   data column

histology_milan_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Milan")) {
    raw <- path(input_dir,"ClinicalData.xls")
    histology_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(histology_df)," rows,",ncol(histology_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  names(histology_df)[names(histology_df) == 'Age_at VCE'] <- 'Age_at_VCE' # we'll need column names without spaces in the logic below
  names(histology_df)[names(histology_df) == 'Histology D2 (MarshOb)'] <- 'MOsansM' # we'll need column names without spaces in the logic below
  # histology_df[ , 'HLA_G'] <- NA # no HLA_G in Milan
  # histology_df[ , 'HLA_DQ'] <- NA # no HLA_DQ in Milan
  histology_df[ , 'VHCD'] <- NA # no VhCd in Milan
  histology_df[ , 'IELCOUNT'] <- NA # no IEL count in Milan

  histology_df <- histology_df %>% 
    dplyr::mutate(STUDYID="Milan") %>%
    dplyr::mutate(SITE="OnlyOneSiteInMilanStudy") %>%
    separate(Nr_patient,into=c("SUBJID","VISIT"),sep="_") %>% 
    dplyr::mutate(DOB=as.Date(ISOdate(DoBirth, 1, 1))) %>% # we use this to calcualte OBSDATE, but won't keep
    dplyr::mutate(OBSDATE=DOB+years(Age_at_VCE)) %>%
    dplyr::mutate(MO=paste0("M",MOsansM)) %>%
    dplyr::mutate(VISIT=paste0("V", as.character(VISIT)))
    
  histology_df <- histology_df %>%
    dplyr::select("STUDYID",
                  "SITE",
                  "SUBJID",
                  "OBSDATE",
                  "VISIT",
                  "MO",
                  # "HLA_G",
                  # "HLA_DQ",
                  "VHCD",
                  "IELCOUNT"
                  ) %>%
    dplyr::mutate(MO = toupper(MO)) 

  if (!is.null(log_dir)) {
    write.csv(histology_df,path(log_dir,"0 histology_df_after_etl_Milan.csv"),row.names=FALSE)
  }
  return(list(histology_df=histology_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

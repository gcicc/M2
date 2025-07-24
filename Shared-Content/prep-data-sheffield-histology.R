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

histology_sheffield_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Sheffield")) {
    raw <- path(input_dir,"AllMetrics_ML.csv")
    histology_df <- read_csv(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(histology_df)," rows,",ncol(histology_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  takeda_study <- read_excel(path(input_dir, "takeda.study.4.5.20.xlsx"))
  takeda_study <- takeda_study %>% dplyr::rename(SUBJID = `...1`) %>%
    dplyr::mutate(SUBJID = as.character(SUBJID))
  takeda_study <- takeda_study %>% dplyr::select(SUBJID, histology.at.ce.2) %>% 
    dplyr::mutate(OBSDATE = ymd(histology.at.ce.2))
  histology_df <- histology_df %>% 
    dplyr::rename(SUBJID = PatientID) %>%
    dplyr::mutate(SUBJID = gsub(SUBJID, pattern = "Patient ", replacement = "")) %>%
    dplyr::select(SUBJID) # I don't see other columns that match with rest of studies we are considering
  histology_df <- left_join(histology_df, takeda_study, by = "SUBJID")
  histology_df <- histology_df %>% dplyr::select(SUBJID, OBSDATE)
  
  histology_df$`Age_at_VCE` = as.Date(NA)
  histology_df$`MO` = as.character(NA)
  histology_df[ , 'VHCD'] <- NA # no VhCd 
  histology_df[ , 'IELCOUNT'] <- NA # no IEL count 
  histology_df$VISIT <- "Visit 1"
  histology_df$SITE <- "OnlyOneSiteInMilanStudy"
  histology_df$STUDYID <- "Sheffield"

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
    write.csv(histology_df,path(log_dir,"0 histology_df_after_etl_sheffield.csv"),row.names=FALSE)
  }
  return(list(histology_df=histology_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

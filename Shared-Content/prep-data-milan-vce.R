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

vce_milan_etl <- function(provenance,log_dir=NULL) {
  # Data build based on Milan metrics
  if (str_contains(input_dir,"Milan")) {
    raw <- path(input_dir,"AllMetrics.csv")
    AllMetrics.in <- read_csv(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(AllMetrics.in)," rows,",ncol(AllMetrics.in),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # the vce has no OBSDATE, so the best we can do is to estimate that it was the same as the histology
  hist_etl_output <- histology_milan_etl(provenance,log_dir=log_dir)

  AllMetrics.in <- AllMetrics.in %>%
    dplyr::mutate(STUDYID="Milan") %>%
    dplyr::mutate(SITE="OnlyOneSiteInMilanStudy") %>%
    dplyr::mutate(SUBJID=gsub(x=SubjectID,pattern="Subject",replacement="")) %>%
    dplyr::rename(VISIT=Visit) %>%
    dplyr::mutate(VISIT=paste0("V", as.character(VISIT))) %>%
    left_join(hist_etl_output$histology_df, by = c("STUDYID", "SITE", "SUBJID", "VISIT"))
    
  # Extract and work with MLV22 columns ----
  AllMetrics.MLV22 <- AllMetrics.in %>%
    dplyr::select(STUDYID,SITE,SUBJID,OBSDATE,VISIT,contains("_MLV22")) %>%
    dplyr::rename(
      First5Percent_Mean=First5Percent_Mean_MLV22,
      BVA=Tertile1_Mean_MLV22,
      Tertile1_Max=Tertile1_Max_MLV22,
      All_Mean=All_Mean_MLV22
    ) %>%
    dplyr::select(-contains("_MLV22")) %>%
    dplyr::mutate(Reader="MLV2.2")

  # Extract and work with LE columns ----
  AllMetrics.LE <- AllMetrics.in %>%
    dplyr::select(STUDYID,SITE,SUBJID,OBSDATE,VISIT,contains("_LE")) %>%
    dplyr::rename(
      First5Percent_Mean=First5Percent_Mean_LE,
      BVA=Tertile1_Mean_LE,
      Tertile1_Max=Tertile1_Max_LE,
      All_Mean=All_Mean_LE
    ) %>%
    dplyr::select(-contains("_LE")) %>%
    dplyr::mutate(Reader="LE") 

  vce_df <- bind_rows(AllMetrics.LE, AllMetrics.MLV22) %>% 
    dplyr::mutate(
      Tertile2_Mean=NA, # not reported in this data set
      Tertile2_Max=NA, # not reported in this data set
      Tertile3_Mean=NA, # not reported in this data set
      Tertile3_Max=NA, # not reported in this data set
      All_Max=NA # not reported in this data set
    ) %>% dplyr::rename(READER = Reader) 
                                                    
  if (!is.null(log_dir)) {
    write.csv(vce_df,path(log_dir,"0 vce_df_after_etl_Milan.csv"),row.names=FALSE)
  }

  # now return (only one IQ level for this study)
  return(list(vce_dfs=list(vce_df),AMR=NULL,provenance=provenance)) # a df for scores in standard columns, all study-specific logic incorporated. No marking in phase0.
}

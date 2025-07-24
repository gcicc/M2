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
# Addiitonal Endpoints          Additional endpoints from AllMetrics_ML.csv are included as this source is a summary file
#                               that also brings in some data from takeda.study.4.5.20.xlsx

vce_sheffield_etl <- function(provenance,log_dir=NULL) {
  # Data build based on Milan metrics
  if (str_contains(input_dir,"Sheffield")) {
    raw <- path(input_dir,"AllMetrics_ML.csv")
    AllMetrics_ML <- read_csv(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(AllMetrics_ML)," rows,",ncol(AllMetrics_ML),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  AllMetrics_ML <- read_csv(path(input_dir, "AllMetrics_ML.csv"))
  AllMetrics_ML <- AllMetrics_ML %>% dplyr::rename(SUBJID=PatientID) %>% pivot_longer(cols=3:22) %>%
    mutate(
      metric = str_extract(name, "^[^_]+_[^_]+"),
      Reader = str_extract(name, "[^_]+$")
    ) %>% dplyr::select(SUBJID, MaskedID, metric, Reader, value, everything(), -name) %>%
    dplyr::rename(READER=Reader)
  
  # the vce has no OBSDATE, But it's in here:
  takeda_study <- read_excel(path(input_dir, "takeda.study.4.5.20.xlsx"))
  takeda_study <- takeda_study %>% dplyr::rename(SUBJID = `...1`) %>%
    dplyr::mutate(SUBJID = paste0("Patient ", SUBJID))
  takeda_study <- takeda_study %>% dplyr::select(SUBJID, dop.capsule.endoscopy) %>% 
    dplyr::mutate(dop.capsule.endoscopy = dmy(dop.capsule.endoscopy)) 
  AllMetrics_ML <- AllMetrics_ML %>% left_join(takeda_study, by = "SUBJID") %>%
    dplyr::select(SUBJID, MaskedID, READER, dop.capsule.endoscopy, metric, READER, value, everything())
  AllMetrics_ML <- AllMetrics_ML %>% pivot_wider(names_from=metric, values_from=value) %>%
    dplyr::select(SUBJID,  MaskedID, READER, Tertile1_Mean , Tertile1_Max , All_Mean , First5Percent_Mean, everything())
  AllMetrics_ML <- AllMetrics_ML %>% dplyr::mutate(
    STUDYID="Sheffield",
    SITE="OnlyOneSiteInSheffieldStudy", # This is Greg's assumption - needs to be verified
    VISIT="V1",
  ) %>% 
    dplyr::rename(OBSDATE = dop.capsule.endoscopy) %>%
    dplyr::mutate(Tertile2_Mean=NA, # not reported in this data set
                  Tertile2_Max=NA, # not reported in this data set
                  Tertile3_Mean=NA, # not reported in this data set
                  Tertile3_Max=NA, # not reported in this data set
                  First5Percent_Max=NA,
                  Retest.Flag= FALSE) %>% # not reported in this data set
  dplyr::rename(BVA = Tertile1_Mean)
  ALLMetrics_ML <- AllMetrics_ML %>% dplyr::mutate(SUBJID = gsub(x=SUBJID,pattern="Patient ",replacement="")) 

  if (!is.null(log_dir)) {
    write.csv(ALLMetrics_ML,path(log_dir,"0 vce_df_after_etl_Milan.csv"),row.names=FALSE)
  }

  # now return (only one IQ level for this study)
  return(list(vce_dfs=list(ALLMetrics_ML),AMR=NULL,provenance=provenance)) # a df for scores in standard columns, all study-specific logic incorporated. No marking in phase0.
}

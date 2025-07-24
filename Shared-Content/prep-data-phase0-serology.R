# Extract-Transform-Load script for ingesting study-specific data and returning 
# data frame using standardized data dictionary column definitions that are ready
# for joining with similarly-transformed ETL results of other data types. See
# https://en.wikipedia.org/wiki/Extract,_transform,_load for more description of
# generalized approach.

# Serology output requires the following:
# Column    type      Notes
# STUDYID   enum      key field for joining
# SITE      string    key field for joining
# SUBJID    string    key field for joining
# OBSDATE   datetime  key field for joining
# VISIT     string    information only, no logic should be performed using it
# tTG_IgA   real      data column
# DGP_IgA   real      data column
# DGP_IgG   real      data column

serology_phase0_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Phase0")) {
    raw <- path(input_dir,"new vce_old vce_old vhcd_new vhcd_Mean Marsh Score_CDSD Weekly Avg.wide1.ValueChg.bl_d15 - wo top row.xlsx")
    serology_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(serology_df)," rows,",ncol(serology_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  names(serology_df)[names(serology_df) == 'Time'] <- 'VISIT' 
  names(serology_df)[names(serology_df) == 'IGATTGBL'] <- 'tTG_IgA' 
  names(serology_df)[names(serology_df) == 'IGADGPBL'] <- 'DGP_IgA' 
  names(serology_df)[names(serology_df) == 'IGGDGPBL'] <- 'DGP_IgG' 
  
  serology_df <- serology_df %>% 
    separate(SUBJID,into=c("SITE","SUBJID"),sep="-") %>% 
    dplyr::mutate(OBSDATE=as.Date(as.character(Date),format="%Y%m%d")) %>%
    dplyr::select("STUDYID",
                  "SITE",
                  "SUBJID",
                  "OBSDATE",
                  "VISIT", 
                  "tTG_IgA",
                  "DGP_IgA", 
                  "DGP_IgG") %>%
    dplyr::filter(!is.na(OBSDATE))

  if (!is.null(log_dir)) {
    write.csv(serology_df,path(log_dir,"0 serology_df_after_etl_Phase0.csv"),row.names=FALSE)
  }
  return(list(serology_df=serology_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

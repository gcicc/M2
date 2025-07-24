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

histology_phase0_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Phase0")) {
    raw <- path(input_dir,"new vce_old vce_old vhcd_new vhcd_Mean Marsh Score_CDSD Weekly Avg.wide1.ValueChg.bl_d15 - wo top row.xlsx")
    histology_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(histology_df)," rows,",ncol(histology_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  names(histology_df)[names(histology_df) == 'Time'] <- 'VISIT' 
  names(histology_df)[names(histology_df) == 'Marsh-Oberhuber (Mode)'] <- 'MO' 
  names(histology_df)[names(histology_df) == 'HLAGENO'] <- 'HLA_G' 
  names(histology_df)[names(histology_df) == 'HLAHALO'] <- 'HLA_DQ' 
  names(histology_df)[names(histology_df) == 'Value SO-vhcd'] <- 'VHCD' 
  names(histology_df)[names(histology_df) == 'Value Avg of IEL Counts'] <- 'IELCOUNT' 
  
  histology_df <- histology_df %>% 
    separate(SUBJID,into=c("SITE","SUBJID"),sep="-") %>% 
    dplyr::mutate(OBSDATE=as.Date(as.character(Date),format="%Y%m%d")) %>%
    dplyr::select("STUDYID",
                  "SITE",
                  "SUBJID",
                  "OBSDATE",
                  "VISIT", 
                  "MO",
                  "HLA_G", 
                  "HLA_DQ",
                  "VHCD", 
                  "IELCOUNT") %>%
    dplyr::filter(!is.na(OBSDATE))

  if (!is.null(log_dir)) {
    write.csv(histology_df,path(log_dir,"0 histology_df_after_etl_Phase0.csv"),row.names=FALSE)
  }
  return(list(histology_df=histology_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

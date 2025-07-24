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

histology_062_2001_etl <- function(provenance,log_dir=NULL) {
  distribution.folder <- path(input_dir, "Data_from_Karthik_01_08_2025")
  if (str_contains(input_dir,"TAK-062-2001")) {
    raw <- path(distribution.folder,"CDSD_Histology_data.xlsx")
    histology_df <- read_excel(raw)
    # View(histology_df)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(histology_df)," rows,",ncol(histology_df),"columns\n")), unique)
    etl_output <- demog_062_2001_etl(provenance=provenance,log_dir=log_dir) # we'll need this to get observed dates from visit calendar
    demog_df <- etl_output$demog_df
    # and we only want rows that are contained in both anyway
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # First let's get columns that we'll be using into standard naming and join with demog
  histology_df <- histology_df %>%
    dplyr::mutate(STUDYID="TAK-062-2001") %>%
    dplyr::rename(mixedSITE=`Study Site Identifier`) %>%
    dplyr::mutate(SITE=as.character(as.numeric(mixedSITE))) %>%
    dplyr::rename(SUBJID=`Subject Identifier for the Study`)
  
  histology_df <- left_join(histology_df, demog_df, by=c("STUDYID","SITE","SUBJID"))
  if (!is.null(log_dir)) {
    write.csv(histology_df,path(log_dir,"histology_df after join w demog.csv"), row.names=FALSE)
  }

  # now process the timepoints in accordance with the study calendar
  VHCD <- histology_df[0,]
  IEL <- histology_df[0,]
  MO <- histology_df[0,]
  qM <- histology_df[0,]

  for (visit in c("VISIT 2 - Week -4","VISIT 6 - Week 24")) {
    # Villous height to Crypt depth ratio----
    VHCD_for_visit <- histology_df %>% 
      dplyr::mutate(VISIT=visit) %>%
      dplyr::mutate(WEEKS_FROM_RAND=ifelse(str_detect(visit,"-4$"),-4,ifelse(str_detect(visit,"12$"),12,24))) %>%
      dplyr::mutate(OBSDATE=`Analysis Date`) %>%
      filter(`Parameter Category 1` == "ESOPHAGOGASTRODUODENOSCOPY", 
             `Parameter Code` == "VHCD", 
             `Visit Name` %in% ifelse(str_detect(visit,"-4$"),c("Visit 2"),c("Visit 6"))) %>%
      dplyr::rename(VHCD=`Analysis Value`) 
    VHCD <- bind_rows(VHCD,VHCD_for_visit)

    # Intraepithelial Lymphocytes/Enterocyte count----
    IEL_for_visit <- histology_df %>%
      dplyr::mutate(VISIT=visit) %>%
      dplyr::mutate(WEEKS_FROM_RAND=ifelse(str_detect(visit,"-4$"),-4,24)) %>%
      dplyr::mutate(OBSDATE=`Analysis Date`) %>%
      filter(`Parameter Category 1` == "ESOPHAGOGASTRODUODENOSCOPY", 
             `Parameter Code` == "LYEPIENT", 
             `Visit Name` %in% ifelse(str_detect(visit,"-4$"),c("Visit 2"),c("Visit 6"))) %>%
      dplyr::rename(IELCOUNT=`Analysis Value`)
    IEL <- bind_rows(IEL,IEL_for_visit)
    
    MO_for_visit <- histology_df %>% 
      dplyr::mutate(VISIT=visit) %>%
      dplyr::mutate(WEEKS_FROM_RAND=ifelse(str_detect(visit,"-4$"),-4,ifelse(str_detect(visit,"12$"),12,24))) %>%
      dplyr::mutate(OBSDATE=`Analysis Date`) %>%
      filter(`Parameter Category 1` == "MARSH CLASSIFICATION", 
             `Parameter Code` == "MRSH01", 
             `Visit Name` %in% ifelse(str_detect(visit,"-4$"),c("Visit 2"),c("Visit 6"))) %>%
      dplyr::rename(MO=`Analysis Value (C)`) 
    MO <- bind_rows(MO,MO_for_visit)
    
    qM_for_visit <- histology_df %>% 
      dplyr::mutate(VISIT=visit) %>%
      dplyr::mutate(WEEKS_FROM_RAND=ifelse(str_detect(visit,"-4$"),-4,ifelse(str_detect(visit,"12$"),12,24))) %>%
      dplyr::mutate(OBSDATE=`Analysis Date`) %>%
      filter(`Parameter Category 1` == "MARSH CLASSIFICATION", 
             `Parameter Code` == "MRSH02", 
             `Visit Name` %in% ifelse(str_detect(visit,"-4$"),c("Visit 2"),c("Visit 6"))) %>%
      dplyr::rename(qM=`Analysis Value (C)`) 
    qM <- bind_rows(qM,qM_for_visit)
  }
  
  VHCD <- VHCD %>% 
    dplyr::select("STUDYID", "SITE", "SUBJID", "VISIT", OBSDATE, VHCD)
  IEL <- IEL %>% 
    dplyr::select("STUDYID", "SITE", "SUBJID", "VISIT", IELCOUNT)
  MO <- MO %>% 
    dplyr::select("STUDYID", "SITE", "SUBJID", "VISIT", MO)
  qM <- qM %>% 
    dplyr::select("STUDYID", "SITE", "SUBJID", "VISIT", qM)
  
  histology_df <- VHCD %>% 
    left_join(IEL, by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>% 
    left_join(MO, by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
    left_join(qM, by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
    # These don't appear in the original file
    dplyr::mutate(HLA_G=NA) %>% # (don't have this data)
    dplyr::mutate(HLA_DQ=NA) # (don't have this data)


  if (!is.null(log_dir)) {
    write.csv(histology_df,path(log_dir,"0 histology_df_after_etl_062-2001.csv"),row.names=FALSE)
  }
  return(list(histology_df=histology_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

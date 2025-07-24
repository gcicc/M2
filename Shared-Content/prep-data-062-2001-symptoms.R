# Extract-Transform-Load script for ingesting study-specific data and returning 
# data frame using standardized data dictionary column definitions that are ready
# for joining with similarly-transformed ETL results of other data types. See
# https://en.wikipedia.org/wiki/Extract,_transform,_load for more description of
# generalized approach.

# Symptoms output requires the following:
# Column    type      Notes
# STUDYID   enum      key field for joining
# SITE      string    key field for joining
# SUBJID    string    key field for joining
# OBSDATE   datetime  key field for joining
# VISIT     string    information only, no logic should be performed using it
# GISS      enum      data column
# (other symptom columns will be added later)    

symptoms_062_2001_etl <- function(provenance,log_dir=NULL) {
  distribution.folder <- path(input_dir, "Data_from_Karthik_01_08_2025")
  if (str_contains(input_dir,"TAK-062-2001")) {
    raw <- path(distribution.folder,"CDSD_Histology_data.xlsx")
    symptoms_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(symptoms_df)," rows,",ncol(symptoms_df),"columns\n")), unique)
    etl_output <- demog_062_2001_etl(provenance=provenance,log_dir=log_dir) # we'll need this to get observed dates from visit calendar
    demog_df <- etl_output$demog_df
    # and we only want rows that are contained in both anyway
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # First let's get columns that we'll be using into standard naming and join with demog
  symptoms_df <- symptoms_df %>%
    dplyr::mutate(STUDYID="TAK-062-2001") %>%
    dplyr::rename(mixedSITE=`Study Site Identifier`) %>%
    dplyr::mutate(SITE=as.character(as.numeric(mixedSITE))) %>%
    dplyr::rename(SUBJID=`Subject Identifier for the Study`)
  
  symptoms_df <- left_join(symptoms_df, demog_df, by=c("STUDYID","SITE","SUBJID"))
  if (!is.null(log_dir)) {
    write.csv(symptoms_df,path(log_dir,"symptoms_df after join w demog.csv"), row.names=FALSE)
  }

  # now process the timepoints in accordance with the study calendar
  GISS <- symptoms_df[0,]
  for (visit in c("VISIT 2 - Week -4","VISIT 5 - Week 12","VISIT 6 - Week 24")) {
    # first let's get columns that we'll be using into standard naming and join with demog
    GISS_for_visit <- symptoms_df %>%
      dplyr::filter(`Parameter` == "CDSD GI Score Weekly",
             `Analysis Visit` %in% ifelse(str_detect(visit,"-4$"),
                                          c("Week -4"),ifelse(str_detect(visit,"12$"),c("Week 12"),c("Week 24")))) %>%
      dplyr::mutate(VISIT=visit) %>%
      dplyr::mutate(WEEKS_FROM_RAND=ifelse(str_detect(visit,"-4$"),-4,ifelse(str_detect(visit,"12$"),12,24))) %>%
      # Fix to OBSDATE
      dplyr::mutate(OBSDATE=case_when(is.na(RANDDATE) == FALSE ~ RANDDATE+weeks(WEEKS_FROM_RAND),
                                      .default = ENRDATE+weeks(WEEKS_FROM_RAND+8))  ) %>%
      dplyr::select("STUDYID",
                    "SITE", 
                    "SUBJID",
                    OBSDATE,
                    "VISIT",  
                    "Parameter", 
                    "Parameter Code", 
                    "Analysis Value")
    GISS <- bind_rows(GISS,GISS_for_visit)
  }

  GISS <- GISS %>%
          dplyr::rename(GISS = `Analysis Value`) %>%
          dplyr::select("STUDYID","SITE","SUBJID","OBSDATE","VISIT",GISS)

  lbl <- c("STUDYID","SITE","SUBJID","OBSDATE","VISIT",'GI Severity Score')
  labelled::var_label(GISS) <- split(lbl,names(GISS))

  # final df
  symptoms_df <- GISS %>% 
                 dplyr::select("STUDYID","SITE","SUBJID","OBSDATE","VISIT",GISS)

  if (!is.null(log_dir)) {
    write.csv(symptoms_df,path(log_dir,"0 symptoms_df_after_etl_062-2001.csv"),row.names=FALSE)
  }
  return(list(symptoms_df=symptoms_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

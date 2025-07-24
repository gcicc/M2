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
# (other symtom columns will be added later)    

symptoms_phase0_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"Phase0")) {
    #raw <- path(input_dir,"new vce_old vce_old vhcd_new vhcd_Mean Marsh Score_CDSD Weekly Avg.wide1.ValueChg.bl_d15 - wo top row.xlsx")
    raw <- path(input_dir,"Phase0_Data_from_Feng_1016\\PRO CDSD\\1_Processed Data\\adqs.daily.Value.long.clean2.Weekly Avg.Chg.wide.xlsx")
    symptoms_df <- read_excel(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(symptoms_df)," rows,",ncol(symptoms_df),"columns\n")), unique)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }
  
  # Enrollment dates in demog are relative to DAY 0.  
  # Day -6 in 'new vce_old vce_old vhcd_new vhcd_Mean Marsh Score_CDSD Weekly Avg.wide1.ValueChg.bl_d15 - wo top row' precedes by 6 days
  # and Day 15 is 15 days later...
  demog <- demog_phase0_etl(provenance=list(),log_dir=NULL)$demog_df
  names(symptoms_df)[names(symptoms_df) == 'Time'] <- 'VISIT' 
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD01] CDSD-Did You Have Diarrhea'] <- 'Had_Diarrhea'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD01A] CDSD-How Many Times Have Diarrhea'] <- 'Freq_Diarrhea'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD02] CDSD-Had a Complete Spontaneous BM'] <- 'Had_CSBM'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD02A] CDSD-How Many Complete Spontaneous BMs'] <- 'Freq_CSBM'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD03] CDSD-Have You Had Abdominal Pain'] <- 'Had_Ab_Pain'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD03A] CDSD-Rate Worst Expnce Abdominal Pain'] <- 'Worst_Ab_Pain'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD04] CDSD-Experienced Bloating'] <- 'Had_Bloating'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD04A] CDSD-How Severe Was Bloating at Worst'] <- 'Worst_Bloating'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD05] CDSD-Have You Felt Nauseated'] <- 'Had_Nausea'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD05A] CDSD-How Severe Was Nausea at Worst'] <- 'Worst_Nausea'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD06] CDSD-Have You Experienced Tiredness'] <- 'Had_Tiredness'
  names(symptoms_df)[names(symptoms_df) == 'Value [CDSD06A] CDSD-How Severe Was Tiredness at Worst'] <- 'Worst_Tiredness'
  names(symptoms_df)[names(symptoms_df) == 'Value [ABPAINSS] Abdominal Pain Severity Score'] <- 'AB_Pain_SS'
  names(symptoms_df)[names(symptoms_df) == 'Value [BLOATSS] Bloating Severity Score'] <- 'Bloating_SS'
  names(symptoms_df)[names(symptoms_df) == 'Value [CSBM] CSBM'] <- 'CSBM'
  names(symptoms_df)[names(symptoms_df) == 'Value [DIARRHSS] Diarrhea Severity Score'] <- 'Diarrhea_SS'
  names(symptoms_df)[names(symptoms_df) == 'Value [NAUSEASS] Nausea Severity Score'] <- 'Nausea_SS'
  names(symptoms_df)[names(symptoms_df) == 'Value [TIREDNSS] Tiredness'] <- 'Tiredness_SS'
  names(symptoms_df)[names(symptoms_df) == 'Value [GITOTS] GI Total Severity Score'] <- 'GISS' 
  names(symptoms_df)[names(symptoms_df) == 'Value [NSGISS] Non-Stool GI Specific Symptom Score'] <- 'NSGISS' 
  names(symptoms_df)[names(symptoms_df) == 'Value [TOTSCR] Total Score'] <- 'TotalScore'
  
  symptoms_df <- symptoms_df %>% 
    separate(SUBJID,into=c("SITE","SUBJID"),sep="-") %>% 
    dplyr::select("STUDYID",
                  "SITE",
                  "SUBJID",
                  "VISIT", 
                  "Had_Diarrhea","Freq_Diarrhea","Had_CSBM",
                  "Freq_CSBM","Had_Ab_Pain","Worst_Ab_Pain"  ,                                         
                  "Had_Bloating","Worst_Bloating","Had_Nausea",                                              
                  "Worst_Nausea","Had_Tiredness","Worst_Tiredness",                                         
                  "AB_Pain_SS","Bloating_SS","CSBM",                                                    
                  "Diarrhea_SS","Nausea_SS","Tiredness_SS",                                            
                  "GISS","NSGISS","TotalScore"  ) %>% 
    left_join(demog %>% dplyr::select("STUDYID","SITE","SUBJID", "ENRDATE"),by=c("STUDYID","SITE","SUBJID")) 
  # View(symptoms_df)
  
  symptoms_df <-  symptoms_df %>% dplyr::mutate(OBSDATE=ENRDATE + days(1+7*as.numeric(str_extract(VISIT, "-?\\d+"))))%>%
    dplyr::mutate(VISIT = case_when(VISIT == "Week -1" ~ "Run-in",
                                    VISIT == "Week 2" ~ "Day 15",
                                    VISIT == "Week 6" ~ "Day 42",
                                    .default = VISIT)) %>% #dplyr::filter(VISIT %in% c("Run-in","Day 15","Day 42")) %>%
    dplyr::select(-ENRDATE) %>%
    dplyr::filter(!is.na(OBSDATE))

  if (!is.null(log_dir)) {
    write.csv(symptoms_df,path(log_dir,"0 symptoms_df_after_etl_Phase0.csv"),row.names=FALSE)
  }
  return(list(symptoms_df=symptoms_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

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

serology_062_2001_etl <- function(provenance,log_dir=NULL) {
  if (str_contains(input_dir,"TAK-062-2001")) {
    # 062-2001 has serology, but we don't have it yet, so the dataframe should for now just have the standard column names but no data rows.
    stdcols <- c("STUDYID","SITE","SUBJID","OBSDATE","VISIT","tTG_IgA","DGP_IgA","DGP_IgG")
    # We need to create an empty dataframe with the correct columns types
    serology_df <- data.frame(
      STUDYID = character(),
      VISIT = character(),
      SITE = character(),
      SUBJID = character(),
      OBSDATE = as.POSIXct(character()),
      # Tissue Transglutaminase IgA Antibody (EliA U/mL)
      tTG_IgA = numeric(),
      # Gliadin IgA Antibody (EliA U/mL)
      DGP_IgA = numeric(),
      # Gliadin IgG Antibody (EliA U/mL) - I don't see this in data set
      DGP_IgG = numeric(),
      # Gluten Immunogenic Peptide
      GIP = numeric(),
      # DQ2 (DQA1 0501+0505 and DQB1 0201+0202)
      DQ2 = factor(c(), levels=c("POSITIVE","NEGATIVE")),
      # DQ8 (DQA1 0301 and DQB1 0302)
      DQ8 = factor(c(), levels=c("POSITIVE","NEGATIVE")),
      
      
      stringsAsFactors = FALSE
    )
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }
  
  if (!is.null(log_dir)) {
    write.csv(serology_df,path(log_dir,"0 serology_df_after_etl_062-2001.csv"),row.names=FALSE)
  }
  return(list(serology_df=serology_df,provenance=provenance)) # only one dataframe, in standard columns, all study-specific logic incorporated
}

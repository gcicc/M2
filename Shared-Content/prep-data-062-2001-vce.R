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
#                               elsewhere referred to as Tertile1_Mean)
# Tertile1_Max        real      data column
# Tertile2_Mean       real      data column
# Tertile2_Max        real      data column
# Tertile3_Mean       real      data column
# Tertile3_Max        real      data column
# All_Mean            real      data column
# All_Max             real      data column
# READER              string    key field for joining. Should be two letter human or "MLV"
#                               followed by explicit version if known. Note that ML is 
#                               reserved for the hypothetical human with initials "ML".
# Retest.Flag         logical   Timepoint=="Re-test additional VCE"

vce_062_2001_etl <- function(provenance, log_dir, analysis_type) {
  # as an optimization, if the last file written is already written (by an earlier pass through), we
  # just read in the set fo files for return rather than re-computing. but of course if they don't
  # exist, we perform the computations to create and write them.
  last_file_written <- path(log_dir,"g vce_etl_provenance.txt")
  if (!file.exists(last_file_written)) {
    distribution.folder <- path(input_dir, "INVICRO_DATA_TRANSFER_02-07-2025 cum")
    distribution.file.list <- dir(distribution.folder)
    
    # Prep Anatomical marking data
    if (str_contains(input_dir,"TAK-062-2001")) {
      raw <- path(distribution.folder, dir(distribution.folder, pattern="Takeda_VCE_Anatomical_Marking_Report"))
      AMR <- read_excel(raw)
      provenance <- sapply(append(provenance, paste(raw,":",nrow(AMR)," rows,",ncol(AMR),"columns\n")), unique)
    } else {
      print("Logic error: input dir is not for the expected study.")
      return(NULL) 
    }
    
    AMR <- AMR %>% 
           dplyr::rename(
                STUDYID=Protocol,
                mixedSITE=`Site Number`,
                SUBJID=`Subject ID`,
                VISIT=`Imaging Timepoint`,
                CapsuleID=`Capsule ID`,
                IQ_SB=`What was the image quality over the small bowel?`,
                IQ_T1=`What was the image quality over the first third of the small bowel?`,
                IQ_T2=`What was the image quality over the second third of the small bowel?`,
                IQ_T3=`What was the image quality over the final third of the small bowel?`,
                CecumReached=`Was the cecum reached?`) %>%
           dplyr::mutate(SITE=as.character(as.numeric(mixedSITE))) %>%
           dplyr::select(-mixedSITE)
    names(AMR)
    lbl <- c("Study Identifier",
             "Study Site Identifier",
             "Subject Identifier for the Study",
             "Analysis Visit", 'Capsule ID', 
             'What was the image quality over the small bowel?',
             'What was the image quality over the first third of the small bowel?',
             'What was the image quality over the second third of the small bowel?',
             'What was the image quality over the final third of the small bowel?',
             'Was the cecum reached?'
             )
    labelled::var_label(AMR) <- split(lbl, names(AMR))
    write.csv(AMR,path(log_dir,"a AMR.csv"), row.names=FALSE)

    # the vce ETL is more complicated for 062-2001 than the earlier studies, and needs to use
    # information from other sister ETLs to complete the logic
    etl_output <- demog_062_2001_etl(provenance,log_dir=log_dir)
    demog_df <- etl_output$demog_df
    etl_output <- histology_062_2001_etl(provenance,log_dir=log_dir)
    histology_df <- etl_output$histology_df
    etl_output <- symptoms_062_2001_etl(provenance,log_dir=log_dir)
    symptoms_df <- etl_output$symptoms_df    

    # ML Region-level Metrics files ----
    metrics.ML.region_level.files <- dir(distribution.folder, pattern="Metrics", full.names=TRUE)
    metrics.ML.region_level.data <- data.frame()
    file_limit_for_debugging <- 5

    # Initialize the data table and provenance list
    metrics.ML.region_level.data <- data.table()
    # Read and bind data more efficiently
    metrics.ML.region_level.data <- rbindlist(lapply(metrics.ML.region_level.files, function(file) {
      df <- fread(file)
      provenance <<- unique(append(provenance, paste(file, ":", nrow(df), " rows,", ncol(df), "columns\n")))
      return(df)
    }), use.names = TRUE, fill = TRUE)
    
    # provenance can be converted back to a dataframe if needed
    provenance <- data.frame(provenance = provenance)
    metrics.ML.region_level.data <- as.data.frame(metrics.ML.region_level.data)
    cat(paste0(sum(table(metrics.ML.region_level.data$Timepoint)), " videos reported\n"))
    write.csv(metrics.ML.region_level.data,path(log_dir,"b metrics.ML.region_level.data.csv"),row.names=FALSE)

    vce_df <- metrics.ML.region_level.data
    names(vce_df)[names(vce_df) == 'First5Percent_MeanFrame'] <- 'First5Percent_Mean'
    names(vce_df)[names(vce_df) == 'Tertile1_Mean'] <- 'BVA'
    names(vce_df)[names(vce_df) == 'Time'] <- 'OBSDATE'
    vce_df <- vce_df %>% 
      dplyr::mutate(STUDYID="TAK-062-2001") %>%
      dplyr::mutate(SUBJID=SubjectID) %>%
      separate(SubjectID,into=c("mixedSITE","individual"),sep="-") %>% 
      dplyr::mutate(SITE=as.character(as.numeric(mixedSITE))) %>%
      dplyr::mutate(Retest.Flag=Timepoint=="Re-test additional VCE") %>%
      dplyr::mutate(VISIT=ifelse(Timepoint=="Re-test additional VCE","VISIT 2 - Week -4",Timepoint)) %>% # note that test-retest is really indicated by OBSDATES begin in a window, not by explicit use of VISIT
      left_join(demog_df, by = c("STUDYID", "SITE", "SUBJID")) %>%
      dplyr::mutate(WEEKS_FROM_RAND=ifelse(str_detect(VISIT,"-4$"),-4,ifelse(str_detect(VISIT,"12$"),12,24))) %>%
      dplyr::mutate(OBSDATE=case_when(is.na(RANDDATE) == FALSE ~ RANDDATE+weeks(WEEKS_FROM_RAND),
                                      .default = ENRDATE+weeks(WEEKS_FROM_RAND+8))  ) %>%
      dplyr::select(STUDYID,SITE,SUBJID,OBSDATE,VISIT,First5Percent_Mean,
        BVA,Tertile1_Max,Tertile2_Mean,Tertile2_Max,Tertile3_Mean,Tertile3_Max,All_Mean,All_Max,Retest.Flag) %>% 
      dplyr::rename(First5Percent_Mean.orig=First5Percent_Mean) 
    
    write.csv(metrics.ML.region_level.data,path(log_dir,"c vce_df.csv"), row.names=FALSE)
    
    # Frame-level ML ----
    ML.frame_level.files <- dir(distribution.folder, pattern="MLOutput", full.names=TRUE)
    ML.frame_level.data <- data.frame()
    file_limit_for_debugging <- 5
    
    # for TAK-062-2001, we do some helpful detailed annotation, including correcting values, for
    # some scores 
    details_dir <- path(log_dir,"subject_details")
    mkdirs(details_dir)
    
    # Increase future.globals.maxSize
    options(future.globals.maxSize = 2 * 1024 ^ 3)  # 2 GB
    
    # Plan for multiprocess parallel backend
    available_cores <- parallel::detectCores()
    plan(multisession, workers= available_cores-1)
    
    process_file <- function(raw, data_files) {
      df.in <- read_csv(raw)
      
      df.in <- df.in %>%
        dplyr::mutate(STUDYID = "TAK-062-2001") %>%
        dplyr::rename(Position = position, Intensity = intensity, VISIT = timepoint) %>%
        dplyr::mutate(SUBJID = subjectId) %>%
        separate(subjectId, into = c("mixedSITE", "individual"), sep = "-") %>%
        dplyr::mutate(SITE = as.character(as.numeric(mixedSITE))) %>%
        dplyr::mutate(READER = str_replace(mlModel, "models/", "ML")) %>%
        dplyr::select(STUDYID, SITE, SUBJID, VISIT, Position, Intensity, READER) %>%
        left_join(vce_df, by = c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
        left_join(demog_df, by = c("STUDYID", "SITE", "SUBJID")) %>%
        left_join(histology_df, by = c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
        dplyr::rename(OBSDATE = OBSDATE.x) %>%
        left_join(symptoms_df, by = c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
        dplyr::rename(OBSDATE = OBSDATE.x)

      raw_frame_score_path <- path(details_dir,paste0("vA&STUDYID=",unique(df.in$STUDYID),"&SITE=",unique(df.in$SITE),"&SUBJID=",unique(df.in$SUBJID),"&VISIT=",unique(df.in$VISIT),"&READER=",unique(df.in$READER),".csv"))
      write.csv(df.in%>%dplyr::select(Position,Intensity),raw_frame_score_path,row.names=FALSE)

      df.in <- df.in %>% mutate(semi_decile=cut(Position, breaks = c(-.05, 0, .3333, .6666, 1)))
      replacement <- df.in %>% dplyr::filter(semi_decile=="(-0.05,0]") %>% dplyr::select(Position, Intensity) %>% summarize(mean=mean(Intensity)) %>% dplyr::pull(mean)
      df.in <- df.in %>% mutate(First5Percent_Mean = replacement) %>% dplyr::select(-semi_decile)
      if (analysis_type == "Gastroenterologist Agreement") {
        write.csv(frame_level_df_cuts, file.path(details_dir, paste(unique(df.in$SUBJID), "frame_level_df_cuts.csv")))
      }
      
      write.csv(vce_df, file.path(log_dir, "d_vce_df_w_some_fixed_MARCS_scores.csv"), row.names = FALSE)
      
      if (nrow(ML.frame_level.data) == 0) {
        ML.frame_level.data <- df.in
      } else {
        ML.frame_level.data <- bind_rows(ML.frame_level.data, df.in)
      }
      
      p <- ggplot(df.in, aes(x = Position, y = Intensity)) +
        geom_line(size = .25) + 
        scale_color_manual(values = cbp2) +
        labs(x = "Position", y = "CE-VAST", title = paste0(df.in$SUBJID, " at ", df.in$VISIT, ", ", ifelse(is.na(df.in$RANDDATE), " not randomized", " randomized"))) +
        geom_vline(xintercept = c(-.05, 0, .3333, .6666, 1.0), linetype = "solid", color = "black") +
        geom_vline(xintercept = seq(-.05, .95, by = 0.05), linetype = "dotted", color = "darkgray") +
        annotate("text", x = .4, y = 3.5, label = paste("VHCD", round(df.in$VHCD[1], 2))) +
        annotate("text", x = .4, y = 3.3, label = paste("IELCOUNT", round(df.in$IELCOUNT[1], 2))) +
        annotate("text", x = .4, y = 3.1, label = paste("GISS", round(df.in$GISS[1], 2))) +
        annotate("text", x = .4, y = 2.9, label = paste("First5%_Mean (orig)", round(df.in$First5Percent_Mean.orig[1], 2))) +
        annotate("text", x = .4, y = 2.7, label = paste("First5%_Mean (fix)", round(df.in$First5Percent_Mean[1], 2))) +
        annotate("text", x = .85, y = 3.5, label = paste("BVA", round(df.in$BVA[1], 2))) +
        annotate("text", x = .85, y = 3.3, label = paste("Tertile1_Max", round(df.in$Tertile1_Max[1], 2))) +
        annotate("text", x = .85, y = 3.1, label = paste("Tertile2_Max", round(df.in$Tertile2_Max[1], 2))) +
        annotate("text", x = .85, y = 2.9, label = paste("Tertile3_Max", round(df.in$Tertile3_Max[1], 2))) +
        annotate("text", x = .85, y = 2.7, label = paste("All (tertile) Max", round(df.in$All_Max[1], 2)))
      
      ggsave(file.path(details_dir, paste(unique(df.in$SUBJID), df.in$VISIT, "intensity_v_position.png")), plot = p)
      
      return(df.in)
    }
    
    ML.frame_level.data <- future_map(ML.frame_level.files, process_file, data_files = ML.frame_level.files) %>% bind_rows()
   for.join <-  ML.frame_level.data %>% dplyr::select(STUDYID, SITE, SUBJID, VISIT, First5Percent_Mean) %>% group_by(STUDYID, SITE, SUBJID, VISIT) %>%
      slice(1)
    vce_df <- dplyr::mutate(vce_df, READER=unique(ML.frame_level.data$READER))
    vce_df <- vce_df %>% left_join(for.join, by=c("STUDYID","SITE","SUBJID","VISIT"))

    # variable histograms
    vce_df.gathered <- vce_df %>%
                      dplyr::filter(VISIT=="VISIT 2 - Week -4") %>%
                      left_join(demog_df, by=c("STUDYID", "SITE", "SUBJID")) %>%
                      left_join(histology_df, by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
                      dplyr::rename(OBSDATE=OBSDATE.x) %>%
                      left_join(symptoms_df, by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
                      dplyr::rename(OBSDATE=OBSDATE.x) %>%
                      dplyr::mutate(am=ifelse(is.na(RANDDATE),"not rand","rand")) %>% 
                      dplyr::select(am, First5Percent_Mean, BVA, VHCD, IELCOUNT, GISS) %>% 
                      dplyr::mutate_if(is.numeric, scale) %>% 
                      gather(key, value, -am)

    ggplot(vce_df.gathered, aes(value, fill=factor(am))) + scale_color_manual(values=cbp2) + 
      geom_histogram(aes(y=..density..), alpha=0.6, position="identity") + 
      facet_wrap(~key, scales='free_x')
    ggsave(path(details_dir,paste("variable histograms.png")))

    # Frame-level Human ----
    Human.frame_level.files <- dir(distribution.folder, pattern="ScoredFrames", full.names=TRUE)
    Human.frame_level.data <- data.frame()
    for (Human.frame_level.file in Human.frame_level.files) {
      raw <- Human.frame_level.file
      df <- read_csv(raw)
      provenance <- sapply(append(provenance, paste(raw,":",nrow(df)," rows,",ncol(df),"columns\n")), unique)
      if (nrow(Human.frame_level.data) == 0) {
        Human.frame_level.data <- df
      } else {
        Human.frame_level.data <- bind_rows(Human.frame_level.data, df)
      }
    }

    # complete processing the human data if it exists and add it to the ML, or just go forward with the ML
    if (nrow(Human.frame_level.data)>0) {
      names(Human.frame_level.data)[names(Human.frame_level.data) == 'timestamp'] <- 'OBSDATE'
      Human.frame_level.data <- Human.frame_level.data %>% 
        dplyr::mutate(STUDYID="TAK-062-2001") %>%
        dplyr::rename(
          Position=position,
          Intensity=intensity,
          VISIT=timepoint
        ) %>%
        dplyr::mutate(SUBJID=subjectId) %>%
        separate(subjectId,into=c("mixedSITE","individual"),sep="-") %>% 
        dplyr::mutate(SITE=as.character(as.numeric(mixedSITE))) %>%
        dplyr::mutate(READER=str_replace(mlModel, "models/", "ML")) %>%
        dplyr::select(STUDYID,SITE,SUBJID,OBSDATE,VISIT,Position,Intensity,READER)
      # we use an outer join on the frames data to keep all rows
      frame_level_df <- outer_join(ML.frame_level.data, Human.frame_level.data, by=c("STUDYID","SITE","SUBJID","OBSDATE","VISIT","Position"))
    } else {
      frame_level_df <- ML.frame_level.data
    }
    
    # now apply the differing image quality levels
    write.csv(vce_df,path(log_dir,"e vce_df_after_etl_062-2001 but BeforeIQLevels.csv"),row.names=FALSE)
    if (analysis_type == "Gastroenterologist Agreement") {
      write.csv(frame_level_df,path(log_dir,"ea frame_level_df_after_etl_062-2001 but BeforeIQLevels.csv"),row.names=FALSE)
    }
    vce_df <- inner_join(vce_df, AMR, by=c("STUDYID", "SITE", "SUBJID", "VISIT"))
    frame_level_df <- inner_join(frame_level_df, AMR, by=c("STUDYID", "SITE", "SUBJID", "VISIT"))
    write.csv(vce_df,path(log_dir,"f vce_df_after_etl_062-2001 w AMR.csv"),row.names=FALSE)
    if (analysis_type == "Gastroenterologist Agreement") {
      write.csv(frame_level_df,path(log_dir,"fa frame_level_df_after_etl_062-2001 w AMR.csv"),row.names=FALSE)
    }

    vce_df.WideIQ_Std <- vce_df %>%
                      dplyr::filter(!is.na(CecumReached) & CecumReached=="Yes") %>%
                      dplyr::filter(IQ_T1!="poor")
    
    vce_df.NarrowIQ_Std <- vce_df.WideIQ_Std %>%
                      dplyr::filter(IQ_SB!="poor")
    
    frame_level_df.WideIQ_Std <- frame_level_df %>%
                      dplyr::filter(CecumReached=="Yes") %>%
                      dplyr::filter(IQ_T1!="poor")
    
    frame_level_df.NarrowIQ_Std <- frame_level_df.WideIQ_Std %>%
                      dplyr::filter(IQ_SB!="poor")
    
    write.csv(vce_df.WideIQ_Std,path(log_dir,"g vce_df.WideIQ_Std_as_returned.csv"),row.names=FALSE)
    write.csv(vce_df.NarrowIQ_Std,path(log_dir,"g vce_df.NarrowIQ_Std_as_returned.csv"),row.names=FALSE)
    if (analysis_type == "Gastroenterologist Agreement") {
      write.csv(frame_level_df.WideIQ_Std,path(log_dir,"ga frame_level_df.WideIQ_Std_as_returned.csv"),row.names=FALSE)
      write.csv(frame_level_df.NarrowIQ_Std,path(log_dir,"ga frame_level_df.NarrowIQ_Std_as_returned.csv"),row.names=FALSE)
    } else {
      frame_level_df.WideIQ_Std <- NULL
      frame_level_df.NarrowIQ_Std <- NULL
    }
    write.table(provenance, last_file_written,row.names=FALSE)
  } else {
    AMR <- read.csv(path(log_dir,"a AMR.csv"))
    vce_df.WideIQ_Std <- read.csv(path(log_dir,"g vce_df.WideIQ_Std_as_returned.csv"))
    vce_df.NarrowIQ_Std <- read.csv(path(log_dir,"g vce_df.NarrowIQ_Std_as_returned.csv"))
    if (analysis_type == "Gastroenterologist Agreement") {
      frame_level_df.WideIQ_Std <- read.csv(path(log_dir,"ga frame_level_df.WideIQ_Std_as_returned.csv"))
      frame_level_df.NarrowIQ_Std <- read.csv(path(log_dir,"ga frame_level_df.NarrowIQ_Std_as_returned.csv"))
    } else {
      frame_level_df.WideIQ_Std <- NULL
      frame_level_df.NarrowIQ_Std <- NULL
    }
    provenance <- read.table(last_file_written)
  }

  # return, listing the two IQ levels for this study
  return(list(AMR=AMR,
              vce_dfs=list(WideIQ_Std=vce_df.WideIQ_Std,NarrowIQ_Std=vce_df.NarrowIQ_Std),
              frame_levels=list(WideIQ_Std=frame_level_df.WideIQ_Std,NarrowIQ_Std=frame_level_df.NarrowIQ_Std),
              provenance=provenance)) # a df for scores and another for marking, both in standard columns, all study-specific logic incorporated
}

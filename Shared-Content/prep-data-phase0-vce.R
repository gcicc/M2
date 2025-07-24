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

vce_phase0_etl <- function(provenance,log_dir=NULL) {
  distribution.folder <- path(input_dir, "Phase0_Data_from_PRA")
  if (str_contains(input_dir,"Phase0")) {
    provenance <- append(provenance, "\n\n")
    provenance <- sapply(append(provenance, read_rtf(path(distribution.folder, dir(distribution.folder, pattern="readme")))), unique)
    provenance <- append(provenance, "\n")
    raw <- path(distribution.folder,"SubjectInfo_20190507.xlsx")
    SubjectInfo <- read_excel(raw) %>% dplyr::rename(VideoID='Patient ID')
    provenance <- sapply(append(provenance, paste(raw,":",nrow(SubjectInfo)," rows,",ncol(SubjectInfo),"columns\n")), unique)
    write.csv(SubjectInfo,path(log_dir,"a SubjectInfo.csv"),row.names=FALSE)
    raw <- path(distribution.folder,"MLV2.2","SelectedSnippets","Metrics.csv")
    Metrics <- read.csv(raw) %>% dplyr::rename(VideoID='SubjectID')
    provenance <- sapply(append(provenance, paste(raw,":",nrow(Metrics)," rows,",ncol(Metrics),"columns\n")), unique)
    write.csv(Metrics,path(log_dir,"a Metrics.csv"),row.names=FALSE)
    raw <- path(input_dir,"Phase_0_global_position_time_stats.csv")
    global_position_time_stats <- read.csv(raw)
    provenance <- sapply(append(provenance, paste(raw,":",nrow(global_position_time_stats)," rows,",ncol(global_position_time_stats),"columns\n")), unique)
    write.csv(global_position_time_stats,path(log_dir,"a global_position_time_stats.csv"),row.names=FALSE)
  } else {
    print("Logic error: input dir is not for the expected study.")
    return(NULL) 
  }

  # first join SubjectInfo and global_position_time_stats
  SubjectInfo$VideoID <- sub("00$", "", SubjectInfo$VideoID)
  SubjectInfo.global_position_time_stats <- full_join(global_position_time_stats, SubjectInfo, by=c("VideoID"))

  # then bring in metrics
  vce_df <- left_join(Metrics, SubjectInfo.global_position_time_stats, by=c("VideoID")) %>% dplyr::filter(!is.na(FirstCaecumIndex))
  write.csv(vce_df,path(log_dir,"b after join.csv"),row.names=FALSE)

  # we'll need this for setting OBSDATE
  etl_output <- demog_phase0_etl(provenance,log_dir=log_dir)
  demog_df <- etl_output$demog_df

  names(vce_df)[names(vce_df) == 'First5..Mean'] <- 'First5Percent_Mean'
  names(vce_df)[names(vce_df) == 'Tertile1.Mean'] <- 'BVA'
  names(vce_df)[names(vce_df) == 'Tertile1.Max'] <- 'Tertile1_Max'
  names(vce_df)[names(vce_df) == 'Tertile2.Mean'] <- 'Tertile2_Mean'
  names(vce_df)[names(vce_df) == 'Tertile2.Max'] <- 'Tertile2_Max'
  names(vce_df)[names(vce_df) == 'Tertile3.Mean'] <- 'Tertile3_Mean'
  names(vce_df)[names(vce_df) == 'Tertile3.Max'] <- 'Tertile3_Max'
  names(vce_df)[names(vce_df) == 'All.Mean'] <- 'All_Mean'
  names(vce_df)[names(vce_df) == 'All.Max'] <- 'All_Max'
  names(vce_df)[names(vce_df) == 'Visit No.'] <- 'VisitNo'
  vce_df.MLV <- vce_df %>% 
    dplyr::mutate(STUDYID="TIMP-GLIA-5001") %>%
    separate(ID,into=c("SITE","SUBJID"),sep="-") %>% 
    dplyr::mutate(VISIT=ifelse(VisitNo==1,"Run-in",ifelse(VisitNo==2,"Day 15","Day 42"))) %>%
    dplyr::mutate(READER=paste0("ML", Reader)) %>%
    left_join(demog_df, by=c("STUDYID", "SITE", "SUBJID")) %>%
    dplyr::mutate(OBSDATE=as.Date(ifelse(VISIT=="Run-in",ENRDATE-days(6),ifelse(VISIT=="Day 15",ENRDATE+days(15),ENRDATE+days(42))))) %>%
    dplyr::arrange(STUDYID,SITE,SUBJID,VISIT)
  write.csv(vce_df.MLV,path(log_dir,"c processed machine learning.csv"),row.names=FALSE)

  # for phase0, we do some helpful detailed annotation 
  etl_output <- histology_phase0_etl(provenance,log_dir=log_dir)
  histology_df <- etl_output$histology_df
  etl_output <- symptoms_phase0_etl(provenance,log_dir=log_dir)
  symptoms_df <- etl_output$symptoms_df

  details_dir <- path(log_dir,"subject_details")
  mkdirs(details_dir)
  raw <- path(distribution.folder,"Curves_Collected.csv")
  curves <- read_csv(raw)
  provenance <- sapply(append(provenance, paste(raw,":",nrow(df)," rows,",ncol(df),"columns\n")), unique)
  # raw <- path(distribution.folder,"outputUnblinded_20190122.csv")
  # markings <- read_excel(raw)
  # provenance <- sapply(append(provenance, paste(raw,":",nrow(df)," rows,",ncol(df),"columns\n")), unique)
  # names(markings)[names(markings) == 'Visit No.'] <- 'VisitNo'
  # markings <- vce_df %>% 
  #   dplyr::mutate(STUDYID="TIMP-GLIA-5001") %>%
  #   separate(ID,into=c("SITE","SUBJID"),sep="-") %>% 
  #   dplyr::mutate(VISIT=ifelse(VisitNo==1,"Run-in",ifelse(VisitNo==2,"Day 15","Day 42"))) %>%
  #   dplyr::mutate(marking=toupper(Value)) %>%
  #   dplyr::filter(marking=="AMPULLA") %>%
  #   #dplyr::mutate(ampulla=(Frame_Time/Read_Time)/1.05) %>% # need to adjust for x scaling to start at -0.05
  #   dplyr::mutate(ampulla=((AmpullaIndex-LastPylorusIndex)/(FirstCaecumIndex-LastPylorusIndex))-1/1.05) %>% # need to adjust for x scaling to start at -0.05
  #   dplyr::select(STUDYID,SITE,SUBJID,VISIT,marking,ampulla)

  names(curves)[names(curves) == 'Reader/Position'] <- 'Reader'
  curves <- curves %>%
    dplyr::mutate(STUDYID="TIMP-GLIA-5001") %>%
    separate(Video,into=c("mixedSITE","individual","VisitNo"),sep="-") %>% 
    dplyr::mutate(SITE=as.character(as.numeric(mixedSITE))) %>%
    dplyr::mutate(SUBJID=as.character(as.numeric(individual))) %>%
    dplyr::mutate(VISIT=ifelse(VisitNo==1,"Run-in",ifelse(VisitNo==2,"Day 15","Day 42"))) %>%
    dplyr::mutate(reader=Reader) %>%
    dplyr::select(-c(mixedSITE,individual,VisitNo,Reader)) %>%
    left_join(histology_df %>% dplyr::select("STUDYID","SITE","SUBJID","VISIT","VHCD","IELCOUNT"), by=c("STUDYID","SITE","SUBJID","VISIT")) %>%
    left_join(symptoms_df %>% dplyr::select("STUDYID","SITE","SUBJID","VISIT","GISS"), by=c("STUDYID","SITE","SUBJID","VISIT")) %>%
    left_join(vce_df.MLV %>% dplyr::select("STUDYID","SITE","SUBJID","VISIT","AmpullaIndex","LastPylorusIndex","FirstCaecumIndex"), by=c("STUDYID","SITE","SUBJID","VISIT")) %>%
    dplyr::mutate(ampulla=((AmpullaIndex-LastPylorusIndex)/(FirstCaecumIndex-LastPylorusIndex))-(0.05/1.05)) %>% # need to adjust for x scaling to start at -0.05
    dplyr::select(-c("AmpullaIndex","LastPylorusIndex","FirstCaecumIndex")) %>% 
    # Excluding human readers
    dplyr::filter(grepl(x = reader, pattern="MLV"))
  write.csv(curves,path(log_dir,"ea curves.csv"),row.names=FALSE)

  makeThumb <- function(file, height, width) {
    require(png)
    img <- readPNG(file)

    png(file=path(dirname(file),paste0(file_path_sans_ext(basename(file)), "_thumb.png")), height=height, width=width, units="px")
      par(mar=c(0,0,0,0), xaxs="i", yaxs="i", ann=FALSE)
      plot(1:2, type='n', xaxt="n", yaxt="n", xlab="", ylab="")
      lim <- par()
      rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    dev.off()
  }

  .across_curves_func <- function(.x, .y, log_dir) {
    temp <- .x  %>%
      dplyr::select(-c(VHCD,IELCOUNT,GISS)) %>%
      pivot_longer(cols=c(1:1000)) %>%
      dplyr::mutate(name=as.numeric(name))
    write.csv(temp,path(log_dir,"ea pivoted.csv"),row.names=FALSE)
    curve_png <- path(details_dir,paste0(.y$SITE, "-", .y$SUBJID, " ", .y$VISIT, ".png"))
    curve <- ggplot(temp,aes(x=name,y=value, color=reader))+
      geom_line() + scale_color_manual(values=cbp2) + coord_cartesian(xlim=c(-0.05, 1.00)) +
      labs(x="Position", y="CE-VAST", title=paste0(.y$SITE, "-", .y$SUBJID, " at ", .y$VISIT)) +
      geom_vline(xintercept=c(-.05, 0, .3333, .6666, 1.0), linetype="solid", color="black") +
      geom_vline(xintercept=seq(-.05, .95, by=0.05), linetype="dotted", color="darkgray") +
      geom_vline(xintercept=.x$ampulla, linetype="solid", color="red") +
      annotate("text", x=.4, y=3.5, label=paste("VHCD", round(.x$VHCD[1],2)))+
      annotate("text", x=.4, y=3.3, label=paste("IELCOUNT", round(.x$IELCOUNT[1],2)))+
      annotate("text", x=.4, y=3.1, label=paste("GISS", round(.x$GISS[1],2)))
    ggsave(curve_png)
    #makeThumb(curve_png, height=300, width=480)
    return(.x %>% dplyr::mutate(curve=list(curve)))
  }
  with_plots <- curves %>%
                group_by(STUDYID, SITE, SUBJID, VISIT) %>% 
                dplyr::group_modify(.across_curves_func, log_dir, .keep=FALSE)
  
  # Now the human reads
  raw <- path(distribution.folder,"TIMPGLIA_INVICRO_VCE_2019-05-28.csv")
  vce_df <- read_excel(raw)
  provenance <- sapply(append(provenance, paste(raw,":",nrow(vce_df)," rows,",ncol(vce_df),"columns\n")), unique)
  
  names(vce_df)[names(vce_df) == 'Tertile1_Mean'] <- 'BVA'
  names(vce_df)[names(vce_df) == 'Visit No.'] <- 'VisitNo'
  vce_df.SL <- vce_df %>% 
    dplyr::mutate(STUDYID="TIMP-GLIA-5001") %>%
    separate(ID,into=c("SITE","SUBJID"),sep="-") %>% 
    dplyr::mutate(VISIT=ifelse(VisitNo==1,"Run-in",ifelse(VisitNo==2,"Day 15","Day 42"))) %>%
    dplyr::mutate(READER="SL") %>%
    dplyr::arrange(STUDYID,SITE,SUBJID,VISIT) %>%
    dplyr::select(STUDYID,SITE,SUBJID,VISIT,READER, First5Percent_Mean,BVA,Tertile1_Max,
      Tertile2_Mean,Tertile2_Max,Tertile3_Mean,Tertile3_Max,All_Mean,All_Max) %>%
    left_join(vce_df.MLV %>% dplyr::select(STUDYID,SITE,SUBJID,OBSDATE,VISIT), by=c("STUDYID","SITE","SUBJID","VISIT")) 
  write.csv(vce_df.SL,path(log_dir,"d human reads.csv"),row.names=FALSE)

  vce_df <- bind_rows(vce_df.MLV, vce_df.SL) %>%
            dplyr::select(STUDYID,SITE,SUBJID,OBSDATE,VISIT,READER,First5Percent_Mean,BVA,Tertile1_Max,
                          Tertile2_Mean,Tertile2_Max,Tertile3_Mean,Tertile3_Max,All_Mean,All_Max)
  write.csv(vce_df,path(log_dir,"0 vce_df_after_etl_Phase0.csv"),row.names=FALSE)

  # now return (only one IQ level for this study)
  return(list(vce_dfs=list(vce_df),AMR=NULL,provenance=provenance)) # a df for scores in standard columns, all study-specific logic incorporated. No marking in phase0.
}

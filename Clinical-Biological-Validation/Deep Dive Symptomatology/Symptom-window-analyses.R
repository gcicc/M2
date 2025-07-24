# Symptom window analysis

study <- params$study
repo_clone <- params$repo_clone
input_dir <- params$input_dir

deepSympt_path <- path(output_dir, "deepSympt")
dir.create(deepSympt_path)
dir.create(path(deepSympt_path, "_details"))

# first truncate summary csv files for this new run (might have been there if multiple runs in same day)
analysis_summary <- path(deepSympt_path, paste0("analysis-summary", Sys.Date(), ".csv"))
if (file.exists(analysis_summary)) {
  file.remove(analysis_summary)
}
CDSD_averages <- path(deepSympt_path, paste("CDSD_avg.csv"))
if (file.exists(CDSD_averages)) {
  file.remove(CDSD_averages)
}
source(paste0(repo_clone, "/Shared-Content/preamble.R"))
functions_path <- path(repo_clone, "Clinical-Biological-Validation", "association", "functions")
r_files <- list.files(path=functions_path, pattern="\\.R$", full.names=TRUE)
for (file in r_files) {
  source(file)
}
table_path <- path(deepSympt_path, "tables")
dir.create(table_path)
figure_path <- path(deepSympt_path, "figures")
dir.create(figure_path)

everything_but_CDSD <- read_csv(path(output_dir, "7 global_analysis_set-prefilter-ML.csv")) %>%
                       dplyr::select(-c(GISS,OBSDATE.symptoms,Disease_Burden)) #remove all the symptoms-derived columns 

CDSD_daily <- read_excel(path(input_base_dir,"TAK-062-2001/Data_from_Karthik_11_04_2024/CDSD_daily_data.xlsx")) 
names(CDSD_daily)[names(CDSD_daily) == 'Unique Subject Identifier'] <- 'SUBJID'
names(CDSD_daily)[names(CDSD_daily) == 'Date/Time of Finding'] <- 'date'
CDSD_daily <- CDSD_daily %>%
  dplyr::mutate(OBSDATE=as.Date(date)) %>%
  dplyr::group_by(`Question Name`) %>%
  dplyr::select(SUBJID, OBSDATE, `Question Name`, `Numeric Finding in Standard Units`) %>%
  pivot_wider(names_from=`Question Name`, values_from=`Numeric Finding in Standard Units`, values_fn=mean) %>%
  dplyr::arrange(SUBJID, OBSDATE) %>%
  # dplyr::group_by(SUBJID) %>% 
  # dplyr::mutate(GISS=(`CDS1-How Severe Was Your Abdominal Pain`+
  #                     `CDS1-How Severe Was Your Bloating`+
  #                     `CDS1-How Severe Was Your Diarrhea`+
  #                     `CDS1-How Severe Was Your Nausea`)/4) %>%
  dplyr::mutate(SUBJID=str_remove(SUBJID, "TAK-062-2001-")) %>%
  dplyr::mutate(SITE=str_split(SUBJID, "-")[[1]][1]) %>%
  dplyr::mutate(STUDYID="TAK-062-2001")
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-Bowel Movements Type 6 or 7'] <- 'BMs'
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-How Many Bowel Movements'] <- 'Type_6_or_7_BMs'
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-How Many Times Did You Vomit'] <- 'Times_Vomit'
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-How Severe Was Your Abdominal Pain'] <- 'AbdPain_Severity'
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-How Severe Was Your Bloating'] <- 'Bloating_Severity'
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-How Severe Was Your Diarrhea'] <- 'Diarrhea_Severity'
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-How Severe Was Your Nausea'] <- 'Nausea_Severity'
names(CDSD_daily)[names(CDSD_daily) == 'CDS1-How Severe Was Your Tiredness'] <- 'Tiredness_Severity'
write.csv(CDSD_daily, path(deepSympt_path, "CDSD_daily.csv"), row.names=FALSE)

#endpoints <- c("VHCD","IELCOUNT","BVA","First5Percent_Mean","Tertile1_Max","Tertile2_Mean","Tertile2_Max","Tertile3_Mean","Tertile3_Max","All_Mean","All_Max")
endpoints <- c("BVA")
comparators <- c("BMs","Type_6_or_7_BMs","Times_Vomit","AbdPain_Severity","Bloating_Severity","Diarrhea_Severity","Nausea_Severity","Tiredness_Severity"#,
  # "BMs.gr","Type_6_or_7_BMs.gr","Times_Vomit.gr","AbdPain_Severity.gr","Bloating_Severity.gr","Diarrhea_Severity.gr","Nausea_Severity.gr","Tiredness_Severity.gr"
  )

for (endpoint in endpoints) {
  print(paste("beginning", endpoint, "..."))
  # Filenames are too long for Windows
  # Error in path_tidy(.Call(fs_path_, lapply(args, function(x) enc2utf8(as.character(x))),  : 
  # Total path length must be less than PATH_MAX: 260
  this_endpoint <- everything_but_CDSD %>%
                   dplyr::mutate(STUDYID="TAK-062-2001") %>%
                   dplyr::filter(!is.na(!!sym(endpoint))) %>%
                   dplyr::mutate(OBSDATE=as.Date(ifelse(!!sym(endpoint) %in% c("VHCD","IELCOUNT"),OBSDATE.histology,OBSDATE.VCE))) %>%
                   dplyr::select(STUDYID,SITE,SUBJID,!!sym(endpoint),OBSDATE,VISIT)
  write.csv(this_endpoint, path(deepSympt_path, paste0(endpoint, ".csv")), row.names=FALSE)
  unique_visits_forthis_endpoint <- unique(na.omit(this_endpoint$VISIT))
  #six_weeks <- 6*7
  #for (time.offset in seq(-six_weeks, six_weeks, 3)) { # shift by 3 days each iteration
  three_weeks <- 3*7
  for (time.offset in seq(0, three_weeks, 7)) { # only look at present and in 3 weeks into future (no past)
    two_week_window <- 14
    #for (window.width in seq(2, two_week_window, 2)) { # window widths always even
    for (window.width in seq(8, 8, 8)) { # only do 8 day windows
      for_analysis <- data.frame()
      for (visit in unique_visits_forthis_endpoint) {
        this_endpoint_and_visit <- this_endpoint %>% dplyr::filter(VISIT==visit)
        this_endpoint_visit_and_window <- this_endpoint_and_visit %>%
                                           dplyr::filter(VISIT==visit) %>%
                                           dplyr::mutate(study.date.left=as.Date(OBSDATE+days(time.offset)-days(window.width/2)),
                                                         study.date.right=as.Date(OBSDATE+days(time.offset)+days(window.width/2)))
        write.csv(this_endpoint_visit_and_window, path(deepSympt_path, paste0("windowDefs&VISIT=",visit,"&time.offset=",time.offset,"&window.width=",window.width,".csv")), row.names=FALSE)
        subject_visits_seen <- list()
        for (subject in unique(na.omit(this_endpoint_and_visit$SUBJID))) {
          this_endpoint_visit_window_and_subject <- this_endpoint_visit_and_window[which(this_endpoint_and_visit$SUBJID == subject), ]
          study.date.left <- this_endpoint_visit_window_and_subject$study.date.left[1]
          study.date.right <- this_endpoint_visit_window_and_subject$study.date.right[1]
          CDSD_daily_forthis_subject <- CDSD_daily[which(CDSD_daily$SUBJID == subject), ]
          # Here's where error is introduced
          # path - as a function returns an error
          # nchar(paste0(deepSympt_path, "_details", paste0("_daily&STUDYID=TAK-062-2001&SITE=",str_split(subject, "-")[[1]][1],"&SUBJID=",subject,"&VISIT=",visit,"&time.offset=",time.offset,"&window.width=",window.width,".csv"), sep="/"))
          # 265
          write.csv(CDSD_daily_forthis_subject, path(deepSympt_path, "_details", paste0("_daily&STUDYID=TAK-062-2001&SITE=",str_split(subject, "-")[[1]][1],"&SUBJID=",subject,"&VISIT=",visit,"&time.offset=",time.offset,"&window.width=",window.width,".csv")), row.names=FALSE)
          if ((study.date.left >= min(CDSD_daily_forthis_subject$OBSDATE)) &&
              (study.date.right <= max(CDSD_daily_forthis_subject$OBSDATE))) {
            # we have sufficient data for this window in this subject
            CDSD_daily_forthis_subject <- CDSD_daily_forthis_subject %>%
                                           dplyr::filter(OBSDATE >= study.date.left) %>%
                                           dplyr::filter(OBSDATE <= study.date.right)
            for_analysis_this_subject <- this_endpoint_visit_window_and_subject %>%
              dplyr::mutate(BMs=mean(CDSD_daily_forthis_subject$BMs),
                            Type_6_or_7_BMs=mean(CDSD_daily_forthis_subject$Type_6_or_7_BMs),
                            Times_Vomit=mean(CDSD_daily_forthis_subject$Times_Vomit),
                            AbdPain_Severity=mean(CDSD_daily_forthis_subject$AbdPain_Severity),
                            Bloating_Severity=mean(CDSD_daily_forthis_subject$Bloating_Severity),
                            Diarrhea_Severity=mean(CDSD_daily_forthis_subject$Diarrhea_Severity),
                            Nausea_Severity=mean(CDSD_daily_forthis_subject$Nausea_Severity),
                            Tiredness_Severity=mean(CDSD_daily_forthis_subject$Tiredness_Severity)#,
                            # BMs.gr=coef(lm(CDSD_daily_forthis_subject$BMs~CDSD_daily_forthis_subject$OBSDATE))[2],
                            # Type_6_or_7_BMs.gr=coef(lm(CDSD_daily_forthis_subject$Type_6_or_7_BMs~CDSD_daily_forthis_subject$OBSDATE))[2],
                            # Times_Vomit.gr=coef(lm(CDSD_daily_forthis_subject$Times_Vomit~CDSD_daily_forthis_subject$OBSDATE))[2],
                            # AbdPain_Severity.gr=coef(lm(CDSD_daily_forthis_subject$AbdPain_Severity~CDSD_daily_forthis_subject$OBSDATE))[2],
                            # Bloating_Severity.gr=coef(lm(CDSD_daily_forthis_subject$Bloating_Severity~CDSD_daily_forthis_subject$OBSDATE))[2],
                            # Diarrhea_Severity.gr=coef(lm(CDSD_daily_forthis_subject$Diarrhea_Severity~CDSD_daily_forthis_subject$OBSDATE))[2],
                            # Nausea_Severity.gr=coef(lm(CDSD_daily_forthis_subject$Nausea_Severity~CDSD_daily_forthis_subject$OBSDATE))[2],
                            # Tiredness_Severity.gr=coef(lm(CDSD_daily_forthis_subject$Tiredness_Severity~CDSD_daily_forthis_subject$OBSDATE))[2]
                           )
            # compile an output file with the averages. remove OBSDATE, add time.offset and window.width
            write.csv(for_analysis_this_subject, path(deepSympt_path, "_details", paste0("_avg&STUDYID=TAK-062-2001&SITE=",str_split(subject, "-")[[1]][1],"&SUBJID=",subject,"&VISIT=",visit,"&time.offset=",time.offset,"&window.width=",window.width,".csv")), row.names=FALSE)
            if (paste(subject,visit) %in% subject_visits_seen) {
              print("INADEQUATE SUPPORT FOR STUDYID AND/OR SITE, ADD THIS SUPPORT TO THE LOGIC SO AS NOT TO DUPLICATE SUBJECTIDs")
            } else {
              subject_visits_seen <- append(subject_visits_seen, paste(subject,visit))
              # the next check is admittedly hard-coded for TAK-062-2001, could be generalized if need be as an enhancement
              arm_for_this_subject <- unique((everything_but_CDSD %>% dplyr::filter(SUBJID==subject))$ARM)
              if ((visit=="VISIT 2 - Week -4") || !grepl("^TAK", arm_for_this_subject)) {
                append.csv(for_analysis_this_subject%>%dplyr::select(-c(OBSDATE,study.date.left,study.date.right))%>%dplyr::mutate(time.offset=time.offset,window.width=window.width), path=CDSD_averages)
              }
            }
          } else {
            next
          }
          if (nrow(for_analysis) == 0) {
            for_analysis <- for_analysis_this_subject
          } else {
            for_analysis <- bind_rows(for_analysis, for_analysis_this_subject)
          }
          write.csv(for_analysis, path(deepSympt_path, "for_analysis (for debug).csv"), row.names=FALSE)
        }
      } # for all visits
      if (nrow(for_analysis) > 0) {
        results <- report.correlation(df.in=for_analysis, group1=endpoint, group2=comparators) %>% 
                   dplyr::mutate(Endpoint=endpoint, time.offset=time.offset, window.width=window.width) %>%
                   dplyr::filter(!is.na(Estimate))
        append.csv(results, path=analysis_summary)
      } else {
        cat(" xxx ")
      }
    } # for all window widths
  } # for all time offsets
  print(paste("...concluding", endpoint))
} # for all endpoints


# A function generating colors
cols <- function(n) {
  colorRampPalette(c("#FFC0CB", "#CC0000"))(20)                           # 20 distinct colors
}



comp.cor.report <- report.correlation(df.in=CDSD_daily, group1=comparators, group2=comparators)
lower.triangle <- create.lower.triangle(comp.cor.report)
lower.triangle$row <- colnames(lower.triangle)
lower.triangle <- lower.triangle %>% dplyr::select(row, everything())
for.return <- lower.triangle %>% 
  gt() %>% sub_missing(missing_text="---") %>%
  tab_header(title="Spearman Correlation Matrix") %>%
  fmt_number(columns=everything(), decimals=2) %>% 
  cols_label(row="")
gt::gtsave(for.return, filename=path(table_path, paste0("Spearman.lower.triangle", ".png")))

sweep_results <- read.csv(analysis_summary) %>% 
                 dplyr::select(-X, c()) %>%
                 dplyr::filter(Statistic=="Spearman") %>%
                 dplyr::filter(window.width>10) %>%
                 dplyr::group_by(Endpoint, Comparator, time.offset) %>%
                 dplyr::summarize(Estimate=mean(Estimate)) %>%
                 dplyr::ungroup()
write.csv(sweep_results, path(deepSympt_path, "sweep_results.csv"), row.names=FALSE)

for (endpoint in endpoints) {
  print(paste("beginning", endpoint, "..."))
  cor_figures <- apply(matrix(1:length(comparators)), 1, function(x) {
    for_figure <- sweep_results %>% 
      dplyr::filter(Endpoint==endpoint) %>%
      dplyr::filter(Comparator==comparators[x])
    
    for_figure %>%
      ggplot(aes(x=time.offset, y=Estimate)) +
        geom_point() + scale_color_manual(values=cbp2) +
        geom_smooth(method="lm", formula=y ~ x, se=FALSE) + geom_vline(xintercept=0, linetype="dashed") +
        labs(title=endpoint, x="time.offset", y=paste0("Spearman corr vs. ",label_for_doc(comparators[x])))
    ggsave(filename=path(figure_path, paste0(endpoint, " vs ", comparators[x], ".png")))
  })
}


# # The plot
# cloud(VADeaths, panel.3d.cloud=panel.3dbars, col="white",                 # white borders for bars
#       xbase=1, ybase=1, zlim=c(0, max(VADeaths)),                         # No space around the bars
#       scales=list(arrows=FALSE, just="right"), xlab=NULL, ylab=NULL,
#       col.facet=level.colors(VADeaths, at=do.breaks(range(VADeaths), 20),        
#                                col.regions=cols,                          # color ramp for filling the bars
#                                colors=TRUE),
#       colorkey=list(col=cols, at=do.breaks(range(VADeaths), 20)),
#       screen=list(z=65, x=-65))                                           # Adjust tilting






# results %>% ggplot(aes(x=`Villus Height/Crypt Depth`)) + geom_histogram() + facet_wrap(~Endpoint) +geom_vline(xintercept=0, linetype="dashed")
# results %>% ggplot(aes(x=`CD3 Intraepithelial Lymphocytes Count`)) + geom_histogram() + facet_wrap(~Endpoint) +geom_vline(xintercept=0, linetype="dashed")+scale_x_continuous(limits=c(-1,1.1), breaks=seq(-1,1.1,0.2))

# write.csv(results, "correlation-across-windows-histology.csv")

# pdf("correlation-across-windows-histology.pdf", h=8.5, w=11)
# results %>% dplyr::filter(window.half.width > 1) %>% ggplot(aes(x=window.center, y=`Villus Height/Crypt Depth`, color=window.half.width)) + geom_point(alpha=.5) + facet_wrap(~Endpoint, scales="free_y") + geom_hline(yintercept=0, linetype="dashed") + labs(title="Correlations between Change in VHCD and Symptoms", subtitle="For Symptom windows of varying lengths", x="Window center", y="Correlation coefficient", color="Window half-width", caption="Vertical lines at Weeks 0, 12, 24")+theme(legend.position="bottom")+
#   geom_hline(yintercept=c(-.2,0,.2), linetype="dashed")+geom_vline(xintercept=c(0, 12*7, 24*7), linetype="dashed")

# results %>% dplyr::filter(window.half.width > 7 & window.half.width < 14) %>% ggplot(aes(x=window.center, y=`Villus Height/Crypt Depth`, color=window.half.width)) + geom_point(alpha=.5) + facet_wrap(~Endpoint, scales="free_y") + geom_hline(yintercept=0, linetype="dashed") + labs(title="Correlations between Change in VHCD and Symptoms", subtitle="For Symptom windows of varying lengths", x="Window center", y="Correlation coefficient", color="Window half-width", caption="Vertical lines at Weeks 0, 12, 24")+theme(legend.position="bottom")+
#   geom_hline(yintercept=c(-.2,0,.2), linetype="dashed")+geom_vline(xintercept=c(0, 12*7, 24*7), linetype="dashed")

# results %>% dplyr::filter(window.half.width >= 1 & window.half.width < 7) %>% ggplot(aes(x=window.center, y=`Villus Height/Crypt Depth`, color=window.half.width)) + geom_point(alpha=.5) + facet_wrap(~Endpoint, scales="free_y") + geom_hline(yintercept=0, linetype="dashed") + labs(title="Correlations between Change in VHCD and Symptoms", subtitle="For Symptom windows of varying lengths", x="Window center", y="Correlation coefficient", color="Window half-width", caption="Vertical lines at Weeks 0, 12, 24")+theme(legend.position="bottom")+
#   geom_hline(yintercept=c(-.2,0,.2), linetype="dashed")+geom_vline(xintercept=c(0, 12*7, 24*7), linetype="dashed")


# results %>% dplyr::filter(window.half.width > 1) %>% ggplot(aes(x=window.center, y=`CD3 Intraepithelial Lymphocytes Count`, color=window.half.width)) + geom_point(alpha=.5) + facet_wrap(~Endpoint, scales="free_y") + geom_hline(yintercept=0, linetype="dashed") + labs(title="Correlations between Change in IEL Count and Symptoms", subtitle="For Symptom windows of varying lengths", x="Window center", y="Correlation coefficient", color="Window half-width", caption="Vertical lines at Weeks 0, 12, 24")+theme(legend.position="bottom")+
#   geom_hline(yintercept=c(-.2,0,.2), linetype="dashed")+geom_vline(xintercept=c(0, 12*7, 24*7), linetype="dashed")

# results %>% dplyr::filter(window.half.width > 7 & window.half.width < 14) %>% ggplot(aes(x=window.center, y=`CD3 Intraepithelial Lymphocytes Count`, color=window.half.width)) + geom_point(alpha=.5) + facet_wrap(~Endpoint, scales="free_y") + geom_hline(yintercept=0, linetype="dashed") + labs(title="Correlations between Change in IEL Count and Symptoms", subtitle="For Symptom windows of varying lengths", x="Window center", y="Correlation coefficient", color="Window half-width", caption="Vertical lines at Weeks 0, 12, 24")+theme(legend.position="bottom")+
#   geom_hline(yintercept=c(-.2,0,.2), linetype="dashed")+geom_vline(xintercept=c(0, 12*7, 24*7), linetype="dashed")

# results %>% dplyr::filter(window.half.width >= 1 & window.half.width < 7) %>% ggplot(aes(x=window.center, y=`CD3 Intraepithelial Lymphocytes Count`, color=window.half.width)) + geom_point(alpha=.5) + facet_wrap(~Endpoint, scales="free_y") + geom_hline(yintercept=0, linetype="dashed") + labs(title="Correlations between Change in IEL Count and Symptoms", subtitle="For Symptom windows of varying lengths", x="Window center", y="Correlation coefficient", color="Window half-width", caption="Vertical lines at Weeks 0, 12, 24")+theme(legend.position="bottom")+
#   geom_hline(yintercept=c(-.2,0,.2), linetype="dashed")+geom_vline(xintercept=c(0, 12*7, 24*7), linetype="dashed")
# dev.off()

df.in <- read_csv(path(output_dir,"5a Merged_dataBeforePairings.csv"))


# Start with: form_merged_data_sets_output$global_analysis_set_out built from Phase0----

reader_type="ML"
this_endpoint="BVA"
# Prepare the data for the time course plots----
 
process_data <- function(df, obsdate_col, select_cols, pivot_cols) {
  df %>%
    dplyr::filter(!is.na(ENRDATE)) %>%
    dplyr::select(STUDYID, SITE, SUBJID, ARM, VISIT, !!sym(obsdate_col), ENRDATE, all_of(select_cols)) %>%
    group_by(STUDYID, SITE, SUBJID) %>%
    dplyr::mutate(
      key = paste(STUDYID, SITE, SUBJID),
      elapsed.time = as.numeric(difftime(!!sym(obsdate_col), ENRDATE, units = "days")),
      n.visits = n(),
      n = 1:n()
    ) %>%
    dplyr::arrange(key, !!sym(obsdate_col)) %>%
    dplyr::rename(OBSDATE = !!sym(obsdate_col)) %>%
    pivot_longer(cols = all_of(pivot_cols), names_to = "measure", values_to = "value") %>%
    dplyr::mutate(OBSDATE = ymd(OBSDATE)) %>%
    dplyr::filter(!is.na(value), !is.na(OBSDATE))
}

# Using the generalized function for temp.vce
temp.vce <- process_data(
  df = df.in,
  obsdate_col = "OBSDATE.VCE",
  select_cols = this_endpoint,
  pivot_cols = this_endpoint)

# Using the generalized function for temp.hist
temp.hist <- process_data(
  df = df.in,
  obsdate_col = "OBSDATE.histology",
  select_cols = c("VHCD", "IELCOUNT"),
  pivot_cols = c("VHCD", "IELCOUNT"))

# Using the generalized function for temp.symp
temp.symp <- process_data(
  df = df.in,
  obsdate_col = "OBSDATE.symptoms",
  select_cols = "GISS",
  pivot_cols = "GISS")

temp <- bind_rows(temp.vce, temp.hist, temp.symp) %>% dplyr::filter(is.na(OBSDATE)==FALSE)

  # Standardize scores
temp <- temp %>% 
    group_by(measure) %>%
    dplyr::mutate(value=ifelse(measure == "VHCD", -value, value),
                  value=(value - mean(value, na.rm=TRUE)) / (sd(value, na.rm=TRUE))) %>% 
    ungroup() %>% dplyr::filter(is.na(value) == FALSE) %>%
    group_by(key, measure) 
  
measures <- c(this_endpoint, "VHCD", "GISS", "IELCOUNT")
  
get.results <- function(measure.in="GISS", key.in="TAK-062-2001 18003 18003-006"){
    subject.df <- temp %>% dplyr::filter(key == key.in, measure == measure.in) 
    
    tryCatch({
      loess_fit <- loess(value ~ elapsed.time, data = subject.df, span = 1)
      
      time_seq <- seq(
        min(subject.df$elapsed.time, na.rm = TRUE), 
        max(subject.df$elapsed.time, na.rm = TRUE), 
        length.out = 300
      )
      
      loess_estimates <- predict(loess_fit, newdata = data.frame(elapsed.time = time_seq), se = FALSE)
      
      loess.out <- data.frame(x = time_seq, loess = loess_estimates)
      loess.out <- loess.out %>% dplyr::mutate(key = key.in, measure = measure.in, ARM=subject.df$ARM[1])
      
      max.points <- findpeaks(loess.out$loess, npeaks = 3)
      min.points <- findpeaks(-loess.out$loess, npeaks = 3)
      
      if (is.null(max.points)) {
        for.return.max <- data.frame(
          feature = "Max", 
          measure = measure.in, 
          key = key.in,
          location = NA, 
          value = NA
        )
      } else {
        for.return.max <- data.frame(
          key = key.in,
          feature = "Max", 
          measure = measure.in, 
          location = loess.out$x[max.points[, 2]], 
          value = max.points[, 1]
        )
      }
      
      if (is.null(min.points)) {
        for.return.min <- data.frame(
          feature = "Min", 
          key = key.in,
          measure = measure.in,
          location = NA, 
          value = NA
        )
      } else {
        for.return.min <- data.frame(
          key = key.in,
          feature = "Min", 
          measure = measure.in, 
          location = loess.out$x[min.points[, 2]], 
          value = -min.points[, 1]
        )
      }
      
      list(max = for.return.max, min = for.return.min)
    }, error = function(e) {
      NULL
    })
    list(loess.out, bind_rows(for.return.max, for.return.min) %>% dplyr::mutate(ARM=subject.df$ARM[1]))
  }
  
my.grid <- expand.grid(measures, key.in=unique(temp$key))

my.grid <- my.grid %>% dplyr::filter(Var1 == "GISS")

 loess.results <-  apply(matrix(1:nrow(my.grid)), 1, function(x) {
    get.results(measure.in=my.grid[x,1], key.in=my.grid[x,2])[[1]]
  }) %>% bind_rows()
  
 maxmin.results <- apply(matrix(1:nrow(my.grid)), 1, function(x) {
    get.results(measure.in=my.grid[x,1], key.in=my.grid[x,2])[[2]]
  }) %>% bind_rows()
  
  
 # This output is just for GISS-------------------
 
 loess.results %>% dplyr::filter(is.na(ARM) == FALSE) %>% ggplot(aes(x=x, y=loess, group=key, color=measure)) +
   geom_line() +
   geom_point(data=temp %>% dplyr::filter(key %in% loess.results$key), aes(x=elapsed.time, y=value, group=key)) +
   facet_wrap(ARM~measure, scales="free_y", nrow=2) +
   theme(legend.position="bottom")
 
 
 loess.results %>% dplyr::filter(is.na(ARM) == FALSE, measure=="GISS") %>% ggplot(aes(x=x, y=loess, group=key, color=measure)) +
   geom_line() +
   geom_point(data=temp %>% dplyr::filter(key %in% loess.results$key, measure=="GISS"), aes(x=elapsed.time, y=value, group=key)) +
   facet_wrap(ARM~key, scales="free_y", nrow=2) +
   theme(legend.position="bottom")+
   geom_point(data=maxmin.results %>% dplyr::filter(measure=="GISS"), aes(x=location, y=value, color=feature), size=3)
 

# Now going for the full complement of symptoms-------------------

 temp.symp <- process_data(
   df = df.in,
   obsdate_col = "OBSDATE.symptoms",
   select_cols = names(df.in)[29:49],
   pivot_cols = names(df.in)[29:49])
 
temp <- temp.symp
my.measures <- names(df.in)[29:49]
 my.grid <- expand.grid(my.measures, key.in=unique(temp$key))
 
 loess.results <-  apply(matrix(1:nrow(my.grid)), 1, function(x) {
   get.results(measure.in=my.grid[x,1], key.in=my.grid[x,2])[[1]]
 }) %>% bind_rows()
 
 maxmin.results <- apply(matrix(1:nrow(my.grid)), 1, function(x) {
   get.results(measure.in=my.grid[x,1], key.in=my.grid[x,2])[[2]]
 }) %>% bind_rows()
 
 important.measures <- unique(loess.results$measure)[c(2,4,6,8,10,12)]
 
 pdf("timecourseplots.pdf", width=14, h=8)
 for(i in 1:length(unique(loess.results$measure))){
 p <- loess.results %>% dplyr::filter(is.na(ARM) == FALSE, measure==unique(loess.results$measure)[i]) %>% ggplot(aes(x=x, y=loess, group=key, color=measure)) +
   geom_line() +
   geom_point(data=temp %>% dplyr::filter(measure==unique(loess.results$measure)[i]), aes(x=elapsed.time, y=value, group=key)) +
   facet_wrap(ARM~key, nrow=2) +
   theme(legend.position="bottom")+
   geom_point(data=maxmin.results %>% dplyr::filter(measure==unique(loess.results$measure)[i]), aes(x=location, y=value, color=feature), size=3)+
   labs(title=paste("Time Course plots for ", unique(loess.results$measure)[i]))
 plot(p)
 }
 dev.off()

 pdf("timecourseplots2.pdf", width=14, h=8)
 for(i in 1:length(important.measures)){
   p <- loess.results %>% dplyr::filter(is.na(ARM) == FALSE, measure==important.measures[i]) %>% ggplot(aes(x=x, y=loess, group=key, color=measure)) +
     geom_line() +
     geom_point(data=temp %>% dplyr::filter(measure==important.measures[i]), aes(x=elapsed.time, y=value, group=key)) +
     facet_wrap(ARM~key, nrow=2) +
     theme(legend.position="bottom")+
     geom_point(data=maxmin.results %>% dplyr::filter(measure==important.measures[i]), aes(x=location, y=value, color=feature), size=3)+
     labs(title=paste("Time Course plots for ", important.measures[i]))
   plot(p)
 }
 dev.off()
 
 
# Now going for the full complement of symptoms per patient------------
 
 loess.results <- loess.results %>% dplyr::mutate(
   class = case_when(measure %in% c("Had_Diarrhea", "Freq_Diarrhea", "Diarrhea_SS") ~  "Diarrhea",
                     measure %in% c("Had_CSBM", "Freq_CSBM", "CSBM") ~ "CSBM",
                     measure %in% c("Had_Ab_Pain", "Worst_Ab_Pain","AB_Pain_SS") ~ "Ab Pain",
                     measure %in% c("Had_Bloating", "Worst_Bloating", "Bloating_SS") ~ "Bloating", 
                     measure %in% c("Had_Nausea","Worst_Nausea", "Nausea_SS") ~ "Nausea",
                     measure %in% c("Had_Tiredness", "Worst_Tiredness", "Tiredness_SS") ~ "Tiredness",
                     measure %in% c("GISS", "NSGISS", "TotalScore") ~ "Aggregate"),
   linetype = case_when(measure %in% c("Had_Diarrhea","Had_CSBM","Had_Ab_Pain","Had_Bloating","Had_Nausea","Had_Tiredness" ) ~ "Had it",
                        measure %in% c("Freq_Diarrhea", "Freq_CSBM") ~ "Freq",
                        measure %in% c("Worst_Ab_Pain","Worst_Bloating", "Worst_Nausea", "Worst_Tiredness") ~ "Worst",
                        measure %in% c("Diarrhea_SS","CSBM","AB_Pain_SS", "Bloating_SS", "Nausea_SS", "Tiredness_SS", "GISS", "NSGISS", "TotalScore") ~ "SS"
   ))
 
 maxmin.results <- maxmin.results%>% dplyr::mutate(
   class = case_when(measure %in% c("Had_Diarrhea", "Freq_Diarrhea", "Diarrhea_SS") ~  "Diarrhea",
                     measure %in% c("Had_CSBM", "Freq_CSBM", "CSBM") ~ "CSBM",
                     measure %in% c("Had_Ab_Pain", "Worst_Ab_Pain","AB_Pain_SS") ~ "Ab Pain",
                     measure %in% c("Had_Bloating", "Worst_Bloating", "Bloating_SS") ~ "Bloating", 
                     measure %in% c("Had_Nausea","Worst_Nausea", "Nausea_SS") ~ "Nausea",
                     measure %in% c("Had_Tiredness", "Worst_Tiredness", "Tiredness_SS") ~ "Tiredness",
                     measure %in% c("GISS", "NSGISS", "TotalScore") ~ "Aggregate"),
   linetype = case_when(measure %in% c("Had_Diarrhea","Had_CSBM","Had_Ab_Pain","Had_Bloating","Had_Nausea","Had_Tiredness" ) ~ "Had it",
                        measure %in% c("Freq_Diarrhea", "Freq_CSBM") ~ "Freq",
                        measure %in% c("Worst_Ab_Pain","Worst_Bloating", "Worst_Nausea", "Worst_Tiredness") ~ "Worst",
                        measure %in% c("Diarrhea_SS","CSBM","AB_Pain_SS", "Bloating_SS", "Nausea_SS", "Tiredness_SS", "GISS", "NSGISS", "TotalScore") ~ "SS"
   ))
 


temp <- temp %>% dplyr::mutate(
  class = case_when(measure %in% c("Had_Diarrhea", "Freq_Diarrhea", "Diarrhea_SS") ~  "Diarrhea",
                    measure %in% c("Had_CSBM", "Freq_CSBM", "CSBM") ~ "CSBM",
                    measure %in% c("Had_Ab_Pain", "Worst_Ab_Pain","AB_Pain_SS") ~ "Ab Pain",
                    measure %in% c("Had_Bloating", "Worst_Bloating", "Bloating_SS") ~ "Bloating", 
                    measure %in% c("Had_Nausea","Worst_Nausea", "Nausea_SS") ~ "Nausea",
                    measure %in% c("Had_Tiredness", "Worst_Tiredness", "Tiredness_SS") ~ "Tiredness",
                    measure %in% c("GISS", "NSGISS", "TotalScore") ~ "Aggregate"),
  linetype = case_when(measure %in% c("Had_Diarrhea","Had_CSBM","Had_Ab_Pain","Had_Bloating","Had_Nausea","Had_Tiredness" ) ~ "Had it",
                       measure %in% c("Freq_Diarrhea", "Freq_CSBM") ~ "Freq",
                       measure %in% c("Worst_Ab_Pain","Worst_Bloating", "Worst_Nausea", "Worst_Tiredness") ~ "Worst",
                       measure %in% c("Diarrhea_SS","CSBM","AB_Pain_SS", "Bloating_SS", "Nausea_SS", "Tiredness_SS", "GISS", "NSGISS", "TotalScore") ~ "SS"
  ))
loess.results$key <- paste(loess.results$key, loess.results$ARM)
maxmin.results$key <- paste(maxmin.results$key, maxmin.results$ARM)
temp$key <- paste(temp$key, temp$ARM)
pdf("patient-plots.pdf", h=8.5, 11)
for(i in 1:14){
fig.out <- loess.results %>% dplyr::filter(key==unique(loess.results$key)[i]) %>% dplyr::filter(linetype != "SS", class!= "Aggregate") %>%
   ggplot(aes(x=x, y=loess, group=measure, color=factor(linetype))) +
   geom_line(size=1) +
  geom_point(data=temp %>% dplyr::filter(key==unique(loess.results$key)[i], class !="Aggregate",linetype != "SS"), aes(x=elapsed.time, y=value, group=key,color=factor(linetype))) +
   facet_grid(class~key, scales="free_y")+
  geom_point(data=maxmin.results %>% dplyr::filter(key==unique(loess.results$key)[i], class !="Aggregate"), aes(x=location, y=value, shape=feature), size=2, color="black") +
  
  theme(legend.position = "bottom") + scale_x_continuous(limits=c(-35,35), breaks=seq(-35,35,7)) 
plot(fig.out)
}
dev.off()
   

# REmoving had it


loess.results <- loess.results %>% dplyr::mutate(
  class = case_when(measure %in% c("Had_Diarrhea", "Freq_Diarrhea", "Diarrhea_SS") ~  "Diarrhea",
                    measure %in% c("Had_CSBM", "Freq_CSBM", "CSBM") ~ "CSBM",
                    measure %in% c("Had_Ab_Pain", "Worst_Ab_Pain","AB_Pain_SS") ~ "Ab Pain",
                    measure %in% c("Had_Bloating", "Worst_Bloating", "Bloating_SS") ~ "Bloating", 
                    measure %in% c("Had_Nausea","Worst_Nausea", "Nausea_SS") ~ "Nausea",
                    measure %in% c("Had_Tiredness", "Worst_Tiredness", "Tiredness_SS") ~ "Tiredness",
                    measure %in% c("GISS", "NSGISS", "TotalScore") ~ "Aggregate"),
  linetype = case_when(measure %in% c("Had_Diarrhea","Had_CSBM","Had_Ab_Pain","Had_Bloating","Had_Nausea","Had_Tiredness" ) ~ "Had it",
                       measure %in% c("Freq_Diarrhea", "Freq_CSBM") ~ "Freq",
                       measure %in% c("Worst_Ab_Pain","Worst_Bloating", "Worst_Nausea", "Worst_Tiredness") ~ "Worst",
                       measure %in% c("Diarrhea_SS","CSBM","AB_Pain_SS", "Bloating_SS", "Nausea_SS", "Tiredness_SS", "GISS", "NSGISS", "TotalScore") ~ "SS"
  ))

maxmin.results <- maxmin.results%>% dplyr::mutate(
  class = case_when(measure %in% c("Had_Diarrhea", "Freq_Diarrhea", "Diarrhea_SS") ~  "Diarrhea",
                    measure %in% c("Had_CSBM", "Freq_CSBM", "CSBM") ~ "CSBM",
                    measure %in% c("Had_Ab_Pain", "Worst_Ab_Pain","AB_Pain_SS") ~ "Ab Pain",
                    measure %in% c("Had_Bloating", "Worst_Bloating", "Bloating_SS") ~ "Bloating", 
                    measure %in% c("Had_Nausea","Worst_Nausea", "Nausea_SS") ~ "Nausea",
                    measure %in% c("Had_Tiredness", "Worst_Tiredness", "Tiredness_SS") ~ "Tiredness",
                    measure %in% c("GISS", "NSGISS", "TotalScore") ~ "Aggregate"),
  linetype = case_when(measure %in% c("Had_Diarrhea","Had_CSBM","Had_Ab_Pain","Had_Bloating","Had_Nausea","Had_Tiredness" ) ~ "Had it",
                       measure %in% c("Freq_Diarrhea", "Freq_CSBM") ~ "Freq",
                       measure %in% c("Worst_Ab_Pain","Worst_Bloating", "Worst_Nausea", "Worst_Tiredness") ~ "Worst",
                       measure %in% c("Diarrhea_SS","CSBM","AB_Pain_SS", "Bloating_SS", "Nausea_SS", "Tiredness_SS", "GISS", "NSGISS", "TotalScore") ~ "SS"
  ))



temp <- temp %>% dplyr::mutate(
  class = case_when(measure %in% c("Had_Diarrhea", "Freq_Diarrhea", "Diarrhea_SS") ~  "Diarrhea",
                    measure %in% c("Had_CSBM", "Freq_CSBM", "CSBM") ~ "CSBM",
                    measure %in% c("Had_Ab_Pain", "Worst_Ab_Pain","AB_Pain_SS") ~ "Ab Pain",
                    measure %in% c("Had_Bloating", "Worst_Bloating", "Bloating_SS") ~ "Bloating", 
                    measure %in% c("Had_Nausea","Worst_Nausea", "Nausea_SS") ~ "Nausea",
                    measure %in% c("Had_Tiredness", "Worst_Tiredness", "Tiredness_SS") ~ "Tiredness",
                    measure %in% c("GISS", "NSGISS", "TotalScore") ~ "Aggregate"),
  linetype = case_when(measure %in% c("Had_Diarrhea","Had_CSBM","Had_Ab_Pain","Had_Bloating","Had_Nausea","Had_Tiredness" ) ~ "Had it",
                       measure %in% c("Freq_Diarrhea", "Freq_CSBM") ~ "Freq",
                       measure %in% c("Worst_Ab_Pain","Worst_Bloating", "Worst_Nausea", "Worst_Tiredness") ~ "Worst",
                       measure %in% c("Diarrhea_SS","CSBM","AB_Pain_SS", "Bloating_SS", "Nausea_SS", "Tiredness_SS", "GISS", "NSGISS", "TotalScore") ~ "SS"
  ))
loess.results$key <- paste(loess.results$key, loess.results$ARM)
maxmin.results$key <- paste(maxmin.results$key, maxmin.results$ARM)
temp$key <- paste(temp$key, temp$ARM)
pdf("patient-plots2.pdf", h=8.5, 11)
for(i in 1:14){
  fig.out <- loess.results %>% dplyr::filter(key==unique(loess.results$key)[i]) %>% dplyr::filter(linetype != "SS", class!= "Aggregate") %>%
    dplyr::filter(!(grepl(measure, pattern="Had"))) %>% 
    ggplot(aes(x=x, y=loess, group=measure, color=factor(linetype))) +
    geom_line(size=1) +
    geom_point(data=temp %>% dplyr::filter(key==unique(loess.results$key)[i], class !="Aggregate",linetype != "SS") %>%  dplyr::filter(!(grepl(measure, pattern="Had"))), aes(x=elapsed.time, y=value, group=key,color=factor(linetype))) +
    facet_wrap(class~key, scales="free_y",ncol=2)+
    geom_point(data=maxmin.results %>% dplyr::filter(key==unique(loess.results$key)[i], class !="Aggregate") %>% dplyr::filter(!(grepl(measure, pattern="Had"))), aes(x=location, y=value, shape=feature), size=2, color="black") +
    theme(legend.position = "bottom") + scale_x_continuous(limits=c(-35,35), breaks=seq(-35,35,7)) 
  plot(fig.out)
}
dev.off()












   
 #-------------


get.results(measure.in=my.measures[1], key.in="TIMP-GLIA-5001 1010 7007")
 
get.Symp.plot <- function(symp.endpoint = "Had_Ab_Pain"){

temp.symp <-  df.in %>% 
  dplyr::filter(is.na(ENRDATE) == FALSE) %>% # filter those who do not have enrollment date
  dplyr::select(STUDYID, SITE, SUBJID, ARM, VISIT, OBSDATE.symptoms, ENRDATE, symp.endpoint, IELCOUNT) %>% 
  dplyr::group_by(STUDYID, SITE, SUBJID) %>% 
  dplyr::mutate(
    key=paste(STUDYID, SITE, SUBJID),
    # Adjust for randomization date
    elapsed.time = as.numeric(difftime(OBSDATE.symptoms, ENRDATE, units = "days")),
    n.visits=n(),
    n=1:n()) %>%
  arrange(key, OBSDATE.symptoms) %>%
  dplyr::rename(OBSDATE=OBSDATE.symptoms) %>%
  pivot_longer(cols=c(symp.endpoint), names_to="measure", values_to="value") 


temp <- temp.symp

# Standardize scores
temp <- temp %>% 
  group_by(measure) %>%
  dplyr::mutate(value=ifelse(measure == "VHCD", -value, value),
                value=(value - mean(value, na.rm=TRUE)) / (sd(value, na.rm=TRUE))) %>% 
  ungroup() %>% dplyr::filter(is.na(value) == FALSE) %>%
  group_by(key, measure) 

measures <- c(symp.endpoint)
for.plot <- apply(matrix(1:length(measures)), 1, function(y) {
  apply(matrix(1:length(unique(temp$key))), 1, function(x) {
    subject.df <- temp %>% dplyr::filter(key == unique(temp$key)[x], measure == measures[y]) 
    # Subset to those with more than one visit
    if(nrow(subject.df) > 1) {
      loess.out <- tryCatch({
        spline.df.fmm <- data.frame(spline(subject.df$elapsed.time, subject.df$value, method="fmm", n=30)) %>% dplyr::rename("fmm"=y)
        spline.df.natural <- data.frame(spline(subject.df$elapsed.time, subject.df$value, method="natural", n=30)) %>% dplyr::rename("natural"=y)
        loess_fit <- loess(value ~ elapsed.time, data=subject.df, span=1)
        time_seq <- seq(min(subject.df$elapsed.time, na.rm=TRUE), 
                        max(subject.df$elapsed.time, na.rm=TRUE), 
                        length.out=30)
        loess_estimates <- predict(loess_fit, newdata=data.frame(elapsed.time=time_seq), se=FALSE)
        loess.out <- data.frame(x=time_seq, loess=loess_estimates)
        loess.out
      }, error=function(e) {
        data.frame(x=NA, loess=NA)
      })
      for.key <- unique(temp$key)[x]
      for.measure <- measures[y]
      df.out <- spline.df.fmm %>% left_join(spline.df.natural) %>% left_join(loess.out) %>% 
        dplyr::mutate(key=for.key, measure=for.measure)
    }
    else{return(NULL)}
  })
}) %>% bind_rows()

for.plot <- for.plot %>% 
  left_join(temp %>% ungroup() %>% dplyr::select(key, ARM), by=c("key")) %>% 
  group_by(key, measure) %>% 
  dplyr::filter(is.na(ARM)==FALSE) %>% ungroup() %>%
  dplyr::filter(is.na(loess)==FALSE)

pdf.out <- for.plot %>%
  group_by(key, measure) %>%
  ggplot(aes(x=x, y=fmm, group=key, color=ARM)) +
  labs(title=paste0("Time Course Plots for ", this_endpoint, " and comparators"),
       x="Days Elapsed", y="Measure", 
       caption="VHCD has been scaled so that higher scores are associated with disease severity") +
  geom_line(aes(x=x, y=loess)) + scale_colour_manual(values=cbp2) +
  facet_wrap(ARM ~ measure, nrow=2) +
  geom_point(data=temp %>% dplyr::filter(key %in% for.plot$key), aes(x=elapsed.time, y=value, group=key)) +
  theme(legend.position="bottom")

pdf.out2 <- for.plot %>% dplyr::filter(measure==symp.endpoint) %>%
  group_by(key, measure) %>%
  ggplot(aes(x=x, y=fmm, group=key, color=key)) +
  labs(title=paste0("Time Course Plots for ",symp.endpoint),
       x="Days Elapsed", y="Measure") +
  geom_line(aes(x=x, y=loess)) +
  facet_wrap(ARM ~ key, nrow=2) +
  geom_point(data=temp %>% dplyr::filter(measure==symp.endpoint, key %in% for.plot$key), aes(x=elapsed.time, y=value, group=key)) +
  geom_line(data=temp %>% dplyr::filter(measure==symp.endpoint,key %in% for.plot$key), aes(x=elapsed.time, y=value, group=key), linetype=2) +
  theme(legend.position="bottom")

return(pdf.out2)
}


get.Symp.plot2 <- function(symp.endpoint = "Had_Ab_Pain"){
  
  temp.symp <-  df.in %>% 
    dplyr::filter(is.na(ENRDATE) == FALSE) %>% # filter those who do not have enrollment date
    dplyr::select(STUDYID, SITE, SUBJID, ARM, VISIT, OBSDATE.symptoms, ENRDATE, symp.endpoint, IELCOUNT) %>% 
    dplyr::group_by(STUDYID, SITE, SUBJID) %>% 
    dplyr::mutate(
      key=paste(STUDYID, SITE, SUBJID),
      # Adjust for randomization date
      elapsed.time = as.numeric(difftime(OBSDATE.symptoms, ENRDATE, units = "days")),
      n.visits=n(),
      n=1:n()) %>%
    arrange(key, OBSDATE.symptoms) %>%
    dplyr::rename(OBSDATE=OBSDATE.symptoms) %>%
    pivot_longer(cols=c(symp.endpoint), names_to="measure", values_to="value") 
  
  
  temp <- temp.symp
  
  # Standardize scores
  temp <- temp %>% 
    group_by(measure) %>%
    # dplyr::mutate(value=ifelse(measure == "VHCD", -value, value),
    #               value=(value - mean(value, na.rm=TRUE)) / (sd(value, na.rm=TRUE))) %>% 
    ungroup() %>% dplyr::filter(is.na(value) == FALSE) %>%
    group_by(key, measure) 
  
  measures <- c(symp.endpoint)
  for.plot <- apply(matrix(1:length(measures)), 1, function(y) {
    apply(matrix(1:length(unique(temp$key))), 1, function(x) {
      subject.df <- temp %>% dplyr::filter(key == unique(temp$key)[x], measure == measures[y]) 
      # Subset to those with more than one visit
      if(nrow(subject.df) > 1) {
        loess.out <- tryCatch({
          spline.df.fmm <- data.frame(spline(subject.df$elapsed.time, subject.df$value, method="fmm", n=30)) %>% dplyr::rename("fmm"=y)
          spline.df.natural <- data.frame(spline(subject.df$elapsed.time, subject.df$value, method="natural", n=30)) %>% dplyr::rename("natural"=y)
          loess_fit <- loess(value ~ elapsed.time, data=subject.df, span=1)
          time_seq <- seq(min(subject.df$elapsed.time, na.rm=TRUE), 
                          max(subject.df$elapsed.time, na.rm=TRUE), 
                          length.out=30)
          loess_estimates <- predict(loess_fit, newdata=data.frame(elapsed.time=time_seq), se=FALSE)
          loess.out <- data.frame(x=time_seq, loess=loess_estimates)
          loess.out
        }, error=function(e) {
          data.frame(x=NA, loess=NA)
        })
        for.key <- unique(temp$key)[x]
        for.measure <- measures[y]
        df.out <- spline.df.fmm %>% left_join(spline.df.natural) %>% left_join(loess.out) %>% 
          dplyr::mutate(key=for.key, measure=for.measure)
      }
      else{return(NULL)}
    })
  }) %>% bind_rows()
  
  for.plot <- for.plot %>% 
    left_join(temp %>% ungroup() %>% dplyr::select(key, ARM), by=c("key")) %>% 
    group_by(key, measure) %>% 
    dplyr::filter(is.na(ARM)==FALSE) %>% ungroup() %>%
    dplyr::filter(is.na(loess)==FALSE)
  
  pdf.out <- for.plot %>%
    group_by(key, measure) %>%
    ggplot(aes(x=x, y=fmm, group=key, color=ARM)) +
    labs(title=paste0("Time Course Plots for ", this_endpoint, " and comparators"),
         x="Days Elapsed", y="Measure", 
         caption="VHCD has been scaled so that higher scores are associated with disease severity") +
    geom_line(aes(x=x, y=loess)) + scale_colour_manual(values=cbp2) +
    facet_wrap(ARM ~ measure, nrow=2) +
    geom_point(data=temp %>% dplyr::filter(key %in% for.plot$key), aes(x=elapsed.time, y=value, group=key)) +
    theme(legend.position="bottom")
  
  pdf.out2 <- for.plot %>% dplyr::filter(measure==symp.endpoint) %>%
    group_by(key, measure) %>%
    ggplot(aes(x=x, y=fmm, group=key, color=key)) +
    labs(title=paste0("Time Course Plots for ",symp.endpoint),
         x="Days Elapsed", y="Measure") +
    geom_line(aes(x=x, y=loess)) +
    facet_wrap(ARM ~ key, nrow=2) +
    geom_point(data=temp %>% dplyr::filter(measure==symp.endpoint, key %in% for.plot$key), aes(x=elapsed.time, y=value, group=key)) +
    geom_line(data=temp %>% dplyr::filter(measure==symp.endpoint,key %in% for.plot$key), aes(x=elapsed.time, y=value, group=key), linetype=2) +
    theme(legend.position="bottom")
  
  return(pdf.out2)
}

pdf(file="Phase0-symptoms.pdf", width = 14, heigh=8.5)
for(i in 29:49){
p<- get.Symp.plot(symp.endpoint = names(df.in)[i])
plot(p)
}
dev.off()



pdf(file="Phase0-symptoms2.pdf", width = 14, heigh=8.5)
for(i in 29:49){
  p<- get.Symp.plot2(symp.endpoint = names(df.in)[i])
  plot(p)
}
dev.off()






#-------------
df.in <- read_csv(path(output_dir,"5a Merged_dataBeforePairings.csv"))

names(temp)
temp <- df.in %>% dplyr::select(
                                "Freq_Diarrhea",
                                "Freq_CSBM",
                                "Worst_Ab_Pain",
                                "Worst_Bloating",
                                "Worst_Nausea",
                                "Worst_Tiredness")
temp <- temp %>%filter_all(all_vars(!is.na(.)))
temp <- temp %>% mutate(across(where(is.numeric), scale))

pca_results <- prcomp(x=as.matrix(temp))
pca_results$rotation %>% round(., 2)
summary(pca_results)

# Extract PCA scores and loadings
scores <- as.data.frame(pca_results$x)
loadings <- as.data.frame(pca_results$rotation)

# Rowsums
rowSums(loadings)

loadings.fig <- loadings %>% dplyr::mutate(variable=factor(rownames(loadings), rownames(loadings))) %>% pivot_longer(contains("PC")) %>%
  dplyr::filter(name %in% c("PC1", "PC2", "PC3")) %>%
  ggplot(aes(x=variable, y=value, group=name, color=name)) +
  geom_point() +geom_line()
 
# Add row names as a new column to loadings
loadings$Variable <- rownames(loadings)

# Add a column to identify score points
scores$Variable <- rownames(scores)

# Create biplot data
biplot_scores <- data.frame(scores, Type = "Score")
biplot_loadings <- data.frame(loadings, Type = "Loading")

# Combine dataframes
biplot_df <- rbind(biplot_scores, biplot_loadings)

# Create the biplot
ggplot() +
  geom_point(data = subset(biplot_df, Type == "Score"), aes(x = PC1, y = PC2), alpha = 0.7, color = "blue") +
  geom_segment(data = subset(biplot_df, Type == "Loading"), aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2,"cm")), alpha = 0.9, color = "red") +
  geom_text_repel(data = subset(biplot_df, Type == "Loading"), aes(x = PC1, y = PC2, label = Variable),
                  size = 4, color = "black", segment.color = 'grey') +
  labs(title = 'Biplot', 
       x = paste0('PC1 (', round(summary(pca_results)$importance[2,1] * 100, 2), '% variance)'), 
       y = paste0('PC2 (', round(summary(pca_results)$importance[2,2] * 100, 2), '% variance)')) +
  theme_minimal()
#--------------

names(df.in)
temp <- df.in %>% dplyr::mutate(key=paste(STUDYID, SITE, SUBJID)) 
# temp %>% dplyr::select("AB_Pain_SS", "Bloating_SS", "CSBM", "Diarrhea_SS", "Nausea_SS", "Tiredness_SS", "GISS", "NSGISS")
temp <- temp %>% dplyr::select(key, "VHCD", "IELCOUNT", "GISS") %>% group_by(key) %>% slice(1)
temp <- temp %>%filter_all(all_vars(!is.na(.)))
temp <- temp %>% mutate(across(where(is.numeric), scale))
cor(temp[,-1])
pca_results <- prcomp(x=as.matrix(temp))
pca_results$rotation %>% round(., 2)
summary(pca_results)

# Extract PCA scores and loadings
scores <- as.data.frame(pca_results$x)
loadings <- as.data.frame(pca_results$rotation)

# Add row names as a new column to loadings
loadings$Variable <- rownames(loadings)

# Add a column to identify score points
scores$Variable <- rownames(scores)

# Create biplot data
biplot_scores <- data.frame(scores, Type = "Score")
biplot_loadings <- data.frame(loadings, Type = "Loading")

# Combine dataframes
biplot_df <- rbind(biplot_scores, biplot_loadings)

# Create the biplot
ggplot() +
  geom_point(data = subset(biplot_df, Type == "Score"), aes(x = PC1, y = PC2), alpha = 0.7, color = "blue") +
  geom_segment(data = subset(biplot_df, Type == "Loading"), aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2,"cm")), alpha = 0.9, color = "red") +
  geom_text_repel(data = subset(biplot_df, Type == "Loading"), aes(x = PC1, y = PC2, label = Variable),
                  size = 4, color = "black", segment.color = 'grey') +
  labs(title = 'Biplot', 
       x = paste0('PC1 (', round(summary(pca_results)$importance[2,1] * 100, 2), '% variance)'), 
       y = paste0('PC2 (', round(summary(pca_results)$importance[2,2] * 100, 2), '% variance)')) +
  theme_minimal()

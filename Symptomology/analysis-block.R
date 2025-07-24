main_analysis <- (analysis.data.in=analysis.data.in.ML, CDSD.in = CDSD){
  for.return <- list()
  analysis.data.in$`key` <- paste0(analysis.data.in$`STUDYID`,"-", analysis.data.in$SUBJID)
  CDSD.in <-  CDSD.in %>% dplyr::rename(key=`Unique Subject Identifier`)
  # Single to noise ratio----
  temp <- CDSD.in %>%
    dplyr::select(`key`, `Study Day of Finding`, `Question Name`, `Numeric Finding in Standard Units`, `Epoch`) %>%
    pivot_wider(names_from = `Question Name`, values_from = `Numeric Finding in Standard Units`, values_fn = mean) %>%
    dplyr::mutate(GISS = (`CDS1-How Severe Was Your Diarrhea` + `CDS1-How Severe Was Your Abdominal Pain` + `CDS1-How Severe Was Your Bloating` + `CDS1-How Severe Was Your Nausea`) / 4)
  
  temp <- temp %>%
    pivot_longer(cols = -c(1:3)) %>%
    dplyr::rename(`Question Name` = name)
  
  temp %>%
    group_by(`Question Name`) %>%
    dplyr::summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE)) %>%
    dplyr::mutate(snr = mean / sd) %>%
    arrange(desc(snr)) -> snr_long
  
  stability_stats <- temp %>%
    group_by(`Question Name`, `key`) %>%
    arrange(`key`, `Study Day of Finding`) %>%
    mutate(diff = c(NA, diff(value))) %>%
    group_by(`Question Name`) %>%
    dplyr::summarise(
      sd_diff = sd(diff, na.rm = TRUE),
      cv = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE)
    ) %>%
    arrange((cv))
  
  for.return$stability_stats <- stability_stats %>% left_join(snr_long) %>% dplyr::select(`Question Name`, mean, sd, cv, snr)
  
  #---------CDSD Cluster Analysis----
  randomized_subjects <- unique(analysis.data.in$`key`)
  
  
  temp <- CDSD.in %>%
    dplyr::filter(`key` %in% randomized_subjects) %>%
    dplyr::select("key", "Study Day of Finding", "Question Name", "Numeric Finding in Standard Units", "Epoch")
  
  # Stack SCREENING AND BLINDED TREATMENT
  temp <- temp %>% dplyr::filter(Epoch %in% c("SCREENING","BLINDED TREATMENT")) %>% 
    dplyr::select(`key`, Epoch, `Question Name`, `Numeric Finding in Standard Units`) %>% 
    pivot_wider(names_from = `Question Name`, values_from = `Numeric Finding in Standard Units`, values_fn=mean) 

  temp[,-c(1,2)] <- scale(temp[,-c(1,2)])
  
  cluster_analysis_plot <- function(data.in=temp, k=8, seed=123, exclude=c(1,2)) {
    set.seed(seed)
    # Perform k-means clustering
    kmeans_result <- kmeans(data.in[,-exclude], centers = k, nstart = 50)
    
    # Add cluster results to the data
    data.in <- data.in %>% mutate(cluster = kmeans_result$cluster)
    
    # Compute average values per cluster
    cluster_means <- data.in %>% 
      group_by(cluster) %>% 
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
    
    cluster_means %>% rowwise() %>%  
      dplyr::summarize(average = mean(c_across(all_of(2:(k+1))))) %>%
      dplyr::mutate(cluster=1:k) %>% arrange(average) %>% 
      dplyr::mutate(cluster2 = 1:k) %>% dplyr::pull(cluster) -> cluster_order
    
    data.in$cluster <- factor(data.in$cluster, levels = cluster_order, labels = 1:k)
    
    # Pivot data for ggplot
    temp <- data.in %>%
      pivot_longer(cols = -c(1,2, ncol(.))) %>%
      dplyr::mutate(name = factor(name, unique(name)))
    
    # Compute average values for annotation
    avg_values <- temp %>%
      group_by(cluster, name) %>%
      dplyr::summarize(avg_value = mean(value), .groups = 'drop')
    
    
    
    # Create ggplot with annotations
    p <- temp %>%
      mutate(string=paste(`key`, Epoch )) %>%
      ggplot(aes(x = as.numeric(as.factor(name)), 
                 y = value, 
                 group = string, 
                 color = as.factor(cluster))) +
      geom_point() + 
      geom_line() +  
      facet_wrap(~cluster) + 
      coord_flip() + 
      scale_x_continuous(breaks = 1:length(levels(temp$name)), labels = levels(temp$name)) + 
      labs(
        title = "GI Symptoms by Cluster",
        subtitle="Subjects represented twice: Screening and Blinded Treatment",
        x = "Symptom",
        y = "Value",
        color = "Cluster",
        caption=paste0(
          "Cluster 1: Lower than average on all\n",
          "Cluster 2: Sligthly higher than average on Bloating, Tiredness and Ab Pain\n",
          "Cluster 3: Lower than average on all but BM Type 6 or 7\n",
          "Cluster 4: Moderately higher Diarrhea, slightly higher BM freq & Ab Pain\n",
          "Cluster 5: Moderately higher Tiredness, Nausea, Bloating, Ab Pain\n",
          "Cluster 6: Very high Diarrhea, Moderately on other symptoms.\n"
        )
      ) +  scale_color_manual(values = c("1" = "#F8766D", "2"= "#A3A500", "3"="#00BF7D", "4"="#00B0F6","5"= "#E76BF3","6"= "#FF61C3","7"= "grey","8"= "#00BFC4")) +
      # Add annotations with adjusted aesthetics
      geom_text(data = avg_values, 
                aes(x = as.numeric(as.factor(name)), 
                    y = avg_value, 
                    label = round(avg_value, 2)), 
                inherit.aes = FALSE, 
                color = "black", 
                size = 5) 
    
   path.table <- data.in %>% dplyr::select(`key`, Epoch, cluster) %>%
      left_join(analysis.data.in %>% dplyr::select(`key`, ARM) %>% group_by(`key`) %>% slice(1)) %>%
     pivot_wider(names_from = Epoch, values_from = cluster) %>%
      dplyr::mutate(path=paste(SCREENING, `BLINDED TREATMENT`)) %>%
      group_by(ARM, path) %>% dplyr::summarize(n = n()) %>% arrange(desc(n))
   
   path.plot <-   data.in %>% dplyr::select(`key`, Epoch, cluster) %>%
     left_join(analysis.data.in %>% dplyr::select(`key`, ARM) %>% group_by(`key`) %>% slice(1)) %>%
     pivot_wider(names_from = Epoch, values_from = cluster) %>%
     dplyr::mutate(path=paste(SCREENING, `BLINDED TREATMENT`)) %>%
     group_by(ARM, path) %>% ggplot(aes(x=SCREENING, y=`BLINDED TREATMENT`, color=ARM)) + geom_point(position = position_jitter(width = 0.25, height = 0.25)) + 
     labs(title="Path of Subjects by Arm", x="Cluster at Screening", y="Cluster Over Blineded Treatment", color="Arm") +
     geom_hline(yintercept = 1:8+.5, linetype="dashed") + geom_vline(xintercept = 1:8+.5, linetype="dashed")
    
    
    
    return(list(plot = p, kmeans_result = kmeans_result, cluster_means = cluster_means, data.in=data.in, path.table=path.table, path.plot=path.plot))
  }
  
  for.return$CDSD.cluster <- cluster_analysis_plot(data.in=temp, k=8, seed=123)

  #---------VCE Cluster Analysis----
  
  adi.temp <- analysis.data.in %>% dplyr::select(`key`, VISIT, ARM, "BVA", "Tertile1_Max", "Tertile2_Mean", 
                                     "Tertile2_Max", "Tertile3_Mean", "Tertile3_Max", "All_Mean",               
                                     "All_Max", "First5Percent_Mean") 
  adi.temp[,-c(1,2,3)] <- scale(adi.temp[,-c(1,2,3)])
  
   cluster_analysis_plot2 <- function(data.in=adi.temp, k=8, seed=123, exclude=c(1,2, 3)) {
    set.seed(seed)
    # Perform k-means clustering
    kmeans_result <- kmeans(data.in[,-exclude] %>% na.omit(), centers = k, nstart = 50)
    for.merge <- data.in %>% na.omit() %>% mutate(cluster = kmeans_result$cluster)
    # Add cluster results to the data
    data.in <- data.in %>% left_join(for.merge)
    data.in$cluster <- ifelse(is.na(data.in$cluster), k+1, data.in$cluster)
    
    # Compute average values per cluster
    cluster_means <- data.in %>% 
      group_by(cluster) %>% 
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
      dplyr::mutate(cluster=ifelse(is.na(cluster), k+1, cluster))
    
    cluster_means %>% rowwise() %>%  
      dplyr::summarize(average = mean(c_across(all_of(2:(k+1))))) %>%
      dplyr::mutate(cluster=1:nrow(.)) %>% arrange(average) %>% 
      dplyr::mutate(cluster2 = 1:nrow(.)) %>% dplyr::pull(cluster) -> cluster_order
    
    data.in$cluster <- factor(data.in$cluster, levels = cluster_order, labels = 1:length(cluster_order))
    
    
    # Pivot data for ggplot
    temp <- data.in %>%
      pivot_longer(cols = -all_of(c(exclude, ncol(.)))) %>%
      dplyr::mutate(name = factor(name, unique(name)))
    
    # Compute average values for annotation
    avg_values <- temp %>%
      group_by(cluster, name) %>%
      dplyr::summarize(avg_value = mean(value), .groups = 'drop')
    
    
    
    # Create ggplot with annotations
    p <- temp %>%
      mutate(string=paste(`key`, VISIT )) %>%
      ggplot(aes(x = as.numeric(as.factor(name)), 
                 y = value, 
                 group = string, 
                 color = as.factor(cluster))) +
      geom_point() + 
      geom_line() +  
      facet_wrap(~cluster) + 
      coord_flip() + 
      scale_x_continuous(breaks = 1:length(levels(temp$name)), labels = levels(temp$name)) + 
      labs(
        title = "GI Symptoms by Cluster",
        subtitle="Subjects represented twice: Screening and Blinded Treatment",
        x = "Symptom",
        y = "Value",
        color = "Cluster",
        caption=paste0(
          "Cluster 1: Lower than average on all\n",
          "Cluster 2: Lower than average on all but First 5%\n",
          "Cluster 3: Average BVA\n",
          "Cluster 4: Elevation in T3, Mild Elevation in T2, but not T1, typical First 5%\n",
          "Cluster 5: Missing First 5%\n",
          "Cluster 6: Elevated First 5%, All Max, All Mean, BVA, T1_Max\n",
          "Cluster 7: Elevated T1 and T2, typical T3."
        )
      ) +  scale_color_manual(values = c("1" = "#F8766D", "2"= "#A3A500", "3"="#00BF7D", "4"="#00B0F6","5"= "#E76BF3","6"= "#FF61C3","7"= "grey","8"= "#00BFC4")) +
      # Add annotations with adjusted aesthetics
      geom_text(data = avg_values, 
                aes(x = as.numeric(as.factor(name)), 
                    y = avg_value, 
                    label = round(avg_value, 2)), 
                inherit.aes = FALSE, 
                color = "black", 
                size = 5) 
    
    path.table <- data.in %>% dplyr::select(`key`, VISIT, cluster) %>%
      left_join(analysis.data.in %>% dplyr::select(`key`, ARM) %>% group_by(`key`) %>% slice(1)) %>%
      pivot_wider(names_from = VISIT, values_from = cluster) %>%
      dplyr::mutate(path=paste(`VISIT 2 - Week -4`, `VISIT 6 - Week 24`)) %>%
      group_by(ARM, path) %>% dplyr::summarize(n = n()) %>% arrange(desc(n))
    
    path.plot <-   data.in %>% dplyr::select(`key`, VISIT, cluster) %>%
      left_join(analysis.data.in %>% dplyr::select(`key`, ARM) %>% group_by(`key`) %>% slice(1)) %>%
      pivot_wider(names_from = VISIT, values_from = cluster) %>%
      dplyr::mutate(path=paste(`VISIT 2 - Week -4`, `VISIT 6 - Week 24`)) %>%
      group_by(ARM, path) %>% ggplot(aes(x=`VISIT 2 - Week -4`, y=`VISIT 6 - Week 24`, color=ARM)) + geom_point(position = position_jitter(width = 0.25, height = 0.25)) + 
      labs(title="Path of Subjects by Arm", x="Cluster at Screening", y="Cluster Over Blineded Treatment", color="Arm") +
      geom_hline(yintercept = 1:8+.5, linetype="dashed") + geom_vline(xintercept = 1:8+.5, linetype="dashed")
    
    
    
    return(list(plot = p, kmeans_result = kmeans_result, cluster_means = cluster_means, data.in=data.in, path.table=path.table, path.plot=path.plot))
  }
  
  for.return$VCE.cluster <- cluster_analysis_plot2(data.in=adi.temp, k=8, seed=123, exclude = c(1:3))
  for.return$VCE.cluster$data.in %>% 
    dplyr::select(key, VISIT, ARM, cluster) %>% group_by(key, VISIT) %>% slice(1) -> C1C2.merge

  analysis.data.in %>% dplyr::filter(`key` %in% C1C2.merge$key) %>%
    dplyr::select(key,VISIT, ARM, First5Percent_Mean, VHCD, IELCOUNT, GISS ) %>% 
    left_join(C1C2.merge) %>% group_by(cluster) %>% 
    dplyr::summarize(n=n(),mean.First5Percent=mean(First5Percent_Mean,na.rm=T),
                     mean.VHCD=mean(VHCD,na.rm=T), mean.IELCOUNT=mean(IELCOUNT,na.rm=T), mean.GISS=mean(GISS,na.rm=T)) -> C1C2.merge.stats
  # scatterplot matrix using ggally with point size associated with n, color associated with cluster
  C1C2.merge.stats %>% na.omit() %>% 
    ggplot(aes(x=mean.First5Percent, y=mean.VHCD)) + 
    geom_point() + 
    geom_smooth() +
    geom_point(aes(size=n, color=as.factor(cluster))) +
    labs(title="First 5% and VHCD", x="First 5% Mean", y="VHCD Mean", color="Cluster", size="n") +
    theme(legend.position = "bottom") 
  
  C1C2.merge.stats %>% na.omit() %>% 
    ggplot(aes(x=mean.First5Percent, y=mean.IELCOUNT)) + 
    geom_point() + 
    geom_smooth() +
    geom_point(aes(size=n, color=as.factor(cluster))) +
    labs(title="First 5% and mean.IELCOUNT", x="First 5% Mean", y="mean.IELCOUNT", color="Cluster", size="n") +
    theme(legend.position = "bottom") 
  
  C1C2.merge.stats %>% na.omit() %>% 
    ggplot(aes(x=mean.First5Percent, y=mean.GISS)) + 
    geom_point() + 
    geom_smooth() +
    geom_point(aes(size=n, color=as.factor(cluster))) +
    labs(title="First 5% and mean.GISS", x="First 5% Mean", y="mean.GISS", color="Cluster", size="n") +
    theme(legend.position = "bottom") 
  
  # ----- Symptoms by epoch and treatment arm----
  
  get.metrics <- function(data.in = CDSD, SUBJID) {
    # Filter the data for the given subject ID
    temp <- CDSD %>% dplyr::filter(`Unique Subject Identifier` == SUBJID)
    
    # Bind the rows for different epochs
    bind_rows(
      
      # Pre gluten SIGE Epoch (Study Day < -14)
      temp %>%
        group_by(`Question Name`) %>%
        dplyr::filter(`Study Day of Finding` < -14) %>%
        dplyr::select(`Numeric Finding in Standard Units`) %>%
        dplyr::summarise(
          n = n(), mean = mean(`Numeric Finding in Standard Units`, na.rm = TRUE),
          sd = sd(`Numeric Finding in Standard Units`, na.rm = TRUE)
        ) %>%
        dplyr::mutate(Epoch = "Pre gluten SIGE"),
      
      # Post gluten SIGE Epoch (-14 <= Study Day < 0)
      temp %>%
        group_by(`Question Name`) %>%
        dplyr::filter(`Study Day of Finding` >= -14 & `Study Day of Finding` < 0) %>%
        dplyr::select(`Numeric Finding in Standard Units`) %>%
        dplyr::summarise(
          n = n(), mean = mean(`Numeric Finding in Standard Units`, na.rm = TRUE),
          sd = sd(`Numeric Finding in Standard Units`, na.rm = TRUE)
        ) %>%
        dplyr::mutate(Epoch = "Post gluten SIGE"),
      
      # Window: Pre 12week Epoch (12 weeks before - 28 days to 12 weeks before day 0)
      temp %>%
        group_by(`Question Name`) %>%
        dplyr::filter(`Study Day of Finding` >= 12 * 7 - 28 & `Study Day of Finding` < 12 * 7) %>%
        dplyr::select(`Numeric Finding in Standard Units`) %>%
        dplyr::summarise(
          n = n(), mean = mean(`Numeric Finding in Standard Units`, na.rm = TRUE),
          sd = sd(`Numeric Finding in Standard Units`, na.rm = TRUE)
        ) %>%
        dplyr::mutate(Epoch = "Window: Pre 12week"),
      
      # Window: including 12week Epoch (12 weeks before day 0 +/- 14 days)
      temp %>%
        group_by(`Question Name`) %>%
        dplyr::filter(`Study Day of Finding` >= 12 * 7 - 14 & `Study Day of Finding` < 12 * 7 + 14) %>%
        dplyr::select(`Numeric Finding in Standard Units`) %>%
        dplyr::summarise(
          n = n(), mean = mean(`Numeric Finding in Standard Units`, na.rm = TRUE),
          sd = sd(`Numeric Finding in Standard Units`, na.rm = TRUE)
        ) %>%
        dplyr::mutate(Epoch = "Window: including 12week"),
      
      # Window: Post 12week Epoch (12 weeks from day 0 to 28 days post 12 weeks)
      temp %>%
        group_by(`Question Name`) %>%
        dplyr::filter(`Study Day of Finding` >= 12 * 7 & `Study Day of Finding` < 12 * 7 + 28) %>%
        dplyr::select(`Numeric Finding in Standard Units`) %>%
        dplyr::summarise(
          n = n(), mean = mean(`Numeric Finding in Standard Units`, na.rm = TRUE),
          sd = sd(`Numeric Finding in Standard Units`, na.rm = TRUE)
        ) %>%
        dplyr::mutate(Epoch = "Window: Post 12week"),
      
      # Window: Pre 24week Epoch (24 weeks before - 28 days to 24 weeks before day 0)
      temp %>%
        group_by(`Question Name`) %>%
        dplyr::filter(`Study Day of Finding` >= 24 * 7 - 28 & `Study Day of Finding` < 24 * 7) %>%
        dplyr::select(`Numeric Finding in Standard Units`) %>%
        dplyr::summarise(
          n = n(), mean = mean(`Numeric Finding in Standard Units`, na.rm = TRUE),
          sd = sd(`Numeric Finding in Standard Units`, na.rm = TRUE)
        ) %>%
        dplyr::mutate(Epoch = "Window: Pre 24week")
    ) %>%
      # Add the subject ID to the result
      dplyr::mutate(SUBJID = SUBJID)
  }
  
  holdit <- apply(matrix(1:length(randomized_subjects)), 1, function(x) {
    get.metrics(SUBJID = randomized_subjects[x])
  })
  holdit <- bind_rows(holdit)
  
  holdit$Epoch <- factor(holdit$Epoch, levels = c(
    "Pre gluten SIGE",
    "Post gluten SIGE",
    "Window: Pre 12week",
    "Window: including 12week",
    "Window: Post 12week",
    "Window: Pre 24week"
  ))
  holdit <- bind_rows(holdit)
  holdit <- holdit %>% dplyr::rename(key=SUBJID) %>% dplyr::left_join(analysis.data.in %>% dplyr::select(`key`, ARM),relationship = "many-to-many") 
  # Denisties
  # holdit %>% ggplot(aes(x=mean, fill=Epoch))+ geom_density(alpha=.5) +  facet_wrap_paginate(ARM~`Question Name`, scales = "free", ncol = 8, nrow = 2, page = 1)
    
  
  check <- holdit %>% dplyr::select(c(1,3,5,6), ARM) %>%
    dplyr::mutate(EQuestion = paste(Epoch, "-", `Question Name`)) %>%
    dplyr::select(-c(1,3)) %>%
    group_by(key, EQuestion) %>% slice(1) %>%
    pivot_wider(names_from=EQuestion, values_from=c(`mean`))
  
  bloat <- check %>% group_by(key, ARM) %>% dplyr::select(contains("Bloat")) %>% na.omit()
  diarrhea <- check %>% group_by(key, ARM) %>% dplyr::select(contains("Diarrhea")) %>% na.omit()
  abdominal_pain <- check %>% group_by(key, ARM) %>% dplyr::select(contains("Abdominal Pain")) %>% na.omit()
  nausea <- check %>% group_by(key, ARM) %>% dplyr::select(contains("Nausea")) %>% na.omit()
  tiredness <- check %>% group_by(key, ARM) %>% dplyr::select(contains("Tiredness")) %>% na.omit()
  how_many <- check %>% group_by(key, ARM) %>% dplyr::select(contains("How Many")) %>% na.omit()
  
  # There's a couple of cluster_analysis_plot functions defined
  cluster_analysis_plot3 <- function(data.in=bloat, k=6, exclude=1:2) {
    # Perform k-means clustering
    kmeans_result <- kmeans(data.in[,-exclude], centers = k, nstart = 25)
    data.in <- data.in %>% ungroup()
    # Add cluster results to the data
    screening <- data.in %>% mutate(cluster = kmeans_result$cluster)
    
    # Compute average values per cluster
    cluster_means <- screening %>% 
      group_by(cluster) %>% 
      dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
    
    cluster_means %>% rowwise() %>%  
      dplyr::summarize(average = mean(c_across(all_of(2:(k+1))))) %>%
      dplyr::mutate(cluster=1:k) %>% arrange(average) %>% 
      dplyr::mutate(cluster2 = 1:k) %>% dplyr::pull(cluster) -> cluster_order
    
    screening$cluster <- factor(screening$cluster, levels = cluster_order, labels = 1:k)
    
    # Pivot data for ggplot
    temp <- screening %>%
      pivot_longer(cols = -c(exclude,ncol(.))) %>%
      dplyr::mutate(name = factor(name, unique(name)))
    
    # Compute average values for annotation
    avg_values <- temp %>%
      group_by(cluster, name) %>%
      dplyr::summarize(avg_value = mean(value), .groups = 'drop')
    levels(temp$name)
    temp$name <- factor(sub("\\s-.*", "", temp$name), c(
      "Pre gluten SIGE", 
      "Post gluten SIGE",
      "Window: Pre 12week", 
      "Window: including 12week",
      "Window: Post 12week", 
      "Window: Pre 24week"
      )
    )
    
        # Create ggplot with annotations
    p <- temp %>%
      ggplot(aes(x = as.numeric(as.factor(name)), 
                 y = value, 
                 group = `key`, 
                 color = ARM)) +
      geom_point() + 
      geom_line() +  
      facet_wrap(~cluster) + 
      coord_flip() + 
      scale_x_continuous(breaks = 1:length(levels(temp$name)), labels = levels(temp$name)) + 
      labs(
        title = "GI Symptoms by Cluster",
        x = "Symptom",
        y = "Value",
        color = "Cluster"
      ) + 
      # Add annotations with adjusted aesthetics
      geom_text(data = avg_values, 
                aes(x = as.numeric(as.factor(name)), 
                    y = avg_value, 
                    label = round(avg_value, 2)), 
                inherit.aes = FALSE, 
                color = "black", 
                size = 5)
    
    return(list(plot = p, kmeans_result = kmeans_result, cluster_means = cluster_means))
  }
  
  # Setting the seed is important as k-means is a random algorithm
  set.seed(123)
  bloat.clust <- cluster_analysis_plot3(data.in=bloat, k=6)
  bloat.clust$plot$labels$title <- "Cluster Analysis for Bloating"
  bloat.improvers <- bloat.clust$plot[[1]] %>% dplyr::filter(cluster %in% c(1,3)) %>% dplyr::pull(key) %>% unique()
  set.seed(123)
  # No diarrea improvers
  diarrhea.clust <- cluster_analysis_plot3(data.in=diarrhea, k=6)
  diarrhea.clust$plot$labels$title <- "Cluster Analysis for Diarrhea"
  diarrhea.improvers <- bloat.clust$plot[[1]] %>% dplyr::filter(cluster %in% c(3)) %>% dplyr::pull(key) %>% unique()
  
  set.seed(123)
  ab_pain.clust <- cluster_analysis_plot3(data.in=abdominal_pain, k=6)
  ab_pain.clust$plot$labels$title <- "Cluster Analysis for Abdominal Pain"
  ab_pain.improvers <- ab_pain.clust$plot[[1]] %>% dplyr::filter(cluster %in% c(3)) %>% dplyr::pull(key) %>% unique()
  set.seed(123)
  nausea.clust <- cluster_analysis_plot3(data.in=nausea, k=6)
  nausea.clust$plot$labels$title <- "Cluster Analysis for Nausea"
  nausea.improvers <- nausea.clust$plot[[1]] %>% dplyr::filter(cluster %in% c(3)) %>% dplyr::pull(key) %>% unique()
  set.seed(123)
  tiredness.clust <- cluster_analysis_plot3(data.in=tiredness, k=6)
  tiredness.clust$plot$labels$title <- "Cluster Analysis for Tiredness"
  tiredness.improvers <- tiredness.clust$plot[[1]] %>% dplyr::filter(cluster %in% c(4)) %>% dplyr::pull(key) %>% unique()
  set.seed(123)

  improvers <- data.frame(
    key = c(bloat.improvers,diarrhea.improvers, ab_pain.improvers, nausea.improvers, tiredness.improvers),
    Symptom=c(rep("Bloating", length(bloat.improvers)),
              rep("Diarrhea", length(diarrhea.improvers)), 
              rep("Abdominal Pain", length(ab_pain.improvers)), 
              rep("Nausea", length(nausea.improvers)), 
              rep("Tiredness", length(tiredness.improvers)))  ) %>%
    dplyr::left_join(analysis.data.in %>% group_by(key, ARM) %>% slice(1) %>% dplyr::select(`key`, ARM) %>% dplyr::filter(key %in% improvers$key), relationship = "many-to-many")
  
  #-t.test----
t.test.report <- bind_rows(
  bloat %>%
    dplyr::mutate(Improver = `Window: Pre 24week - CDS1-How Severe Was Your Bloating` - `Post gluten SIGE - CDS1-How Severe Was Your Bloating` < 0) %>%
    select(ARM, Improver) %>%
    group_by(ARM) %>%
    dplyr::summarize(Improver = sum(Improver), n = n(), .groups = 'drop') %>%
    ungroup() %>%
    summarize(
      tidy_result = list(
        tidy(prop.test(Improver, n))
      )
    ) %>%
    unnest(tidy_result) %>% dplyr::mutate(endpoint="Bloating"),
  
  diarrhea %>% 
    dplyr::mutate(Improver = `Window: Pre 24week - CDS1-How Severe Was Your Diarrhea` - `Post gluten SIGE - CDS1-How Severe Was Your Diarrhea` < 0) %>%
    select(ARM, Improver) %>%
    group_by(ARM) %>%
    dplyr::summarize(Improver = sum(Improver), n = n(), .groups = 'drop') %>%
    ungroup() %>%
    summarize(
      tidy_result = list(
        tidy(prop.test(Improver, n))
      )
    ) %>%
    unnest(tidy_result) %>% dplyr::mutate(endpoint="Diarrhea"),
  
  abdominal_pain %>%
    dplyr::mutate(Improver = `Window: Pre 24week - CDS1-How Severe Was Your Abdominal Pain` - `Post gluten SIGE - CDS1-How Severe Was Your Abdominal Pain` < 0) %>%
    select(ARM, Improver) %>%
    group_by(ARM) %>%
    dplyr::summarize(Improver = sum(Improver), n = n(), .groups = 'drop') %>%
    ungroup() %>%
    summarize(
      tidy_result = list(
        tidy(prop.test(Improver, n))
      )
    ) %>%
    unnest(tidy_result) %>% dplyr::mutate(endpoint="Abdominal Pain"),
  
  nausea %>%
    dplyr::mutate(Improver = `Window: Pre 24week - CDS1-How Severe Was Your Nausea` - `Post gluten SIGE - CDS1-How Severe Was Your Nausea` < 0) %>%
    select(ARM, Improver) %>%
    group_by(ARM) %>%
    dplyr::summarize(Improver = sum(Improver), n = n(), .groups = 'drop') %>%
    ungroup() %>%
    summarize(
      tidy_result = list(
        tidy(prop.test(Improver, n))
      )
    ) %>%
    unnest(tidy_result) %>% dplyr::mutate(endpoint="Nausea"),
  
  tiredness %>%
    dplyr::mutate(Improver = `Window: Pre 24week - CDS1-How Severe Was Your Tiredness` - `Post gluten SIGE - CDS1-How Severe Was Your Tiredness` < 0) %>%
    select(ARM, Improver) %>%
    group_by(ARM) %>%
    dplyr::summarize(Improver = sum(Improver), n = n(), .groups = 'drop') %>%
    ungroup() %>%
    summarize(
      tidy_result = list(
        tidy(prop.test(Improver, n))
      )
    ) %>%
    unnest(tidy_result) %>% dplyr::mutate(endpoint="Tiredness")) %>% dplyr::select(-method)
  

#----package for return
  for.return$symptom.summary <- list(
    symptoms_by_epoch=list(bloat.clust, diarrhea.clust, ab_pain.clust, nausea.clust, tiredness.clust),
    improvers = improvers, 
                                       n.distinct=improvers %>% n_distinct(),
                                       no.symptom.improve=improvers %>% group_by(key) %>% dplyr::mutate(n=n()) %>% group_by(n) %>% dplyr::summarize(tally=n()),
                                       symtom.improver.report=analysis.data.in %>% dplyr::filter(key %in% improvers$key) %>% 
                                         group_by(ARM) %>% dplyr::summarize(`Symptom Improver` = n()), 
                                       t.test.report = t.test.report)


  # Are there differences in the proportion of improvers between arms?----
  # Improver defined as CDSD in Pre24 week period is lower than Post gluten SIGE



  
  
}

# Form Merged Data Sets
# This function prepares study data of all types for analysis by the several
# modues. See file analysis-partioning-and-reporting.xlsx for details both as
# to the structure of the retrun, and how the various modules use it. All of
# the qmd files call this function, in order to ensure consistency. 
#

form_merged_data_sets <- function(analysis_type, study, this_endpoint, output_dir) {
  # Primarily we will return merged_data_sets and provenance, both of which are lists
  merged_data_sets <- list()
  provenance <- list()

  # Gather prepared data following ETL. First, data used by all analysis types:
  if (study=="Milan") {
    demog_etl_output <- demog_milan_etl(provenance,log_dir=output_dir)
    demog_df <- demog_etl_output$demog_df
    provenance <- demog_etl_output$provenance
    etl_output <- vce_milan_etl(provenance,log_dir=output_dir)
    vce_dfs <- etl_output$vce_dfs
    AMR <- etl_output$AMR
    provenance <- etl_output$provenance
  } else if (study=="Phase0") {
    demog_etl_output <- demog_phase0_etl(provenance,log_dir=output_dir)
    demog_df <- demog_etl_output$demog_df
    provenance <- demog_etl_output$provenance
    etl_output <- vce_phase0_etl(provenance,log_dir=output_dir)
    vce_dfs <- etl_output$vce_dfs
    AMR <- etl_output$AMR
    provenance <- etl_output$provenance
  } else if (study=="TAK-062-2001") {
    demog_etl_output <- demog_062_2001_etl(provenance,log_dir=output_dir)
    demog_df <- demog_etl_output$demog_df
    provenance <- demog_etl_output$provenance
    etl_output <- vce_062_2001_etl(provenance,log_dir=output_dir,analysis_type=analysis_type) # the frame level objects may be skipped for most analyses
    vce_dfs <- etl_output$vce_df
    AMR <- etl_output$AMR
    provenance <- etl_output$provenance
  } else if (study=="Mock-Study") {
    demog_etl_output <- demog_mock_etl(provenance,log_dir=output_dir)
    demog_df <- demog_etl_output$demog_df
    provenance <- demog_etl_output$provenance
    etl_output <- vce_mock_etl(provenance,log_dir=output_dir)
    vce_dfs <- etl_output$vce_dfs
    AMR <- etl_output$AMR
    provenance <- etl_output$provenance
  } else if (study =="Sheffield"){
    demog_etl_output <- demog_sheffield_etl(provenance,log_dir=output_dir)
    demog_df <- demog_etl_output$demog_df
    etl_output <- vce_sheffield_etl(provenance,log_dir=output_dir)
    vce_dfs <- etl_output$vce_dfs
  }
    else {
    cat("LOGIC ERROR: unsupported study")
  }
  counts <- data.frame(Data=c("Demographics"),
                       Sites=c(length(unique(demog_df[["SITE"]]))),
                       Subjects=nrow(demog_df %>% filter(!duplicated(paste(SITE, SUBJID)))),
                       Visits=NA,
                       Observations=NA)

  for (i in 1:length(vce_dfs)) {
    vce_df <- vce_dfs[[i]]
    count_name <- "MARCS"
    if (length(vce_dfs)>1) {
      count_name <- paste0(count_name, '.', names(vce_dfs)[i])
    }
    counts <- bind_rows(counts, data.frame(Data=c(count_name),
                                           Sites=c(length(unique(vce_df[["SITE"]]))),
                                           Subjects=nrow(vce_df %>% filter(!duplicated(paste(SITE, SUBJID)))),
                                           Visits=nrow(vce_df %>% filter(!duplicated(paste(SITE, SUBJID, VISIT)))),
                                           Observations=nrow(vce_df %>% filter(!duplicated(paste(SITE, SUBJID, OBSDATE))))))
    
    vce_df$SITE <- as.character(vce_df$SITE) # sometimes the SITE appears as an integer but we always want it to be a string
    
    merged_data <- right_join(demog_df, vce_df, by=c("STUDYID", "SITE", "SUBJID")) %>%
      dplyr::rename(OBSDATE.VCE = OBSDATE) 

    # our first pairing outlier check only considers VCE: 
    pairing_outlier_check <- merged_data %>% 
      drop_na(OBSDATE.VCE) %>%
      dplyr::group_by(STUDYID, SITE, SUBJID, VISIT) %>% 
      dplyr::summarise(
        earliest=min(as.POSIXct(OBSDATE.VCE), na.rm=TRUE),
        latest=max(as.POSIXct(OBSDATE.VCE), na.rm=TRUE),
        span=as.numeric(as.Date(latest, "%Y-%m-%d")-as.Date(earliest, "%Y-%m-%d")),
        .groups='drop'
      )
    
    write.csv(merged_data, path(output_dir, paste0("1 ", count_name, "_dataAfterVCEandDemogJoin.csv")), row.names=FALSE)
    
    if (analysis_type != "Precision") {
      # whereas Precision retains both test and re-test metrics, non of the other analysis types do
      write.csv(merged_data %>% dplyr::arrange(STUDYID, SITE, SUBJID, OBSDATE.VCE), path(output_dir, paste0("1a ", count_name, "_arranged data.csv")), row.names=FALSE)
      merged_data <- merged_data %>%
        dplyr::group_by(STUDYID, SITE, SUBJID, VISIT, READER) %>%
        # Many records do not have OBSDATE.VCE
        # dplyr::arrange(OBSDATE.VCE) %>% 
        # dplyr::filter(row_number()==1) %>%
        # Instead:
        dplyr::filter(Retest.Flag==FALSE) %>%
        dplyr::ungroup()
      write.csv(merged_data, path(output_dir, paste0("1b ", count_name, "_filtered data.csv")), row.names=FALSE)
    }
    
    # Histology data used by only some of the analysis types:
    histology_df <- data.frame() # initialize it so that even if it is not used, it is defined
    if (analysis_type %in% c("Path Agreement",
                             "Single Timepoint Association",
                             "Assoc of Change Across Time",
                             "Proportion Explained",
                             "Clinical Trial Utility",
                             "Precision")) {
      if (study=="Milan") {
        etl_output <- histology_milan_etl(provenance,log_dir=output_dir)
        histology_df <- etl_output$histology_df
        provenance <- etl_output$provenance
      } else if (study=="Phase0") {
        etl_output <- histology_phase0_etl(provenance,log_dir=output_dir)
        histology_df <- etl_output$histology_df
        provenance <- etl_output$provenance
      } else if (study=="TAK-062-2001") {
        etl_output <- histology_062_2001_etl(provenance,log_dir=output_dir)
        histology_df <- etl_output$histology_df
        provenance <- etl_output$provenance
      } else if (study=="Mock-Study") {
        etl_output <- histology_mock_etl(provenance,log_dir=output_dir)
        histology_df <- etl_output$histology_df
        provenance <- etl_output$provenance
      } else if (study=="Sheffield") {
        etl_output <- histology_sheffield_etl(provenance,log_dir=output_dir)
        histology_df <- etl_output$histology_df
        provenance <- etl_output$provenance
      }
      
      
      counts <- bind_rows(counts, data.frame(Data=c("Histology"),
                                             Sites=c(length(unique(histology_df[["SITE"]]))),
                                             Subjects=nrow(histology_df %>% filter(!duplicated(paste(SITE, SUBJID)))),
                                             Visits=nrow(histology_df %>% filter(!duplicated(paste(SITE, SUBJID, VISIT)))),
                                             Observations=nrow(histology_df %>% filter(!duplicated(paste(SITE, SUBJID, OBSDATE))))))
      
      # Changing from full_join
      merged_data <- left_join(merged_data, histology_df %>% filter(paste(SITE, SUBJID) %in% paste(merged_data$SITE, merged_data$SUBJID)), 
                               by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>%
        dplyr::rename(OBSDATE.histology = OBSDATE)
      
      # we extend our pairing outlier check to encompass VCE and histology: 
      pairing_outlier_check <- merged_data %>% 
        drop_na(OBSDATE.VCE) %>%
        dplyr::group_by(STUDYID, SITE, SUBJID, VISIT) %>% 
        dplyr::summarise(
          earliest=min(c(as.POSIXct(OBSDATE.VCE), as.POSIXct(OBSDATE.histology)), na.rm=TRUE),
          latest=max(c(as.POSIXct(OBSDATE.VCE), as.POSIXct(OBSDATE.histology)), na.rm=TRUE),
          span=as.numeric(as.Date(latest, "%Y-%m-%d")-as.Date(earliest, "%Y-%m-%d")),
          .groups='drop'
        )
      
      write.csv(merged_data, path(output_dir, paste0("2 ", count_name, "_AfterHistologyJoin.csv")), row.names=FALSE)
    }
      
    # Symptoms data used in even fewer of the analysis types:
    symptoms_df <- data.frame() # initialize it so that even if it is not used, it is defined
    if (analysis_type %in% c("Single Timepoint Association",
                             "Assoc of Change Across Time",
                             "Proportion Explained",
                             "Clinical Trial Utility", "Precision")) {
      if (study=="Milan") {
        etl_output <- symptoms_milan_etl(provenance,log_dir=output_dir)
        symptoms_df <- etl_output$symptoms_df
        provenance <- etl_output$provenance
      } else if (study=="Phase0") {
        etl_output <- symptoms_phase0_etl(provenance,log_dir=output_dir)
        symptoms_df <- etl_output$symptoms_df
        provenance <- etl_output$provenance
      } else if (study=="TAK-062-2001") {
        etl_output <- symptoms_062_2001_etl(provenance,log_dir=output_dir)
        symptoms_df <- etl_output$symptoms_df
        provenance <- etl_output$provenance
      } else if (study=="Mock-Study") {
        etl_output <- symptoms_mock_etl(provenance,log_dir=output_dir)
        symptoms_df <- etl_output$symptoms_df
        provenance <- etl_output$provenance
      } else if (study=="Sheffield") {
        etl_output <- symptoms_sheffield_etl(provenance,log_dir=output_dir)
        symptoms_df <- etl_output$symptoms_df
        provenance <- etl_output$provenance
      }
      
      counts <- bind_rows(counts, data.frame(Data=c("Symptomatology"),
                                             Sites=c(length(unique(symptoms_df[["SITE"]]))),
                                             Subjects=nrow(symptoms_df %>% filter(!duplicated(paste(SITE, SUBJID)))),
                                             Visits=nrow(symptoms_df %>% filter(!duplicated(paste(SITE, SUBJID, VISIT)))),
                                             Observations=nrow(symptoms_df %>% filter(!duplicated(paste(SITE, SUBJID, OBSDATE))))))
      if(study == "TAK-062-2001"){
        
      merged_data <- left_join(merged_data, full_join(symptoms_df, demog_df), by=c("STUDYID", "SITE", "SUBJID", "VISIT", "DOB", "ARM", 
                                                                                   "ETHNICITY", "RACE", "SEX", "ENRDATE", "RANDDATE",
                                                                                   "SEROSTATUS", "HISTINJURY", "PPIHIST2ANT")) %>%
        dplyr::rename(OBSDATE.symptoms = OBSDATE)
      } else {
        symptoms_df <- symptoms_df 
        
        merged_data <- left_join(merged_data, full_join(symptoms_df, demog_df), by=c("STUDYID", "SITE", "SUBJID", "VISIT", "DOB", "ARM", 
                                                                                     "ETHNICITY", "RACE", "SEX", "ENRDATE", "RANDDATE")) %>%
          dplyr::rename(OBSDATE.symptoms = OBSDATE)
      }

      # we extend our pairing outlier check to now also encompass symptoms: 
      pairing_outlier_check <- merged_data %>% 
        drop_na(OBSDATE.VCE) %>%
        dplyr::group_by(STUDYID, SITE, SUBJID, VISIT) %>% 
        dplyr::summarise(
          earliest=min(c(as.POSIXct(OBSDATE.VCE), as.POSIXct(OBSDATE.histology), as.POSIXct(OBSDATE.symptoms)), na.rm=TRUE),
          latest=max(c(as.POSIXct(OBSDATE.VCE), as.POSIXct(OBSDATE.histology), as.POSIXct(OBSDATE.symptoms)), na.rm=TRUE),
          span=as.numeric(as.Date(latest, "%Y-%m-%d")-as.Date(earliest, "%Y-%m-%d")),
          .groups='drop'
        )
      
      write.csv(merged_data, path(output_dir, paste0("3 ", count_name, "_dataAfterSymptomsJoin.csv")), row.names=FALSE)
    }
    # Serology data used in even fewer of the analysis types:
    serology_df <- data.frame() # initialize it so that even if it is not used, it is defined
    if (analysis_type %in% c("Single Timepoint Association",
                             "Assoc of Change Across Time", "Precision")) {
      if (study=="Milan") {
        etl_output <- serology_milan_etl(provenance,log_dir=output_dir)
        serology_df <- etl_output$serology_df
        provenance <- etl_output$provenance
      } else if (study=="Phase0") {
        etl_output <- serology_phase0_etl(provenance,log_dir=output_dir)
        serology_df <- etl_output$serology_df
        provenance <- etl_output$provenance
      } else if (study=="TAK-062-2001") {
        etl_output <- serology_062_2001_etl(provenance,log_dir=output_dir)
        serology_df <- etl_output$serology_df
        provenance <- etl_output$provenance
      } else if (study=="Mock-Study") {
        etl_output <- serology_mock_etl(provenance,log_dir=output_dir)
        serology_df <- etl_output$serology_df
        provenance <- etl_output$provenance
      } else if (study=="Sheffield") {
        etl_output <- serology_sheffield_etl(provenance,log_dir=output_dir)
        serology_df <- etl_output$serology_df
        provenance <- etl_output$provenance
      }
      
      counts <- bind_rows(counts, data.frame(Data=c("Serology"),
                                             Sites=c(length(unique(serology_df[["SITE"]]))),
                                             Subjects=nrow(serology_df %>% filter(!duplicated(paste(SITE, SUBJID)))),
                                             Visits=nrow(serology_df %>% filter(!duplicated(paste(SITE, SUBJID, VISIT)))),
                                             Observations=nrow(serology_df %>% filter(!duplicated(paste(SITE, SUBJID, OBSDATE))))))
      
      merged_data <- left_join(merged_data, serology_df, by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>% 
        dplyr::rename(OBSDATE.serology = OBSDATE)
      
      # we extend our pairing outlier check to now also encompass serology: 
      pairing_outlier_check <- merged_data %>% 
        drop_na(OBSDATE.VCE) %>%
        dplyr::group_by(STUDYID, SITE, SUBJID, VISIT) %>% 
        dplyr::summarise(
          earliest=min(c(as.POSIXct(OBSDATE.VCE), as.POSIXct(OBSDATE.histology), as.POSIXct(OBSDATE.symptoms), as.POSIXct(OBSDATE.serology)), na.rm=TRUE),
          latest=max(c(as.POSIXct(OBSDATE.VCE), as.POSIXct(OBSDATE.histology), as.POSIXct(OBSDATE.symptoms), as.POSIXct(OBSDATE.serology)), na.rm=TRUE),
          span=as.numeric(as.Date(latest, "%Y-%m-%d")-as.Date(earliest, "%Y-%m-%d")),
          .groups='drop'
        )
      
      write.csv(merged_data, path(output_dir, paste0("4 ", count_name, "_dataAfterSerologyJoin.csv")), row.names=FALSE)
    }
    
    count_name <- "Merged"
    if (length(vce_dfs)>1) {
      count_name <- paste0(count_name, '.', names(vce_dfs)[i])
    }
    
    counts <- bind_rows(counts, data.frame(Data=c(count_name),
                                           Sites=c(length(unique(merged_data[["SITE"]]))),
                                           Subjects=nrow(merged_data %>% filter(!duplicated(paste(SITE, SUBJID)))),
                                           Visits=nrow(merged_data %>% filter(!duplicated(paste(SITE, SUBJID, VISIT)))),
                                           Observations=nrow(merged_data %>% filter(!duplicated(paste(SITE, SUBJID, OBSDATE.VCE))))))
    
    if(!(analysis_type %in% c("Accuracy", "Precision"))){
    merged_data <- add.latent_disease_burden_comparators(df.to_be_extended=merged_data,output_dir=output_dir)
    }
    write.csv(merged_data, path(output_dir, paste0("5a ", count_name, "_dataAfterJoins.csv")), row.names=FALSE)
    
    write.csv(pairing_outlier_check, path(output_dir, paste0("5b ", count_name, "_pairing_outlier_check.csv")), row.names=FALSE)
    # We drop from 306 to 238 rows with the filters on.
    merged_data <- left_join(merged_data, pairing_outlier_check, by=c("STUDYID", "SITE", "SUBJID", "VISIT")) %>% arrange(desc(span)) # %>%
        # dplyr::filter(!is.na(span)) %>%  
        # dplyr::filter(span <= 14)
    merged_data$VISIT <- factor(merged_data$VISIT, levels=demog_etl_output$visit_levels)
    write.csv(merged_data, path(output_dir, paste0("5c ", count_name, "_dataBeforePairings.csv")), row.names=FALSE)
    
    # now either fabricate intervals (for some analysis types), or check span of observations (for other analysis types)
   
    if (analysis_type %in% c("Assoc of Change Across Time","Proportion Explained")) {
      # for these study types, the pairings are based on intervals of visits, rather than the visits themselves
      # this method from https://dplyr.tidyverse.org/reference/group_map.html#:~:text=group_map(),%20group_modify()%20and%20group_walk()%20are%20purrr-style%20functions%20that%20can%20be
      merged_data$key <- paste(merged_data$STUDYID, merged_data$SITE, merged_data$SUBJID)
      holdit <- apply(matrix(1:length(unique(merged_data$key))), 1, function(x){
        temp <- merged_data %>% filter(key == unique(merged_data$key)[x])
        unique_visits <- as.character(sort(unique(temp$VISIT)))
        new_merged_data_snippet <- merged_data[0,]
        if (length(unique_visits) > 1) {
          intervals <- CombPairs(x=unique_visits)
          for (i in 1:(nrow(intervals))) {
            interval <- intervals[i,]
            interval_rows <- temp %>% dplyr::filter(VISIT==as.character(interval[1])) # all the values will be overwritten, this is just for sizing
            # for each MARCS score, and comparator the two aggregates, do the diff
            for (variable in intersect(c(unlist(vce_endpoints), unlist(comparators)), colnames(temp))) {
              later_value <- temp %>% dplyr::filter(VISIT==as.character(interval[2])) %>% dplyr::pull(variable)
              earlier_value <- temp %>% dplyr::filter(VISIT==as.character(interval[1])) %>% dplyr::pull(variable)
              interval_rows[[variable]] <- ifelse(length(later_value-earlier_value)==0, NA, later_value-earlier_value) # will replace by variable-specific change calc
            }
            interval_rows$pairing <- paste0(interval[1], ":", interval[2])
            new_merged_data_snippet <- bind_rows(new_merged_data_snippet, interval_rows)
          }
        }
        new_merged_data_snippet%>% dplyr::select(-c(VISIT))  
      }) %>% bind_rows() 
      merged_data <- holdit %>% dplyr::select(-key)
      merged_data$pairing <- factor(merged_data$pairing, levels=demog_etl_output$interval_levels)
      
      # Running into issues with this implementation... commenting it out for time being.
      # fab_intervals <- function(.x, .y, intervals) {
      #   # .x is group from merged_data_orig, .y is the key for the group, intervals are what to fabricate
      #   .x <- .x %>% dplyr::arrange(OBSDATE.VCE,READER)
      #   browser()
      #   new_merged_data_snippet <- merged_data[0,]
      #   unique_visits <- unique(.x$VISIT)
      #   write.csv(.x, path(output_dir, paste0("5c ", count_name, "_.x.csv")), row.names=FALSE)
      #   if (length(unique_visits) > 1) {
      #     #intervals <- combinations(n=length(unique_visits), r=2, v=unique_visits)
      #     intervals <- as.matrix(CombPairs(unique_visits))
      #     for (i in 1:(length(intervals)/2)) {
      #       interval <- intervals[i,]
      #       interval_rows <- .x %>% dplyr::filter(VISIT==interval[1]) # all the values will be overwritten, this is just for sizing
      #       # for each MARCS score, and comparator the two aggregates, do the diff
      #       for (variable in intersect(c(unlist(vce_endpoints), unlist(comparators)), colnames(.x))) {
      #         later_value <- (.x %>% dplyr::filter(VISIT==intervals[2]))[[variable]]
      #         earlier_value <- (.x %>% dplyr::filter(VISIT==intervals[1]))[[variable]]
      #         interval_rows[[variable]] <- later_value-earlier_value # will replace by variable-specific change calc
      #       }
      #       interval_rows$pairing <- paste0(intervals[1], ":", intervals[2])
      #       new_merged_data_snippet <- bind_rows(new_merged_data_snippet, interval_rows)
      #     }
      #   }
      #   return(new_merged_data_snippet%>% dplyr::select(-c(STUDYID,SITE,SUBJID,VISIT)))
      # }
      # merged_data <- merged_data %>%
      #   dplyr::filter(!is.na(OBSDATE.VCE)) %>%
      #   dplyr::group_by(STUDYID, SITE, SUBJID) %>% 
      #   dplyr::group_modify(fab_intervals, .keep=FALSE)
      
      
      
      merged_data$pairing <- factor(merged_data$pairing, levels=demog_etl_output$interval_levels)

      
    } else {
      # for the rest of the analysis types, the pairings are simply the visits
      merged_data <- merged_data %>% dplyr::mutate(pairing=VISIT)
      merged_data$pairing <- factor(merged_data$pairing, levels=demog_etl_output$visit_levels)
    } 
    
    merged_data <- merged_data %>%
      dplyr::filter(!is.na(OBSDATE.VCE)) %>%
      dplyr::group_by(STUDYID, SITE, SUBJID) %>% dplyr::arrange(pairing)
    write.csv(merged_data, path(output_dir, paste0("5d ", count_name, "_dataAfterPairings.csv")), row.names=FALSE)
    
    # is.na(READER) == TRUE to pick up other measures
    merged_data.ML.full <- merged_data %>% filter(str_detect(READER, "^MLV") | is.na(READER) == TRUE)
    merged_data.Human.full <- merged_data %>% filter(!str_detect(READER, "^MLV") | is.na(READER) == TRUE)
    
    write.csv(merged_data.ML.full, path(output_dir, "6a merged_data.ML.full.csv"), row.names=FALSE)
    write.csv(merged_data.Human.full, path(output_dir, "6b merged_data.Human.full.csv"), row.names=FALSE)
    
    # form the actual return set structure
    merged_data.ML <- list(merged_data.ML.full=merged_data.ML.full)
    merged_data.Human <- list(merged_data.Human.full=merged_data.Human.full)
    merged_data_set <- list(merged_data.ML=merged_data.ML, merged_data.Human=merged_data.Human)
    merged_data_sets[[length(merged_data_sets)+1]] <- merged_data_set
    names(merged_data_sets)[i] <- names(vce_dfs)[i]
  } # for each IQ level

  # do some global analyses
  global_analysis_set <- merged_data_sets[[1]]$merged_data.ML$merged_data.ML.full
  # for posterity since there's a filter coming...
  global_analysis_set_out <- global_analysis_set
  write.csv(global_analysis_set_out, path(output_dir, paste0("7 global_analysis_set-prefilter-", "ML", ".csv")), row.names=FALSE)
  earliest_pairing <- dplyr::arrange(global_analysis_set, OBSDATE.VCE)$pairing[1]
  global_analysis_set <- dplyr::filter(global_analysis_set, pairing==earliest_pairing)
  write.csv(global_analysis_set, path(output_dir, paste0("7a global_analysis_set-postfilter-", "ML", ".csv")), row.names=FALSE)
  
  demographics <- global_analysis_set %>%
    dplyr::select(STUDYID, SITE, SUBJID, ARM, SEX, DOB, RACE, ETHNICITY, ENRDATE) %>% 
    group_by(STUDYID, SITE, SUBJID, ENRDATE) %>% 
    dplyr::mutate(AGE=as.numeric(difftime(ENRDATE, DOB, units="weeks")) / 52.25) %>%
    ungroup() %>%
    dplyr::select(ARM, SEX, RACE, ETHNICITY, AGE, -ENRDATE) %>%
    gtsummary::tbl_summary(by=ARM, type=list(AGE ~ "continuous"), statistic=list(AGE ~ "{mean} ({sd})") ) %>% 
      add_n() |> # add column with total number of non-missing observations
      add_p() |>
      modify_header(label="**Variable**") |> # update the column header
      bold_labels() 
  
  # GC: I'm getting an error
 # gt::gtsave(demographics %>% as_gt(), filename=path(table_path, "demographics.formatted.png"))
  write.csv(demographics, file=path(table_path, "demographics.csv"))
  
  return_data <- list(merged_data_sets=merged_data_sets,
                      demographics=demographics,
                      provenance=as.character(provenance),
                      counts=counts,
                      global_analysis_set_out=global_analysis_set_out)
  return(return_data)
}
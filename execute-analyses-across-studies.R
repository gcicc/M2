# Execute-Analyses-Across-Studies: Provide means for convenient and consistent analyses
# for MARCS validation. Note that it takes advantage of the multi-study support and adds
# to it a multi-analysis support. Assumes that global-config has already been set.

source(paste0(repo_clone,"/Shared-Content/preamble.R"))

analysis_types <- c(
  # "Accuracy",
  # "Precision",
  # "Gastroenterologist Agreement",
  "Path Agreement",
  "Single Timepoint Association",
  "Assoc of Change Across Time",
  "Proportion Explained"#,
  # "Clinical Trial Utility"
)
for (analysis_type in analysis_types) {
  for (study in studies) {
    if (study=="Milan") {
      if (analysis_type=="Path Agreement") { # milan only supports path agreement
        input_dir <- path(input_base_dir,study)
        output_dir <- output_dir_func(analysis_type,study)
        # first truncate the analysis summary csv for this new run (might have been there if multiple runs in same day)
        analysis_summary <- path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv"))
        if (file.exists(analysis_summary)) {
          file.remove(analysis_summary)
        }
        for (this_endpoint in vce_endpoints) {
          if (analysis_type=="Path Agreement") {
            if (this_endpoint=="First5Percent_Mean")  {
              run_quarto_with_error_handling(Path_Agreement_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
            }
          } else { 
            print(paste0(analysis_type," anaysis not supported in ",study,", skipping."))
          }
        }
      }
    } else if (study=="Phase0") {
      input_dir <- path(input_base_dir,study)
      output_dir <- output_dir_func(analysis_type,study)
      # first truncate the analysis summary csv for this new run (might have been there if multiple runs in same day)
      analysis_summary <- path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv"))
      if (file.exists(analysis_summary)) {
        file.remove(analysis_summary)
      }
      for (this_endpoint in vce_endpoints) {
        if (analysis_type=="Path Agreement") {
          if (this_endpoint=="First5Percent_Mean")  {
            run_quarto_with_error_handling(Path_Agreement_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
          }
        } else if (analysis_type=="Single Timepoint Association") { 
          run_quarto_with_error_handling(Assoc_Single_Tp_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Assoc of Change Across Time") { 
          run_quarto_with_error_handling(Assoc_Change_Across_Tps_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Proportion Explained") { 
          run_quarto_with_error_handling(Proportion_Explained_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else { 
          print(paste0(analysis_type," anaysis not supported in ",study,", skipping."))
        }
      }
    } else if (study=="TAK-062-2001") {
      input_dir <- path(input_base_dir,study)
      output_dir <- output_dir_func(analysis_type,study)
      # first truncate the analysis summary csv for this new run (might have been there if multiple runs in same day)
      analysis_summary <- path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv"))
      if (file.exists(analysis_summary)) {
        file.remove(analysis_summary)
      }
      for (this_endpoint in vce_endpoints) {
        if (analysis_type=="Accuracy") { 
          run_quarto_with_error_handling(Accuracy_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Precision") { 
          run_quarto_with_error_handling(Precision_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Gastroenterologist Agreement") { 
          run_quarto_with_error_handling(Gastro_Agreement_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Path Agreement") {
          if (this_endpoint=="First5Percent_Mean")  {
            run_quarto_with_error_handling(Path_Agreement_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
          }
        } else if (analysis_type=="Single Timepoint Association") { 
          run_quarto_with_error_handling(Assoc_Single_Tp_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Assoc of Change Across Time") { 
          run_quarto_with_error_handling(Assoc_Change_Across_Tps_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Proportion Explained") { 
          run_quarto_with_error_handling(Proportion_Explained_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Clinical Trial Utility") { 
          run_quarto_with_error_handling(Clinical_Utility_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else { 
          print("Logic error: All analysis types are supported in TAK-062-2001.")
        }
      }
    } else if (study=="Mock-Study") {
      input_dir <- path(input_base_dir,study)
      output_dir <- output_dir_func(analysis_type,study)
      # first truncate the analysis summary csv for this new run (might have been there if multiple runs in same day)
      analysis_summary <- path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv"))
      if (file.exists(analysis_summary)) {
        file.remove(analysis_summary)
      }
      for (this_endpoint in vce_endpoints) {
        if (analysis_type=="Accuracy") { 
          run_quarto_with_error_handling(Accuracy_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Precision") { 
          run_quarto_with_error_handling(Precision_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Gastroenterologist Agreement") { 
          run_quarto_with_error_handling(Gastro_Agreement_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Path Agreement") {
          if (this_endpoint=="First5Percent_Mean")  {
            run_quarto_with_error_handling(Path_Agreement_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
          }
        } else if (analysis_type=="Single Timepoint Association") { 
          run_quarto_with_error_handling(Assoc_Single_Tp_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Assoc of Change Across Time") { 
          run_quarto_with_error_handling(Assoc_Change_Across_Tps_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Proportion Explained") { 
          run_quarto_with_error_handling(Proportion_Explained_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else if (analysis_type=="Clinical Trial Utility") { 
          run_quarto_with_error_handling(Clinical_Utility_sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint)
        } else { 
          print("Logic error: All analysis types are supported in Mock-Study.")
        }
      }
    } else {
      print("Logic error: unsupported study")
    }
  }
}

# Run one off

# This loads the preamble.R from MARCS repo
source(paste0(repo_clone,"/Shared-Content/preamble.R"))

# Sorted by Development rather than protocol's order...

# analysis_type <- "Accuracy"
# sc <- Accuracy_sc

# analysis_type <- "Precision"
# sc <- Precision_sc

# analysis_type <- "Gastroenterologist Agreement"
# sc <- Gastro_Agreement_sc

# analysis_type <- "Path Agreement"
# sc <- Path_Agreement_sc
# 
# analysis_type <- "Single Timepoint Association"
# sc <- Assoc_Single_Tp_sc

# analysis_type <- "ODPTS_pre"
# sc <- ODPTS_pre_sc

analysis_type <- "ODPTS_post"
sc <- ODPTS_post_sc

# analysis_type <- "Assoc of Change Across Time"
# sc <- Assoc_Change_Across_Tps_sc

# analysis_type <- "Proportion Explained"
# sc <- Proportion_Explained_sc

# analysis_type <- "Clinical Trial Utility"
# sc <- Clinical_Utility_sc

output_dir <- output_dir_func(analysis_type,study)
mkdirs(output_dir)

tictoc::tic()
# first truncate the analysis summary csv for this new run (might have been there if multiple runs in same day)
analysis_summary <- path(output_dir, paste0("analysis-summary-", study, "-", Sys.Date(), ".csv"))
if (file.exists(analysis_summary)) {
  file.remove(analysis_summary)
}

# For testing
# this_endpoint <- "BVA"

# now do the analyses (noting that Phase0 does not have First5Percent_Mean)
tictoc::tic()
for (this_endpoint in vce_endpoints) {
  if ((analysis_type=="Path Agreement") && (this_endpoint!="First5Percent_Mean")) {
    next
  }
  sc_result <- sc(output_dir)
  if (grepl("^ODPTS", analysis_type)) {
    repo_clone_save <- repo_clone
    repo_clone <- replace_last_component(repo_clone, "rd-gi-cdpm-marcs")
    qmd <- "ODPTS.qmd"
    file.copy(path(repo_clone,"Curve-Interpretation-ML","app-specific-pre-and-post",qmd),output_dir,overwrite=TRUE)
    mkdirs(path(output_dir,"images"))
    more_images <- list.files(path=path(repo_clone,"Curve-Interpretation-ML","images"),pattern="\\.png$",full.names=TRUE)
    for (file in more_images) {
      file.copy(file,path(output_dir,"images"),overwrite=TRUE,recursive=TRUE)
    }
    file.copy(path(repo_clone,"Shared-Content","images"),output_dir,overwrite=TRUE,recursive=TRUE)
    repo_clone <- path(repo_clone,"Curve-Interpretation-ML")
  } else {
    input_dir <- path(input_base_dir,study)
  }
  
  
  # # We define the following for convenience in debugging without Quarto...
  params <- list()
  params$analysis_type <- analysis_type
  params$study <- study
  params$repo_clone <- repo_clone
  params$input_dir <- input_dir
  params$output_dir <- output_dir
  params$this_endpoint <- "BVA"
tictoc::tic()
  quarto_render(input=sc_result,
                output_format="pdf",
                execute_params=list(analysis_type=analysis_type,
                                    study=study,
                                    repo_clone=repo_clone,
                                    input_dir=input_dir,
                                    output_dir=output_dir,
                                    this_endpoint=this_endpoint),
                execute_dir=output_dir,debug=TRUE)
  tictoc::toc()
  if (!grepl("^ODPTS", analysis_type)) {
    # rename the pdf
    rename_file(path=output_dir, 
                original_filename=paste0(file_path_sans_ext(basename(sc_result)),".pdf"), 
                new_filename=paste0(file_path_sans_ext(str_trunc(basename(sc_result), 20)),
                                    "-",this_endpoint,"-",study,"-",Sys.Date(),".pdf"))
  } else {
    repo_clone <- repo_clone_save
    break
  }
}
tictoc::toc()

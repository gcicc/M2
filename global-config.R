# Global-Config: Configuration management for MARCS validation analyses
# 
# MODERNIZED: This file now uses the automated configuration system to eliminate
# manual user switching. Configuration is loaded automatically based on
# the current system user from config.yml.
#
# NEW WORKFLOW:
# 1. User configurations are stored in config.yml
# 2. System automatically detects current user (Sys.info()[["user"]])
# 3. Loads appropriate configuration without manual code changes
# 4. Validates paths and sets up Chrome environment
#
# ADDING NEW USERS:
# Simply add a new user section to config.yml - no code changes needed!
#
# LEGACY MANUAL SWITCHING (NO LONGER NEEDED):
# The old manual commenting/uncommenting system has been replaced.
# Previous manual user blocks have been removed and are now handled
# automatically via the config.yml file.

# Load configuration management system
source(file.path("Shared-Content", "config-management.R"))

# Initialize configuration automatically
# This replaces all the manual user switching with automatic detection
marcs_config <- initialize_marcs_config()

# Extract main configuration variables (for backward compatibility)
local_sync_dir <- marcs_config$local_sync_dir
repo_clone <- marcs_config$repo_clone
initials <- marcs_config$initials
studies <- marcs_config$studies

# Set default study for debugging (can be overridden)
study <- studies[5] # TAK-062-2001

# The main list is within preamble.R. Here, only the small subset needed to get into Quarto.
# Added 'config' and 'yaml' packages for new configuration system
global_pckgs = c("R.utils","fs","tictoc","this.path","tools","goft","quarto","config","yaml")#,"tinytex")

is.installed <- function(pckg) {
  is.element(pckg,installed.packages()[,1])
} 

for (pckg in global_pckgs) {
  if (!is.installed(pckg)) {
    install.packages(pckg)
  }
  library(pckg, character.only=TRUE)
}

#tinytex::install_tinytex(force=TRUE)

# Directory paths ----
input_base_dir <- path(local_sync_dir,"/Input/")
output_base_dir <- path(local_sync_dir,"/Output/")
output_dir_func <- function(analysis_type,study) {
  output_dir <- path(output_base_dir,analysis_type,paste(Sys.Date(),study,initials))
  mkdirs(output_dir)
  return(output_dir)
}
replace_last_component <- function(path, new_component) {
  str_replace(path, "/[^/]+$", paste0("/", new_component))
}

# Utility functions to process main markdown scripts by copying to the output_dir their output
# be placed in, serving both to direct quarto_render() and in so doing provide self documentation
# of what version ran at that time (also could be determined using git of course but this is easy)
Accuracy_sc <- function(to) {
  qmd <- "accuracy.qmd"
  file.copy(path(repo_clone,"Analytical-Validation","Accuracy",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
Precision_sc <- function(to) {
  qmd <- "precision.qmd"
  file.copy(path(repo_clone,"Analytical-Validation","Precision",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
Gastro_Agreement_sc <- function(to) {
  qmd <- "relative-to-gastroenterologist.qmd"
  file.copy(path(repo_clone,"CE-VAST-Agreement",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
Path_Agreement_sc <- function(to) {
  qmd <- "relative-to-pathologist.qmd"
  file.copy(path(repo_clone,"CE-VAST-Agreement",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
Assoc_Single_Tp_sc <- function(to) {
  qmd <- "associations-relative-to-accepted-clinical-endpoints.qmd"
  file.copy(path(repo_clone,"Clinical-Biological-Validation","association",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
Assoc_Change_Across_Tps_sc <- function(to) {
  qmd <- "associations-with-longitudinal-clinical-markers.qmd"
  file.copy(path(repo_clone,"Clinical-Biological-Validation","association",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
Proportion_Explained_sc <- function(to) {
  qmd <- "determination-of-the-strength-of-surrogacy.qmd"
  file.copy(path(repo_clone,"Clinical-Biological-Validation","proportion-explained",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
Clinical_Utility_sc <- function(to) {
  qmd <- "documenting-the-value-of-the-validated-biomarker.qmd"
  file.copy(path(repo_clone,"Clinical-Utility-and-Use","Accuracy",qmd),to,overwrite=TRUE)
  mkdirs(path(to,"images"))
  file.copy(path(repo_clone,"Shared-Content","images"),to,overwrite=TRUE,recursive=TRUE)
  input_dir <<- path(input_base_dir,study)
  file.copy(path(input_dir, dir(input_dir, pattern="_Study_Design")),path(to,"images","Study_Design.png"),overwrite=TRUE)
  return(path(to,qmd))
}
ODPTS_pre_sc <- function(to) {
  qmd <- "ODPTS.qmd"
  input_dir <<- output_dir_func("STA",study) # daisychains from today's run of single tp assoc
  return(path(to,qmd))
}
ODPTS_post_sc <- function(to) {
  qmd <- "ODPTS.qmd"
  input_dir <<- output_dir_func("ODPTS_post",study) # input is at same location as output due to nature of processing
  return(path(to,qmd))
}

run_quarto_with_error_handling <- function(sc,analysis_type,study,output_dir,repo_clone,input_dir,this_endpoint) {
  if ((analysis_type=="Path Agreement") && (this_endpoint!="First5Percent_Mean")) {
    return
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
    input_dir <<- path(input_base_dir,study)
  }
  tryCatch({
    quarto_render(input=sc_result,
                  output_format="pdf",
                  execute_params=list(analysis_type=analysis_type,
                                      study=study,
                                      repo_clone=repo_clone,
                                      input_dir=input_dir,
                                      output_dir=output_dir,
                                      this_endpoint=this_endpoint),
                  execute_dir=output_dir,debug=TRUE)
  }, error=function(e){}) # just allow continued running for now, can add more sophisticated handling as needed
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

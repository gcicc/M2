# Preamble: Provide means to maintain consistency across all analyses of MARCS validation. 
# Note that global-config is expected to have already been run, providing means to support
# multiple study runs, but this sets study-specific variables.

# In general, the following list will grow only slowly. If a new package is found to be
# needed during development, then it should be added to the end of the list and the
# following re-run.
pckgs = c("here","R.utils","readxl","tidyverse","ggeasy","gt","gtsummary","broom","VCA","mcr","cowplot",
          "psych", "MPsychoR","MASS","car","pracma","plyr","spatstat","GGally","webshot2","purrr","dplyr",
          "stringr","rmarkdown","sjmisc","tidyverse","readxl","readr","iterators","fs","knitr","cardx",
          "lubridate","gridExtra","git2r","gtools","kableExtra","tools","labelled","systemfit","caret",
          "ggpubr","DescTools","ggpubr","ggh4x","patchwork","striprtf","png","Rcpp","data.table","furrr",
          "future","mice","reticulate","spatstat.univar","spatstat")

is.installed <- function(pckg) {
  is.element(pckg,installed.packages()[,1])
} 

for (pckg in pckgs) {
  if (!is.installed(pckg)) {
    install.packages(pckg,repos="http://cran.us.r-project.org")
  }
  library(pckg, character.only=TRUE)
}

# Source shared r-files (to handle any study-specific processing)
shared_content_path <- path(repo_clone,"Shared-Content")
r_files <- list.files(path=shared_content_path,pattern="\\.R$",full.names=TRUE)
for (file in r_files) {
  if (!grepl(x=file,pattern="preamble")) source(file) # don't re-run self
}

label_for_doc <- function(internal_id) {
  if (internal_id=="VHCD") {
    human_readable_label_for_id <- "Vh:Cd"
  } else if (internal_id=="IELCOUNT") {
    human_readable_label_for_id <- "IEL Count"
  } else if (internal_id=="GISS"){
    human_readable_label_for_id <- "GI Severity Score"
  } else if (internal_id=="Agg_Histology") {
    human_readable_label_for_id <- "Aggregate Histology"
  } else if (internal_id=="Disease_Burden") {
    human_readable_label_for_id <- "Disease Burden"
  } else if (internal_id=="All_Mean") {
    human_readable_label_for_id <- "All (tertile) Mean"
  } else if (internal_id=="All_Max") {
    human_readable_label_for_id <- "All (tertile) Max"
  } else {
    human_readable_label_for_id <- gsub('_',' ',internal_id)
    human_readable_label_for_id <- gsub('.',' ',human_readable_label_for_id,fixed=TRUE)
  }
  
  if (exists("this_is_a_modue_where_change_is_analyzed")) {
    return(paste(human_readable_label_for_id, "Change"))
  } else {
    return(human_readable_label_for_id)
  }
}

label_for_doc.vec <- function(internal_ids) {
  sapply(internal_ids, label_for_doc)
}

# used in multiple analyses
vce_endpoints <- c("BVA","First5Percent_Mean","Tertile1_Max","Tertile2_Mean","Tertile2_Max","Tertile3_Mean","Tertile3_Max","All_Mean","All_Max")
comparators <- c("VHCD","IELCOUNT","GISS","VCIEL","Agg_Histology","Disease_Burden")
# comparators <- c("VHCD","IELCOUNT","Had_Diarrhea","Freq_Diarrhea","Had_CSBM","Freq_CSBM","Had_Ab_Pain", 
#   "Worst_Ab_Pain","Had_Bloating","Worst_Bloating","Had_Nausea","Worst_Nausea","Had_Tiredness","Worst_Tiredness", 
#   "AB_Pain_SS","Bloating_SS","CSBM","Diarrhea_SS","Nausea_SS","Tiredness_SS","GISS","NSGISS", 
#   "tTG_IgA","DGP_IgA","DGP_IgG", 
#   "VCIEL","Agg_Histology","Disease_Burden")
comparator.labels <- lapply(comparators, label_for_doc)
vce.labels <- lapply(vce_endpoints, label_for_doc)

subscript_change_labels <- function(df) {
  better_df <- df
  #for (variable in c(unlist(this_endpoint), unlist(lapply(comparators, label_for_doc)))) {
  for (variable in c(unlist(this_endpoint), unlist(comparators, label_for_doc))) {
    better_df[better_df == variable] <- paste(variable, "change")
  }
  return(better_df)
}

# for consistency in reporting
cbp2 <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
theme_set(new=theme_grey(base_size=12)) 
theme_update(axis.title=element_text(size=14)) 
theme_update(axis.text=element_text(size=12)) 
theme_update(legend.text=element_text(size=14)) 
theme_update(plot.title=element_text(size=14,face="bold")) 
theme_update(plot.subtitle=element_text(size=12)) 
theme_update(plot.caption=element_text(size=10,face="italic"))

# This bit is needed to ensure gt::gtsave works with chrome update
Sys.setenv(CHROMOTE_HEADLESS = "new")

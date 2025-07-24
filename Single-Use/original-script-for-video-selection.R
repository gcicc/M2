#----------------------------------------------------------------------------
# This script produces the original selection of videos for the MARCS study
#----------------------------------------------------------------------------

input_dir <- path(input_base_dir,"TAK-062-2001")
repo_clone <- "C:/Users/eri7441/OneDrive - Takeda/Documents/R-Projects/MARCS/"
source(paste0(repo_clone,"/global-config.R"))
source(paste0(repo_clone,"/Shared-Content/preamble.R"))
source(paste0(repo_clone,"/Shared-Content/IRT-models.R"))

# prep data----
CDSD_Histology_data <- read_excel(path(input_dir,"CDSD_Histology_data.xlsx"))

library(readxl)
AMR <- read_excel(path(input_dir, "Takeda_VCE_Anatomical_Marking_Report_15Aug2024.xlsx"))
AMR.report <- AMR %>% 
  dplyr::summarize(`Number of Videos` = n(),
                   `Cecum reached` = sum(is.na(`Was the cecum reached?`) == FALSE & `Was the cecum reached?` == "Yes"),
                   `Sufficient image quality` = 
                     sum(is.na(`Was the cecum reached?`) == FALSE & `Was the cecum reached?` == "Yes" &
                           `What was the image quality over the first third of the small bowel?` %in% c("fair", "good", "excellent") &
                           `What was the image quality over the small bowel?` %in% c("fair", "good", "excellent")),
                   `Video at Baseline or W24` = sum(is.na(`Was the cecum reached?`) == FALSE & `Was the cecum reached?` == "Yes" &
                                                      `What was the image quality over the first third of the small bowel?` %in% c("fair", "good", "excellent") &
                                                      `What was the image quality over the small bowel?` %in% c("fair", "good", "excellent") & 
                                                      `Imaging Timepoint` %in% c("VISIT 2 - Week -4", "VISIT 6 - Week 24") )
  )

VHCD <- CDSD_Histology_data %>%
  filter(`Parameter Category 1` == "ESOPHAGOGASTRODUODENOSCOPY",
         `Parameter Code` == "VHCD",
         `Analysis Visit` %in% c("Baseline", "Week 24")) %>%
  dplyr::select(
    "Study Identifier",
    "Unique Subject Identifier",
    "Subject Identifier for the Study",
    "Study Site Identifier",
    "Analysis Visit",
    "Parameter",
    "Parameter Code",
    "Analysis Value")

IEL <- CDSD_Histology_data %>%
  filter(`Parameter Category 1` == "ESOPHAGOGASTRODUODENOSCOPY",
         `Parameter Code` == "LYEPIENT",
         `Analysis Visit` %in% c("Baseline", "Week 24")) %>%
  dplyr::select(
    "Study Identifier",
    "Unique Subject Identifier",
    "Subject Identifier for the Study",
    "Study Site Identifier",
    "Analysis Visit",
    "Parameter",
    "Parameter Code",
    "Analysis Value")

GISS <- CDSD_Histology_data %>%
  filter(`Parameter` == "CDSD GI Score Weekly",
         `Analysis Visit` %in% c("Baseline", "Week 24")) %>%
  dplyr::select(
    "Study Identifier",
    "Unique Subject Identifier",
    "Subject Identifier for the Study",
    "Study Site Identifier",
    "Analysis Visit",
    "Parameter",
    "Parameter Code",
    "Analysis Value")

VHCD <- VHCD %>%
  dplyr::rename(VHCD = `Analysis Value`) %>%
  dplyr::select("Study Identifier",
                "Unique Subject Identifier",
                "Subject Identifier for the Study",
                "Study Site Identifier", 
                "Analysis Visit",  VHCD)

IEL <- IEL %>%
  dplyr::rename(IEL = `Analysis Value`) %>%
  dplyr::select("Study Identifier",
                "Unique Subject Identifier",
                "Subject Identifier for the Study",
                "Study Site Identifier", 
                "Analysis Visit", IEL)

GISS <- GISS %>%
  dplyr::rename(GISS = `Analysis Value`) %>%
  dplyr::select("Study Identifier",
                "Unique Subject Identifier",
                "Subject Identifier for the Study",
                "Study Site Identifier", 
                "Analysis Visit", GISS)

working.df <- VHCD %>%
  left_join(IEL) %>%
  left_join(GISS)


eligible.videos <- AMR %>% dplyr::filter(
  `Imaging Timepoint` %in% c("VISIT 2 - Week -4", "VISIT 6 - Week 24"),
  is.na(`Was the cecum reached?`) == FALSE,
  `Was the cecum reached?` == "Yes",
  `What was the image quality over the first third of the small bowel?` %in% c("fair", "good", "excellent"),
  `What was the image quality over the small bowel?` %in% c("fair", "good", "excellent")
) %>%
  dplyr::rename(`Analysis Visit` =`Imaging Timepoint`) %>%
  mutate(`Analysis Visit` = ifelse(`Analysis Visit` == "VISIT 2 - Week -4", "Baseline", "Week 24")) %>%
  dplyr::select(c("Protocol", "Site Number", "Subject ID", "Analysis Visit")) %>% 
  dplyr::mutate(`Site Number` = as.character(`Site Number`))

working.df <- working.df %>%
  filter(is.na(GISS) == FALSE, is.na(VHCD) == FALSE)

for.IRT <- working.df %>% 
  mutate(VHCD.neg = -VHCD) %>%
  mutate(VHCD = (VHCD - mean(VHCD)) / sd(VHCD),
         VHCD.neg = (VHCD.neg - mean(VHCD.neg)) / sd(VHCD.neg),
         IEL  = (IEL - mean(IEL)) / sd(IEL),
         GISS = (GISS - mean(GISS, na.rm = T)) / sd(GISS, na.rm = T)) %>%
  dplyr::rename(SUBJID = `Subject Identifier for the Study`)%>% 
  dplyr::filter(is.na(VHCD) == FALSE, is.na(GISS) == FALSE, is.na(IEL) == FALSE)

# run model ----
common.alpha.model.out <- common.alpha.model(data.in = for.IRT %>% dplyr::select(SUBJID, VHCD.neg, IEL, GISS))

# Grab esttheta from here
PostEst.out.common <- PosteriorEstimates(
  grid = seq(-3, 5, .1),
  dat = for.IRT %>% dplyr::select(SUBJID, VHCD.neg, IEL, GISS),
  I = 3,
  indicator = "C",
  lin = "lin",
  parmatrest = common.alpha.model.out$parmatrix,
  stdest = common.alpha.model.out$stdmixt
)



IRT.summary <- for.IRT %>%
  mutate(esttheta = PostEst.out.common$esttheta) %>%
  mutate(IRT.group = factor(case_when(
    esttheta < quantile(esttheta, .33) ~ "Low",
    esttheta > quantile(esttheta, .67) ~ "High",
    .default = "Moderate"
  ), c("Low", "Moderate", "High"))) 

IRT.summary.subset <-   IRT.summary %>% 
#  dplyr::select(`Study Identifier`, `Study Site Identifier`, `SUBJID`, `Analysis Visit`, IRT.group) %>%
  dplyr::rename(Protocol = `Study Identifier`,
                `Site Number` = `Study Site Identifier`,
                `Subject ID` = `SUBJID`)
# 123 rows
# eligible.videos.AMR_df: 227
# IRT.summary.subsetL 201
# Original scrip ran with this
Videos.IRT <- eligible.videos %>% left_join(IRT.summary.subset) %>% arrange(`Subject ID`, `Analysis Visit`) %>% dplyr::filter(is.na(IRT.group) == FALSE)

# NB: This code should have been run with
# Videos.IRT.correction.1 <- IRT.summary.subset %>% left_join(eligible.videos) %>% arrange(`Subject ID`, `Analysis Visit`) 

set.seed(8675309)
High <- Videos.IRT %>% 
  dplyr::filter(IRT.group == "High") %>% 
  sample_n(size = n()) %>%
  group_by(`Subject ID`) %>% slice(1) 

Low <- Videos.IRT %>% dplyr::filter(IRT.group == "Low", 
                                    !(`Subject ID` %in% High$`Subject ID`)) %>%
  sample_n(size = n()) %>%
  group_by(`Subject ID`) %>% slice(1) %>% ungroup() %>%
  sample_n(size = 24)

Moderate <- Videos.IRT %>% dplyr::filter(IRT.group == "Moderate", 
                                         !(`Subject ID` %in% union(High$`Subject ID`,Low$`Subject ID`)))  %>%
  sample_n(size = n()) %>%
  group_by(`Subject ID`) %>% slice(1) %>% ungroup() %>%
  sample_n(size = 26)


for.vendor <- bind_rows(High, Low, Moderate) %>% sample_n(size = n()) %>% dplyr::select(- IRT.group) %>%
  dplyr::mutate(`Analysis Visit` = ifelse(`Analysis Visit` == "Baseline", "VISIT 2 - Week -4", "VISIT 6 - Week 24"))

write.csv(for.vendor, "Patient-Selection-for-Set-A.csv")


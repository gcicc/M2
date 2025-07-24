# Bring in analysis.data.in from associations-relative-to-accepted-clinical-endpoints.qmd

names(analysis.data.in)
# R2 of 1... so severity score is a linear combo of the two sub measures
summary(lm(Diarrhea_SS ~ Had_Diarrhea + Freq_Diarrhea, analysis.data.in))
summary(lm(CSBM ~ Had_CSBM + Freq_CSBM, analysis.data.in))
summary(lm(AB_Pain_SS ~ Had_Ab_Pain + Worst_Ab_Pain, analysis.data.in))
summary(lm(Bloating_SS ~ Had_Bloating + Worst_Bloating, analysis.data.in))
summary(lm(Nausea_SS ~ Had_Nausea + Worst_Nausea, analysis.data.in))
summary(lm(Tiredness_SS ~ Had_Tiredness + Worst_Tiredness, analysis.data.in))

# these have r2 values greater than .7, so there's redundancy...
summary(lm(Freq_Diarrhea ~ Had_Diarrhea , analysis.data.in))
ggplot(aes(x=Had_Diarrhea, y=Freq_Diarrhea), data=analysis.data.in) + geom_point() + geom_smooth(method="lm")

summary(lm(Freq_CSBM ~ Had_CSBM, analysis.data.in))
ggplot(aes(x=Had_CSBM, y=Freq_CSBM), data=analysis.data.in) + geom_point() + geom_smooth(method="lm")

summary(lm(Worst_Ab_Pain ~ Had_Ab_Pain, analysis.data.in))
ggplot(aes(x=Had_Ab_Pain, y=Worst_Ab_Pain), data=analysis.data.in) + geom_point() + geom_smooth(method="lm")

summary(lm(Worst_Bloating ~ Had_Bloating , analysis.data.in))
ggplot(aes(x=Had_Bloating, y=Worst_Bloating), data=analysis.data.in) + geom_point() + geom_smooth(method="lm")

summary(lm(Worst_Nausea ~ Had_Nausea, analysis.data.in))
ggplot(aes(x=Had_Nausea, y=Worst_Nausea), data=analysis.data.in) + geom_point() + geom_smooth(method="lm")

summary(lm(Worst_Tiredness ~ Had_Tiredness, analysis.data.in))
ggplot(aes(x=Had_Tiredness, y=Worst_Tiredness), data=analysis.data.in) + geom_point() + geom_smooth(method="lm")



## WINNER
summary(lm(GISS ~ Worst_Ab_Pain + Worst_Bloating + Worst_Nausea + Freq_Diarrhea, analysis.data.in))
summary(lm(GISS ~ Freq_Diarrhea+Worst_Bloating+Worst_Nausea, analysis.data.in))
summary(lm(GISS ~ Freq_Diarrhea+Worst_Bloating, analysis.data.in))
summary(lm(GISS ~ Worst_Ab_Pain+Worst_Bloating, analysis.data.in))

## Even better fit --- what is NSGISS????
summary(lm(NSGISS ~ Worst_Ab_Pain + Worst_Bloating + Worst_Nausea, analysis.data.in))
## TotalScore
summary(lm(TotalScore ~ Worst_Ab_Pain + Worst_Bloating + Worst_Nausea, analysis.data.in))
# Pretty good...
summary(lm(TotalScore ~ Worst_Ab_Pain + Worst_Bloating + Worst_Nausea + Freq_CSBM, analysis.data.in))
# Nearly perfect...
summary(lm(TotalScore ~ Worst_Ab_Pain + Worst_Bloating + Worst_Nausea + Freq_Diarrhea + Freq_CSBM, analysis.data.in))
# Nearly perfect...
summary(lm(TotalScore ~ Worst_Ab_Pain + Worst_Bloating + Worst_Nausea + Freq_Diarrhea + Freq_CSBM+Worst_Tiredness, analysis.data.in))

library(MASS)
for.regression <- c("GISS", "Had_Diarrhea", "Freq_Diarrhea", "Had_CSBM", "Freq_CSBM", "Had_Ab_Pain", "Worst_Ab_Pain", "Had_Bloating", 
                    "Worst_Bloating", "Had_Nausea", "Worst_Nausea", "Had_Tiredness", "Worst_Tiredness")
for.regression.df <- analysis.data.in[, for.regression] %>% na.omit()
full_model <- lm(GISS ~ ., for.regression.df)
best_model <- stepAIC(full_model, direction = "both", trace = TRUE)


for.regression <- c("NSGISS", "Had_Diarrhea", "Freq_Diarrhea", "Had_CSBM", "Freq_CSBM", "Had_Ab_Pain", "Worst_Ab_Pain", "Had_Bloating", 
                    "Worst_Bloating", "Had_Nausea", "Worst_Nausea", "Had_Tiredness", "Worst_Tiredness")
for.regression.df <- analysis.data.in[, for.regression] %>% na.omit()
full_model <- lm(NSGISS ~ ., for.regression.df)
# Did better earlier
best_model <- stepAIC(full_model, direction = "both", trace = TRUE)


for.regression <- c("TotalScore", "Had_Diarrhea", "Freq_Diarrhea", "Had_CSBM", "Freq_CSBM", "Had_Ab_Pain", "Worst_Ab_Pain", "Had_Bloating", 
                    "Worst_Bloating", "Had_Nausea", "Worst_Nausea", "Had_Tiredness", "Worst_Tiredness")
for.regression.df <- analysis.data.in[, for.regression] %>% na.omit()
full_model <- lm(TotalScore ~ ., for.regression.df)
# Did better earlier
best_model <- stepAIC(full_model, direction = "both", trace = TRUE)




#--------------------


for.regression <- c("TotalScore", "Had_Diarrhea", "Freq_Diarrhea", "Had_CSBM", "Freq_CSBM", "Had_Ab_Pain", "Worst_Ab_Pain", "Had_Bloating", 
                    "Worst_Bloating", "Had_Nausea", "Worst_Nausea", "Had_Tiredness", "Worst_Tiredness")
for.regression.df <- analysis.data.in[, for.regression] %>% na.omit()
data <- for.regression.df

# Ensure the data frame is not empty and contains columns
if (is.null(ncol(data)) || ncol(data) <= 1) {
  stop("Data frame does not have enough columns")
}


# Perform subset selection
subset_selection <- regsubsets(TotalScore ~ ., data = data, nvmax = ncol(data) - 1)

# Summarize the results
selection_summary <- summary(subset_selection)

# Function to get the best model for each number of covariates based on Adjusted R²
get_best_model <- function(summary, index) {
  best_index <- which.max(summary$adjr2[index])
  model <- coef(subset_selection, best_index)
  return(model)
}

# Initialize a list to store results
results <- data.frame(
  Num_Covariates = integer(),
  Adjusted_R2 = numeric(),
  Covariates = character()
)

# Loop over the number of covariates ensuring we handle any potential errors properly
for (i in 1:(ncol(data) - 1)) {
  model <- get_best_model(selection_summary, 1:i)
  adjusted_r2 <- selection_summary$adjr2[which.max(selection_summary$adjr2[1:i])]
  covariates <- paste(names(model[-1]), collapse = ", ")
  
  results <- rbind(results, data.frame(
    Num_Covariates = i,
    Adjusted_R2 = adjusted_r2,
    Covariates = covariates
  ))
}

# Print the results as a data.frame
results %>% dplyr::select(Covariates, Adjusted_R2) %>% slice(1:5) %>% gt() %>% fmt_number(columns = 2, decimals = 3) %>%
  tab_header(title = "Best Models for TotalScore")

print(results)











#------------------

temp <- analysis.data.in %>% dplyr::mutate(key=paste(STUDYID, SITE, SUBJID))
 temp %>% dplyr::filter(key==unique(temp$key)[1])

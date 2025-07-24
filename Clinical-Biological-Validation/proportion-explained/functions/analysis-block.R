# main_analysis for Clinical-Biological-Validation\proportion-explained
# This function performs the majority of the analysis calculations. it is called multiple
# times with filtered data to support different configurations, e.g., differing IQ levels,
# ML vs. Human, etc.

main_analysis <- function(analysis.data.in, reader_type, this_endpoint) {
  for.return <- list()
  write.csv(analysis.data.in, path(output_dir, paste0("8 analysis.data.in-", reader_type, "-", this_endpoint, ".csv")), row.names=FALSE)

  # Initialize list to hold the results, where elements of each list will be for the comparators
  surrog.full <- list()
  model.full <- list()
  fig.full <- list()
  surrog.train <- list()
  model.train <- list()
  fig.train <- list()
  model.fit.out <- list()

  analysis.data.in.original <- analysis.data.in %>% 
    ungroup() %>%
    dplyr::mutate(VHCD=-VHCD) %>% 
    dplyr::mutate(across(where(is.numeric), ~ (. - mean(., na.rm=TRUE)) / sd(., na.rm=TRUE)))
  
  # Let S and T denote the surrogate and true endpoints, respectively, and let Z be an indicator
  # variable for treatment. We assume that S and T or appropriate variance stabilizing
  # transformations of them, are normally distributed and that the following bivariate
  # regression model holds
  
  # Assume analysis.data.in is your dataset with relevant variables

  for (x in (1:length(comparators))) {
    temp.in <- analysis.data.in.original %>% ungroup() %>% dplyr::select(ARM, all_of(this_endpoint), comparators[x]) %>% na.omit()
    
    # the analysis is perfomed in three steps:
    # Step 1: full data set
    surrog.statistics.full <- compute.surrog.statistics(data.in=temp.in, this_endpoint=this_endpoint, comparatorT=comparators[x], reader_type=reader_type, dataset.descript="Full data") 
    prentice.figure.list.full <- prentice.fig.suite(data.in=temp.in, this_endpoint=this_endpoint, comparatorT=comparators[x], model.in=surrog.statistics.full, dataset.descript="Full data")

    surrog.full <- append(surrog.full, list(surrog.statistics.full$altogether))
    model.full <- append(model.full, list(surrog.statistics.full$model.report))
    new_element <- setNames(list(prentice.figure.list.full), comparators[x])
    fig.full <- append(fig.full, new_element)
    
    # Step 2: split data into training (20%) and testing (80%) sets
    if (nrow(analysis.data.in.original) > 50) {
      trainIndex <- createDataPartition(y=temp.in %>% dplyr::pull(this_endpoint), p=0.2, list=FALSE)
      trainData <- temp.in[trainIndex, ]
      testData <- temp.in[-trainIndex, ]

      # with the 20% test data we can report the same provided sample size is sufficiently large
      surrog.statistics.train <- compute.surrog.statistics(data.in=trainData, this_endpoint=this_endpoint, comparatorT=comparators[x], reader_type=reader_type, dataset.descript="Training data")
      prentice.figure.list.train <- prentice.fig.suite(data.in=temp.in, this_endpoint=this_endpoint, comparatorT=comparators[x], model.in=surrog.statistics.train, dataset.descript="Train data")
      
      surrog.train <- append(surrog.train, list(surrog.statistics.train$altogether))
      model.train <- append(model.train, list(surrog.statistics.train$model.report))
      new_element <- setNames(list(prentice.figure.list.train), comparators[x])
      fig.train <- append(fig.train,new_element)

      # Step 3: test the trained model on the test data
      predictions_full <- predict(surrog.statistics.train$fit.SUR, newdata=testData)
      
      # Compute root mean squared error (RMSE) and mean absolute error (MAE)
      rmse.vce <- caret::RMSE(predictions_full$Surrogate.pred, testData %>% dplyr::pull(this_endpoint))
      rmse <- caret::RMSE(predictions_full$True.pred, testData %>% dplyr::pull(comparators[x]))
      mae.vce <- caret::MAE(predictions_full$Surrogate.pred, testData %>% dplyr::pull(this_endpoint))
      mae <- caret::MAE(predictions_full$True.pred, testData %>% dplyr::pull(comparators[x]))
      
      Model.fit.metrics <- data.frame(Metric=c("RMSE", "RMSE", "MAE", "MAE"), 
                                      Endpoint=c(this_endpoint, comparators[x], this_endpoint, comparators[x]), 
                                      Value=c(rmse.vce, rmse, mae.vce, mae),
                                      model.components=paste0("Surrogate:", this_endpoint, "; Comparator:", comparators[x])) %>% 
        dplyr::mutate(reader_type=reader_type)
      
      # Compute residuals for the test data
      residuals <- data.frame(residuals.vce=predictions_full$this_endpoint.pred - testData[[this_endpoint]],
                              residuals.Disease_Burden=predictions_full$Latent.pred - testData %>% dplyr::pull(comparators[x]))
      
      model.fit.out <- append(model.fit.out, list(Model.fit.metrics))
    } # if there are enough rows to support a train/test split
  } # for each comparator
  
  # Combine all surrog.statistics.full data frames using bind_rows
  for.return$full.data <- analysis.data.in
  for.return$surrog.full <- bind_rows(surrog.full)
  for.return$model.full <- bind_rows(model.full)
  for.return$fig.full <- fig.full

  if (nrow(analysis.data.in.original) > 50) {
    for.return$trainData <- trainData
    for.return$surrog.train <- bind_rows(surrog.train)
    for.return$model.train <- bind_rows(model.train)
    for.return$fig.train <- fig.train

    for.return$model.fit.out <- bind_rows(model.fit.out)
  } else {
    for.return$trainData <- NULL
    for.return$surrog.train <- NULL
    for.return$model.train <- NULL
    for.return$fig.train <- NULL

    for.return$model.fit.out <- NULL
  }

  return(for.return)
}

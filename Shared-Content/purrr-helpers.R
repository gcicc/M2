#' Purrr Helper Functions for MARCS Analysis Pipeline
#'
#' This file provides functional programming helpers to modernize the MARCS
#' analysis pipeline, replacing manual loops and imperative patterns with
#' clean, readable purrr-based functional programming.

#' Process Merged Data Sets with Functional Pipeline
#'
#' Modernizes the common pattern of looping over merged_data_sets to process
#' each IQ level configuration. Replaces manual list building with clean
#' functional programming.
#'
#' @param merged_data_sets_output Output from form_merged_data_sets()
#' @param analysis_function Function to apply to each data set (e.g., main_analysis)
#' @param data_prep_function Optional function to prepare data before analysis
#' @param ... Additional arguments passed to analysis_function
#'
#' @return Named list with analysis results for each IQ level
#'
#' @details
#' This function replaces the common imperative pattern:
#' ```r
#' results.ML <- list()
#' for (i in 1:length(form_merged_data_sets_output$merged_data_sets)) {
#'   merged_data_set <- form_merged_data_sets_output$merged_data_sets[[i]]
#'   label.name <- names(form_merged_data_sets_output$merged_data_sets)[i]
#'   # ... processing ...
#'   results.ML[[length(results.ML)+1]] <- results
#'   names(results.ML)[length(results.ML)] <- label.name
#' }
#' ```
#'
#' With clean functional programming:
#' ```r
#' results.ML <- process_merged_data_sets(
#'   form_merged_data_sets_output, 
#'   main_analysis,
#'   prepare_combined_data,
#'   this_endpoint = this_endpoint
#' )
#' ```
#'
#' @examples
#' \dontrun{
#'   # Accuracy analysis
#'   results.ML <- process_merged_data_sets(
#'     form_merged_data_sets_output,
#'     main_analysis,
#'     prepare_combined_data,
#'     this_endpoint = this_endpoint
#'   )
#'   
#'   # Precision analysis
#'   results.ML <- process_merged_data_sets(
#'     form_merged_data_sets_output,
#'     main_analysis,
#'     prepare_precision_data,
#'     reader_type = "ML",
#'     this_endpoint = this_endpoint
#'   )
#' }
#'
#' @export
process_merged_data_sets <- function(merged_data_sets_output, 
                                   analysis_function, 
                                   data_prep_function = NULL, 
                                   ...) {
  
  # Load purrr if not already loaded
  if (!require("purrr", quietly = TRUE)) {
    stop("purrr package required. Install with: install.packages('purrr')")
  }
  
  merged_data_sets_output$merged_data_sets %>%
    imap(function(merged_data_set, label_name) {
      
      # Apply data preparation function if provided
      if (!is.null(data_prep_function)) {
        analysis_data <- data_prep_function(merged_data_set, label_name)
      } else {
        # Default: combine ML and Human data
        analysis_data <- prepare_combined_data(merged_data_set, label_name)
      }
      
      # Apply analysis function
      analysis_function(analysis_data, ...)
    })
}

#' Prepare Combined ML and Human Data
#'
#' Standard data preparation function that combines ML and Human reader data
#' with proper labeling. This is the most common data preparation pattern.
#'
#' @param merged_data_set Single merged data set from form_merged_data_sets
#' @param label_name IQ level label name
#'
#' @return Combined data frame with ML and Human data
#'
#' @export
prepare_combined_data <- function(merged_data_set, label_name) {
  
  if (!require("dplyr", quietly = TRUE)) {
    stop("dplyr package required")
  }
  
  # Extract and label ML data
  analysis_data_ml <- merged_data_set$merged_data.ML$merged_data.ML.full %>%
    mutate(label.name = label_name)
  
  # Extract and label Human data
  analysis_data_human <- merged_data_set$merged_data.Human$merged_data.Human.full %>%
    mutate(label.name = label_name)
  
  # Combine datasets
  bind_rows(analysis_data_ml, analysis_data_human)
}

#' Prepare Precision Analysis Data
#'
#' Specialized data preparation for precision analysis that handles the
#' complex retest flag logic and study-specific processing.
#'
#' @param merged_data_set Single merged data set from form_merged_data_sets
#' @param label_name IQ level label name
#' @param study Study identifier for study-specific logic
#'
#' @return Prepared data frame for precision analysis
#'
#' @export
prepare_precision_data <- function(merged_data_set, label_name, study = NULL) {
  
  if (!require("dplyr", quietly = TRUE)) {
    stop("dplyr package required")
  }
  
  # Get study from global environment if not provided
  if (is.null(study) && exists("study", envir = .GlobalEnv)) {
    study <- get("study", envir = .GlobalEnv)
  }
  
  if (study != "Sheffield") {
    # TAK-062-2001 logic
    analysis_data_temp <- merged_data_set$merged_data.ML$merged_data.ML.full %>%
      mutate(label.name = label_name) %>%
      filter(VISIT == "VISIT 2 - Week -4")
    
    # Handle retest flag logic
    retest_true <- analysis_data_temp %>% 
      filter(Retest.Flag == TRUE)
    
    retest_true_subjids <- retest_true$SUBJID
    
    retest_false_matched <- analysis_data_temp %>%
      filter(Retest.Flag == FALSE & SUBJID %in% retest_true_subjids)
    
    bind_rows(retest_true, retest_false_matched)
    
  } else {
    # Sheffield logic
    analysis_data_ml <- merged_data_set$merged_data.ML$merged_data.ML.full %>%
      mutate(label.name = label_name) %>%
      filter(!is.na(BVA))
    
    analysis_data_human <- merged_data_set$merged_data.Human$merged_data.Human.full %>%
      mutate(label.name = label_name) %>%
      filter(!is.na(BVA))
    
    bind_rows(analysis_data_ml, analysis_data_human)
  }
}

#' Process Stratified Analysis with Functional Programming
#'
#' Modernizes the apply(matrix(1:length(...))) pattern commonly used for
#' stratified analyses across different grouping variables.
#'
#' @param data Input data frame
#' @param stratify_by Column name to stratify analysis by
#' @param analysis_function Function to apply to each stratum
#' @param ... Additional arguments passed to analysis_function
#'
#' @return Named list with results for each stratum
#'
#' @details
#' Replaces patterns like:
#' ```r
#' apply(matrix(1:length(unique(data$stratum))), 1, function(y) {
#'   temp <- data %>% filter(stratum == unique(data$stratum)[y])
#'   analysis_function(temp, ...)
#' })
#' ```
#'
#' With clean functional programming:
#' ```r
#' process_stratified_analysis(data, "stratum", analysis_function, ...)
#' ```
#'
#' @export
process_stratified_analysis <- function(data, stratify_by, analysis_function, ...) {
  
  if (!require("purrr", quietly = TRUE) || !require("dplyr", quietly = TRUE)) {
    stop("purrr and dplyr packages required")
  }
  
  data %>%
    split(.[[stratify_by]]) %>%
    map(~ analysis_function(.x, ...)) %>%
    set_names(names(.))
}

#' Create Analysis Results Pipeline
#'
#' Creates a complete analysis pipeline that combines data preparation,
#' analysis execution, and result formatting in a clean functional style.
#'
#' @param merged_data_sets_output Output from form_merged_data_sets()
#' @param pipeline_config List containing pipeline configuration:
#'   - data_prep: Data preparation function
#'   - analysis: Analysis function  
#'   - post_process: Optional post-processing function
#' @param ... Additional arguments passed to analysis functions
#'
#' @return Processed analysis results
#'
#' @examples
#' \dontrun{
#'   # Define pipeline configuration
#'   accuracy_pipeline <- list(
#'     data_prep = prepare_combined_data,
#'     analysis = main_analysis,
#'     post_process = NULL
#'   )
#'   
#'   # Execute pipeline
#'   results <- create_analysis_pipeline(
#'     form_merged_data_sets_output,
#'     accuracy_pipeline,
#'     this_endpoint = this_endpoint
#'   )
#' }
#'
#' @export
create_analysis_pipeline <- function(merged_data_sets_output, pipeline_config, ...) {
  
  if (!require("purrr", quietly = TRUE)) {
    stop("purrr package required")
  }
  
  # Validate pipeline configuration
  required_elements <- c("data_prep", "analysis")
  missing_elements <- setdiff(required_elements, names(pipeline_config))
  
  if (length(missing_elements) > 0) {
    stop(paste("Missing pipeline configuration elements:", 
               paste(missing_elements, collapse = ", ")))
  }
  
  # Execute pipeline
  results <- merged_data_sets_output$merged_data_sets %>%
    imap(function(merged_data_set, label_name) {
      
      # Data preparation
      prepared_data <- pipeline_config$data_prep(merged_data_set, label_name)
      
      # Analysis
      analysis_results <- pipeline_config$analysis(prepared_data, ...)
      
      # Post-processing (if specified)
      if (!is.null(pipeline_config$post_process)) {
        analysis_results <- pipeline_config$post_process(analysis_results, label_name)
      }
      
      analysis_results
    })
  
  return(results)
}

#' Apply Function Over Named List Elements
#'
#' Convenience wrapper for common pattern of applying functions over 
#' named list elements while preserving names.
#'
#' @param .x Named list to iterate over
#' @param .f Function to apply
#' @param ... Additional arguments passed to .f
#'
#' @return Named list with function applied to each element
#'
#' @export
map_named <- function(.x, .f, ...) {
  if (!require("purrr", quietly = TRUE)) {
    stop("purrr package required")
  }
  
  .x %>%
    imap(.f, ...) %>%
    set_names(names(.x))
}

#' Convert Legacy apply(matrix()) Pattern to Purrr
#'
#' Helper function to convert the common legacy pattern of apply(matrix(1:length(...)))
#' to modern purrr functional programming.
#'
#' @param items Vector or list to iterate over
#' @param .f Function to apply to each item
#' @param use_names Logical, whether to preserve names (default: TRUE)
#' @param ... Additional arguments passed to .f
#'
#' @return List with function applied to each item
#'
#' @details
#' Converts patterns like:
#' ```r
#' apply(matrix(1:length(items)), 1, function(x) {
#'   item <- items[x]
#'   process_item(item)
#' })
#' ```
#'
#' To:
#' ```r
#' legacy_apply_to_purrr(items, process_item)
#' ```
#'
#' @export
legacy_apply_to_purrr <- function(items, .f, use_names = TRUE, ...) {
  
  if (!require("purrr", quietly = TRUE)) {
    stop("purrr package required")
  }
  
  if (use_names && !is.null(names(items))) {
    items %>% imap(.f, ...)
  } else {
    items %>% map(.f, ...)
  }
}

#' Process Association Analysis with Functional Programming
#'
#' Specialized helper for the complex association analysis that handles both
#' ML and Human readers with extensive result collection. This modernizes
#' the most complex nested loop pattern in the codebase.
#'
#' @param merged_data_sets_output Output from form_merged_data_sets()
#' @param analysis_function Main analysis function (typically main_analysis)
#' @param reader_types Vector of reader types to process (default: c("ML", "Human"))
#' @param ... Additional arguments passed to analysis_function
#'
#' @return Named list with structured results for both ML and Human readers
#'
#' @details
#' This function replaces the most complex nested loop pattern in the codebase:
#' - Outer loop over IQ levels
#' - Inner loop over reader types (ML, Human)
#' - Extensive manual list building for multiple result types
#' - Conditional logic for stratified analyses
#'
#' The function returns a structured list with separate sections for ML and Human
#' results, each containing all the various analysis outputs.
#'
#' @examples
#' \dontrun{
#'   # Association analysis
#'   results <- process_association_analysis(
#'     form_merged_data_sets_output,
#'     main_analysis,
#'     reader_types = c("ML", "Human"),
#'     this_endpoint = this_endpoint,
#'     runMI = FALSE
#'   )
#'   
#'   # Access ML results
#'   cor.report.ML <- results$ML$cor.report
#'   scatterplots.ML <- results$ML$scatterplots
#'   
#'   # Access Human results  
#'   cor.report.Human <- results$Human$cor.report
#' }
#'
#' @export
process_association_analysis <- function(merged_data_sets_output, 
                                       analysis_function,
                                       reader_types = c("ML", "Human"),
                                       ...) {
  
  if (!require("purrr", quietly = TRUE) || !require("dplyr", quietly = TRUE)) {
    stop("purrr and dplyr packages required")
  }
  
  # Helper function to extract data for a specific reader type
  extract_reader_data <- function(merged_data_set, reader_type, label_name) {
    if (reader_type == "ML") {
      merged_data_set$merged_data.ML$merged_data.ML.full %>% 
        mutate(label.name = label_name)
    } else if (reader_type == "Human") {
      if (nrow(merged_data_set$merged_data.Human$merged_data.Human.full) > 10) {
        merged_data_set$merged_data.Human$merged_data.Human.full %>% 
          mutate(label.name = label_name)
      } else {
        NULL  # Skip if insufficient Human data
      }
    }
  }
  
  # Helper function to extract all result components
  extract_result_components <- function(results) {
    list(
      cor.report = results$cor.report,
      scatterplots = results$scatterplots,
      roc.report = results$roc.report,
      roc.figure = results$roc.figure,
      sumstats = results$sumstats,
      analysis.data = results$analysis.data,
      cross.sectional.tests = results$cross.sectional.tests,
      
      # Stratified results (may be NULL)
      cor.report.byAggHist = results$cor.report.byAggHist,
      scatterplots.byAggHist = results$scatterplots.byAggHist,
      roc.report.byAggHist = results$roc.report.byAggHist,
      roc.figure.byAggHist = results$roc.figure.byAggHist,
      
      cor.report.byMO = results$cor.report.byMO,
      scatterplots.byMO = results$scatterplots.byMO,
      roc.report.byMO = results$roc.report.byMO,
      roc.figure.byMO = results$roc.figure.byMO,
      
      cor.report.byqM = results$cor.report.byqM,
      scatterplots.byqM = results$scatterplots.byqM,
      roc.report.byqM = results$roc.report.byqM,
      roc.figure.byqM = results$roc.figure.byqM,
      
      cor.report.byRandStat = results$cor.report.byRandStat,
      scatterplots.byRandStat = results$scatterplots.byRandStat,
      roc.report.byRandStat = results$roc.report.byRandStat,
      roc.figure.byRandStat = results$roc.figure.byRandStat,
      
      cor.report.byPairing = results$cor.report.byPairing,
      scatterplots.byPairing = results$scatterplots.byPairing,
      roc.report.byPairing = results$roc.report.byPairing,
      roc.figure.byPairing = results$roc.figure.byPairing,
      
      cor.report.byArm = results$cor.report.byArm,
      scatterplots.byArm = results$scatterplots.byArm,
      roc.report.byArm = results$roc.report.byArm,
      roc.figure.byArm = results$roc.figure.byArm
    )
  }
  
  # Process each reader type
  results_by_reader <- map(reader_types, function(reader_type) {
    
    # Process each IQ level for this reader type
    iq_results <- merged_data_sets_output$merged_data_sets %>%
      imap(function(merged_data_set, label_name) {
        
        # Extract data for this reader type
        analysis_data <- extract_reader_data(merged_data_set, reader_type, label_name)
        
        # Skip if no data available (e.g., insufficient Human data)
        if (is.null(analysis_data)) {
          return(NULL)
        }
        
        # Run analysis
        results <- analysis_function(
          analysis.data.in = analysis_data,
          reader_type = reader_type,
          ...
        )
        
        # Extract all components
        extract_result_components(results)
      }) %>%
      # Remove NULL entries (where data was insufficient)
      discard(is.null)
    
    # Reorganize results by component type
    if (length(iq_results) > 0) {
      component_names <- names(iq_results[[1]])
      
      map(component_names, function(component) {
        map(iq_results, ~ .x[[component]]) %>%
          # Remove NULL entries for stratified analyses
          discard(is.null) %>%
          # Set names to IQ levels
          set_names(names(iq_results))
      }) %>%
      set_names(component_names)
    } else {
      # Return empty lists if no data processed
      list()
    }
    
  }) %>%
  set_names(reader_types)
  
  return(results_by_reader)
}
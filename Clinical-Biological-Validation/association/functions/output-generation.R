# Output generation functions for clinical associations and ROC analysis

# Executive Summary
#' Generate Executive Association Metrics
#'
#' This function takes the analysis output and generates formatted metrics. It processes the data,
#' writes CSV files for the analysis, and generates a table with formatted results.
#'
#' @param analysis_output.in List of data frames containing the analysis output.
#' @return Formatted output object.
#' @import dplyr
#' @import gt
#' @importFrom dplyr mutate bind_rows arrange rename filter select everything group_by
#' @importFrom gt cols_merge cols_label cols_align fmt_number tab_header gtsave
generate_exec_assoc_metrics <- function(analysis_output.in) {
  
  formatted.out <- NULL
  
  # Check if the input has more than 0 elements
  if (length(analysis_output.in) > 0) {
    
    # If there is only one element in the input list
    if (length(analysis_output.in) == 1) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(`Image.Quality` = "")
      )
      
      # If there are two elements in the input list
    } else if (length(analysis_output.in) == 2) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(`Image.Quality` = names(analysis_output.in)[1]),
        analysis_output.in[[2]] %>% mutate(`Image.Quality` = names(analysis_output.in)[2])
      )
    }
    
    # Extract reader type for use in filenames
    reader_type <- temp$reader_type[1]
    
    # Write the temporary results to a CSV file
    write.csv(temp, file = path(table_path, paste0("cor.exec-", params$this_endpoint, "-", reader_type, ".csv")))
    
    # Append additional information to another CSV file
    append.csv(
      temp %>% dplyr::mutate(stratum = "<pooled>"),
      path = path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv"))
    )
    
    # Apply label formatting to the Comparator column
    temp$Comparator <- label_for_doc.vec(temp$Comparator)
    
    # Generate formatted output table
    formatted.out <- temp %>%
      arrange(`p.value`, Comparator, Statistic, `Image.Quality`) %>%
      dplyr::rename(`p-value` = p.value) %>%
      dplyr::filter(Statistic == "Spearman") %>%
      dplyr::select(-c(Endpoint, Statistic, reader_type)) %>%
      dplyr::select(Comparator, `Image.Quality`, everything()) %>%
      group_by(Image.Quality) %>%
      gt() %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything()) %>%
      fmt_number(columns = c(Estimate, LCL, UCL, `p-value`), decimals = 2) %>%
      tab_header(
        title = paste("Spearman Associations for", label_for_doc(params$this_endpoint)),
        subtitle = paste0("for ", reader_type, " reads")
      )
    
    # Save the formatted table as an image
    gt::gtsave(
      data = formatted.out,
      filename = path(table_path, paste0("cor.exec-", params$this_endpoint, "-", reader_type, ".formatted.png"))
    )
  }
  
  return(formatted.out)
}

# Spearman/Pearson
#' Generate Association Metrics
#'
#' This function takes the analysis output and generates formatted metrics. It processes the data,
#' writes CSV files for the analysis, and generates a table with formatted results.
#'
#' @param analysis_output.in List of data frames containing the analysis output.
#' @return Formatted output object.
#' @import dplyr
#' @import gt
#' @importFrom dplyr mutate bind_rows arrange rename filter select everything group_by
#' @importFrom gt cols_merge cols_label cols_align fmt_number tab_header gtsave
generate_assoc_metrics <- function(analysis_output.in) {
  
  formatted.out <- NULL
  
  # Check if the input has more than 0 elements
  if (length(analysis_output.in) > 0) {
    
    # If there is only one element in the input list
    if (length(analysis_output.in) == 1) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(` ` = "")
      )
      
      # If there are two elements in the input list
    } else if (length(analysis_output.in) == 2) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(`Image.Quality` = names(analysis_output.in)[1]),
        analysis_output.in[[2]] %>% mutate(`Image.Quality` = names(analysis_output.in)[2])
      )
    }
    
    # Extract reader type for use in filenames
    reader_type <- temp$reader_type[1]
    
    # Write the temporary results to a CSV file
    write.csv(temp, file = path(table_path, paste0("cor-", params$this_endpoint, "-", reader_type, ".csv")))
    
    # Generate formatted output table
    formatted.out <- temp %>%
      dplyr::filter(Statistic != "Obuchowski") %>%
      arrange(`p.value`, Comparator, Statistic, `Image.Quality`) %>%
      dplyr::rename(`p-value` = p.value) %>%
      dplyr::select(-c(Endpoint, reader_type)) %>%
      dplyr::select(Comparator, Statistic, `Image.Quality`, everything()) %>%
      group_by(Image.Quality) %>%
      gt() %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything()) %>%
      fmt_number(columns = c(Estimate, LCL, UCL, `p-value`), decimals = 2) %>%
      tab_header(
        title = paste("Report on Pearson and Spearman Correlations for", label_for_doc(params$this_endpoint)),
        subtitle = paste0("for ", reader_type, " reads")
      ) %>%
      tab_footnote(
        footnote = "Table sorted by p-value in ascending order.")
    
    # Save the formatted table as an image
    gt::gtsave(
      data = formatted.out,
      filename = path(table_path, paste0("cor-", params$this_endpoint, "-", reader_type, ".formatted.png"))
    )
  }
  
  return(formatted.out)
}

# Scatterplots
#' Generate Scatterplots
#'
#' This function takes a list of scatterplots and generates a grid of scatterplot arrangements for each item
#' in the list. It saves the generated scatterplot grids to files.
#'
#' @param scatterplots List of scatterplot objects to be arranged and saved.
#' @param reader_type The type of reader used, for use in filenames.
#' @return A list of arranged scatterplot grids, or NULL if the input list is empty.
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggsave
generate_scatterplots <- function(scatterplots, reader_type) {
  
  scatterplot.grid <- NULL
  
  # Check if the input has more than 0 elements
  if (length(scatterplots) > 0) {
    
    scatterplot.grid <- list()
    
    # Loop through each scatterplot to arrange and save
    for (i in 1:length((scatterplots))) {
      
      scatterplot.grid[[length(scatterplot.grid) + 1]] <- 
        grid.arrange(
          scatterplots[[i]]$VHCD, scatterplots[[i]]$IELCOUNT, scatterplots[[i]]$GISS, scatterplots[[i]]$Disease_Burden, 
          nrow = 2, 
          bottom = names(scatterplots)[i], 
          top = "Associations of MARCS Scores to Clinical Endpoints"
        )
      
      ggsave(
        scatterplot.grid[[length(scatterplot.grid)]], 
        filename = path(figure_path, paste0("scatterplots.grid-", params$this_endpoint, "-", reader_type, "-", names(scatterplots)[i], ".png")), 
        height = 8.5, width = 8.5, units = "in", dpi = 300
      )
    }
    
  } else {
    
    # Return NULL if input list is empty
    
  }
  return(scatterplot.grid)
}

# Generalized ROC
#' Generate ROC Table
#'
#' This function takes the analysis output and generates a formatted ROC table. It processes the data,
#' writes CSV files for the analysis, and generates a table with formatted results.
#'
#' @param analysis_output.in List of data frames containing the analysis output.
#' @return Formatted output object.
#' @import dplyr
#' @import gt
#' @importFrom dplyr mutate bind_rows arrange rename filter select everything group_by
#' @importFrom gt cols_merge cols_label cols_align fmt_number tab_header gtsave
generate_roc_table <- function(analysis_output.in) {
  
  formatted.out <- NULL
  
  # Check if the input has more than 0 elements
  if (length(analysis_output.in) > 0) {
    
    # If there is only one element in the input list
    if (length(analysis_output.in) == 1) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(` ` = "")
      )
      
      # If there are two elements in the input list
    } else if (length(analysis_output.in) == 2) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(` ` = names(analysis_output.in)[1]),
        analysis_output.in[[2]] %>% mutate(` ` = names(analysis_output.in)[2])
      )
    }
    
    # Extract reader type for use in filenames
    reader_type <- temp$reader_type[1]
    
    # Write the temporary results to a CSV file
    write.csv(temp, file = path(table_path, paste0("roc-", params$this_endpoint, "-", reader_type, ".csv")))
    
    # Append additional information to another CSV file
    append.csv(
      temp %>% dplyr::mutate(stratum = "<pooled>"),
      path = path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv"))
    )
    
    # Generate formatted output table
    formatted.out <- temp %>%
      dplyr::select(-c(`this_endpoint`, reader_type, variance)) %>%
      dplyr::select(Comparator, ` `, everything()) %>%
      dplyr::select(Comparator, ` `, everything(), -Statistic, ) %>%
      group_by(` `) %>%
      arrange(`p.value`, Comparator, ` `) %>%      
      gt() %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything()) %>%
      fmt_number(columns = c(Estimate, LCL, UCL, `p.value`), decimals = 2) %>%
      tab_header(
        title = paste0("Obuchowski Estimators and 95% Confidence Intervals for ", params$this_endpoint),
        subtitle = paste0("for ", reader_type, " reads")
      ) %>% 
      tab_footnote(
        footnote = "p-value is testing the null hypothesis that Obuchowski = 0.5 vs alternative that it is greater than 0.5") %>%
      tab_footnote(
        footnote = "Table sorted by p-value in ascending order.")
      
    
    
    # Save the formatted table as an image
    gt::gtsave(
      data = formatted.out,
      filename = path(table_path, paste0("roc-", params$this_endpoint, "-", reader_type, ".formatted.png"))
    )
  }
  
  return(formatted.out)
}

#' Generate ROC Figures
#'
#' This function takes a list of ROC figures and generates and saves the figures. 
#' It processes the list, saves each figure to the disk, and arranges pairs of figures for display.
#'
#' @param roc.figure List of ROC figure objects to be saved.
#' @param reader_type The type of reader used, for use in filenames.
#' @return The generated figures, or a grid arrangement if there are two figures.
#' @importFrom ggplot2 ggsave
#' @importFrom gridExtra grid.arrange
generate_roc_figure <- function(roc.figure, reader_type) {
  
  roc.fig.out <- list()
  
  # If there is only one ROC figure
  if (length(roc.figure) == 1) {
    
    roc.fig.out[[length(roc.fig.out) + 1]] <- roc.figure[[1]]
    
    ggsave(
      filename = path(figure_path, paste0("roc.figure-", params$this_endpoint, "-", reader_type, ".png")),
      plot = roc.figure[[1]]
    )
    
  } else {
    
    # Loop through each ROC figure
    for (i in 1:length(roc.figure)) {
      
      # Modify subtitle of the ROC figure
      roc.figure[[i]]$labels$subtitle <- paste0(roc.figure[[i]]$labels$subtitle, "-", names(roc.figure)[i])
      
      roc.fig.out[[length(roc.fig.out) + 1]] <- roc.figure[[i]]
      
      ggsave(
        filename = path(figure_path, paste0("roc.figure-", params$this_endpoint, "-", reader_type, "-", names(roc.figure)[i], ".png")),
        plot = roc.figure[[i]]
      )
    }
  }
  
  # Arrange and return the ROC figures
  if (length(roc.fig.out) == 1) {
    
    return(roc.fig.out[[1]])
    
  } else {
    
    if (length(roc.fig.out) == 2) {
      
      return(list(roc.fig.out[[1]], roc.fig.out[[2]]))
      
    }
  }
}


# Now the stratified counterparts
#' Generate Executive Correlation Report (Stratified)
#'
#' This function takes the analysis output and generates a formatted correlation report stratified by a given parameter. 
#' It processes the data, writes CSV files for the analysis, and generates a table with formatted results.
#'
#' @param analysis_output.in List of data frames containing the analysis output.
#' @param reader_type The type of reader used, for use in filenames.
#' @param stratifier The stratifier parameter for the analysis.
#' @return Formatted output object.
#' @import dplyr
#' @import gt
#' @importFrom dplyr mutate bind_rows arrange rename filter select everything group_by
#' @importFrom gt cols_merge cols_label cols_align fmt_number tab_header gtsave
generate_exec_cor_report_stratified <- function(analysis_output.in, reader_type, stratifier) {
  
  formatted.out <- NULL
  
  # Check if the input has more than 0 elements
  if (length(analysis_output.in) > 0) {
    
    # If there is only one element in the input list
    if (length(analysis_output.in) == 1) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(` ` = "")
      )
      
      # If there are two elements in the input list
    } else if (length(analysis_output.in) == 2) {
      temp <- bind_rows(
        analysis_output.in[[1]] %>% mutate(`Image.Quality` = names(analysis_output.in)[1]),
        analysis_output.in[[2]] %>% mutate(`Image.Quality` = names(analysis_output.in)[2])
      )
    }
    
    # Extract reader type for use in filenames
    reader_type <- temp$reader_type[1]
    
    # Write the temporary results to a CSV file
    write.csv(temp, file = path(table_path, paste0("cor.by", stratifier, "-", params$this_endpoint, "-", reader_type, ".csv")))
    
    # Append additional information to another CSV file
    append.csv(temp, path = path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv")))
    
    # Filter the temporary results for Spearman statistic
    formatted.out <- temp %>% dplyr::filter(Statistic == "Spearman")
    
    # Further filter if there are more than one analysis output
    if (length(analysis_output.in) > 1) {
      formatted.out <- formatted.out %>% dplyr::filter(`Image.Quality` == "NarrowIQ_Std")
    }
    
    # Get the image quality information
    image.quality <- formatted.out$`Image.Quality`[1]
    
    # Apply label formatting to the Comparator column
    formatted.out$Comparator <- label_for_doc.vec(formatted.out$Comparator)
    
    # Generate formatted output table
    formatted.out <- formatted.out %>%
      dplyr::filter(n != 0) %>%
      arrange(`p.value`, Comparator, stratum, Statistic) %>%
      dplyr::select(-Endpoint, -reader_type) %>%
      dplyr::select(Comparator, stratum, everything(), -Statistic) %>%
      group_by(Image.Quality) %>%
      gt() %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything()) %>%
      fmt_number(columns = c(Estimate, LCL, UCL, `p.value`), decimals = 2) %>%
      tab_header(
        title = paste0("Report on Spearman Correlations by ", stratifier, " for ", params$this_endpoint),
        subtitle = paste0("for ", reader_type, " reads ", ifelse(image.quality != "", paste("with the", image.quality, "level"), ""))
      ) %>%
      fmt_number(columns = c(Estimate, `p.value`), decimals = 2)
    
    # Save the formatted table as an image
    gt::gtsave(
      data = formatted.out,
      filename = path(table_path, paste0("cor.by", stratifier, "-", params$this_endpoint, "-", reader_type, ".formatted.png"))
    )
  }
  
  return(formatted.out)
}

#' Generate Stratified Scatterplots
#'
#' This function takes a list of scatterplots and generates grid arrangements for each group stratified by a given parameter.
#' It saves the generated scatterplot grids to files.
#'
#' @param scatterplots List of lists containing scatterplot objects to be arranged and saved.
#' @param reader_type The type of reader used, for use in filenames.
#' @param stratifier The stratifier parameter for the analysis.
#' @return A list of arranged scatterplot grids.
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggsave
generate_scatterplots_stratified <- function(scatterplots, reader_type, stratifier) {
  
  temp <- list()
  
  # Loop through each stratifier category in scatterplots
  for (i in 1:length(scatterplots)) {
    
    # Loop through each scatterplot within each stratifier category
    for (j in 1:length(scatterplots[[i]])) {
      
      # If there is only one stratifier category
      if (length(scatterplots) == 1) {
        
        temp[[length(temp) + 1]] <- 
          grid.arrange(
            scatterplots[[i]][[j]]$VHCD, scatterplots[[i]][[j]]$IELCOUNT, scatterplots[[i]][[j]]$GISS, scatterplots[[i]][[j]]$Disease_Burden, 
            nrow = 2, 
            top = paste0("Associations by ", names(scatterplots[[i]])[j])
          )
        
        ggsave(
          filename = path(figure_path, paste0("scatterplots.grid-", params$this_endpoint, "-", reader_type, ".by", stratifier, "-", names(scatterplots[[i]])[j], ".png")),
          plot = temp[[length(temp)]],
          height = 8.5, width = 11, units = "in", dpi = 300
        )
        
      } else {
        
        temp[[length(temp) + 1]] <- 
          grid.arrange(
            scatterplots[[i]][[j]]$VHCD, scatterplots[[i]][[j]]$IELCOUNT, scatterplots[[i]][[j]]$GISS, scatterplots[[i]][[j]]$Disease_Burden,
            nrow = 2,
            top = paste0("Associations at ", names(scatterplots)[i], " by ", names(scatterplots[[i]])[j])
          )
        
        ggsave(
          filename = path(figure_path, paste0("scatterplots.grid-", params$this_endpoint, "-", reader_type, ".by", stratifier, "-", names(scatterplots)[i], "-", names(scatterplots[[i]])[j], ".png")),
          plot = temp[[length(temp)]],
          height = 8.5, width = 11, units = "in", dpi = 300
        )
      }
    }
  }
}

#' Generate Stratified ROC Report
#'
#' This function takes the analysis output and generates a formatted ROC report stratified by a given parameter.
#' It processes the data, writes CSV files for the analysis, and generates a table with formatted results.
#'
#' @param analysis_output.in List of data frames containing the analysis output.
#' @param stratifier The stratifier parameter for the analysis.
#' @return Formatted output object.
#' @import dplyr
#' @import gt
#' @importFrom dplyr mutate bind_rows arrange filter select everything group_by
#' @importFrom gt cols_merge cols_label cols_align fmt_number tab_header gtsave
generate_roc_report_stratified <- function(analysis_output.in, stratifier) {
  
  formatted.out <- NULL
  
  # Check if the input has more than 0 elements
  if (length(analysis_output.in) > 0) {
    
    # If there is only one element in the input list
    if (length(analysis_output.in) == 1) {
      temp.df <- bind_rows(
        analysis_output.in[[1]] %>% mutate(` ` = "")
      )
      
      # If there are two elements in the input list
    } else if (length(analysis_output.in) == 2) {
      temp.df <- bind_rows(
        analysis_output.in[[1]] %>% mutate(`Image.Quality` = names(analysis_output.in)[1]),
        analysis_output.in[[2]] %>% mutate(`Image.Quality` = names(analysis_output.in)[2])
      )
    }
    
    # Extract reader type for use in filenames
    reader_type <- temp.df$reader_type[1]
    
    # Write the temporary results to a CSV file
    write.csv(temp.df, file = path(table_path, paste0("roc.by", stratifier, "-", params$this_endpoint, "-", reader_type, ".csv")))
    
    # Append additional information to another CSV file
    append.csv(temp, path = path(output_dir, paste("analysis-summary", study, Sys.Date(), ".csv")))
    
    # Apply label formatting to the Comparator column
    temp.df$Comparator <- label_for_doc.vec(temp.df$Comparator)
    
    # Generate formatted output table
    formatted.out <- temp.df %>%
      dplyr::filter(Statistic == "Obuchowski") %>%
      dplyr::filter(` ` == "NarrowIQ_Std") %>%
      arrange(`p.value`, Comparator, stratum, ` `) %>%
      dplyr::select(-`this_endpoint`, -reader_type) %>%
      dplyr::select(Comparator, stratum, `Image.Quality`, everything()) %>%
      group_by(Image.Quality, stratum) %>%
      gt() %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything()) %>%
      fmt_number(columns = c(Estimate, LCL, UCL, `p.value`), decimals = 2) %>%
      tab_header(
        title = paste0("Report on Generalized ROC Analysis by Visit (Interval) for ", params$this_endpoint),
        subtitle = paste0("for ", reader_type, " reads")
      ) %>%
      fmt_number(columns = c(Estimate, `p.value`), decimals = 2)
    
    # Save the formatted table as an image
    gt::gtsave(
      data = formatted.out,
      filename = path(table_path, paste0("roc.by", stratifier, "-", params$this_endpoint, "-", reader_type, ".formatted.png"))
    )
  }
  
  return(formatted.out)
}

#' Generate Stratified ROC Figure
#'
#' This function takes a list of ROC figures and generates and saves the figures stratified by a given parameter.
#' It processes the list and saves each figure to the disk.
#'
#' @param roc.figure List of ROC figure objects to be saved.
#' @param reader_type The type of reader used, for use in filenames.
#' @param stratifier The stratifier parameter for the analysis.
#' @return The saved figure, or NULL if the input list is empty.
#' @importFrom ggplot2 ggsave
generate_roc_figure_stratified <- function(roc.figure, reader_type, stratifier) {
  
  temp <- NULL
  
  # Check if the input list has more than 0 elements
  if (length(roc.figure) > 0) {
    
    # Save the first ROC figure to a file
    temp <- ggsave(
      plot = roc.figure[[1]], 
      filename = path(figure_path, paste0("roc.figure.by", stratifier, "-", params$this_endpoint, "-", reader_type, "-", names(roc.figure)[1], ".png"))
    )
    
    # Append additional information to the figure subtitle
    roc.figure[[1]]$labels$subtitle <- paste0(roc.figure[[1]]$labels$subtitle, "-", names(roc.figure)[1])
    
    # Return the first ROC figure
    return(roc.figure[[1]])
  }
  
  # Return NULL if there are no figures
  return(temp)
}

#' Generate Lower Triangle Correlation Matrix
#'
#' This function takes a lower triangle correlation matrix and generates a formatted table.
#' It processes the data, formats the table, and saves it as an image.
#'
#' @param lower.triangle Data frame containing the lower triangle correlation matrix.
#' @param stratifier The stratifier parameter for the analysis.
#' @return Formatted output object.
#' @import dplyr
#' @import gt
#' @importFrom gt sub_missing tab_header fmt_number cols_label gtsave
generate_lower_triangle <- function(lower.triangle, stratifier) {
  
  # Add a row column with the column names of the original matrix
  lower.triangle$row <- colnames(lower.triangle)
  
  # Move the row column to the first position
  lower.triangle <- lower.triangle %>% dplyr::select(row, everything())
  
  # Generate formatted output table
  for.return <- lower.triangle %>% 
    gt() %>% 
    sub_missing(missing_text = "---") %>%
    tab_header(title = "Spearman Correlation Matrix") %>%
    fmt_number(columns = everything(), decimals = 2) %>%
    cols_label(row = "")
  
  # Save the formatted table as an image
  gt::gtsave(
    data = for.return, 
    filename = path(table_path, paste0("Spearman.lower.triangle.by", stratifier, ".png"))
  )
  
  return(for.return)
}

#' Generate Variable Distributions Plot
#'
#' This function takes a global analysis set and generates a plot showing the distributions of variables.
#' It processes the data, formats it for plotting, and saves the resulting plot as an image.
#'
#' @param global_analysis_set Data frame containing the global analysis set.
#' @return Generated plot object.
#' @import dplyr
#' @import ggplot2
#' @importFrom dplyr mutate select ungroup across where
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_boxplot coord_flip theme scale_color_manual labs ggsave
generate_variable_distributions_plot <- function(global_analysis_set) {
  
  # Identify the columns of interest for the figure
  for.figure <- intersect(c(this_endpoint, unlist(comparators)), colnames(global_analysis_set))
  
  # Create a temporary data frame with selected variables and a new factorized MOlevel column
  temp.df <- global_analysis_set %>%
    dplyr::mutate(MOlevel = factor(MO, 0:3)) %>%
    dplyr::mutate(qMlevel=factor(qM, 0:3)) %>%
    dplyr::select(unlist(for.figure), MOlevel, qMlevel)
  
  # Save the temporary data frame before pivot for reference
  write.csv(temp.df, path(output_dir, paste0("7b just before pivot-", "ML", ".csv")), row.names = FALSE)
  
  # Normalize numeric columns and pivot the data for plotting
  temp.df <- temp.df %>%
    ungroup() %>%
    dplyr::mutate(VHCD = -VHCD) %>%
    dplyr::mutate(across(where(is.numeric), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))) %>%
    pivot_longer(cols = -intersect(colnames(temp.df), c("MOlevel","qMlevel", "STUDYID", "SITE", "SUBJID")), 
                 names_to = "Endpoint", values_to = "Value")
  
  # Save the pivoted data frame for reference
  write.csv(temp.df, path(output_dir, paste0("7c pivot_longer output-", "ML", ".csv")), row.names = FALSE)
  
  # Factorize the Endpoint column with appropriate labels
  temp.df$Endpoint <- factor(temp.df$Endpoint, levels = unique(temp.df$Endpoint), labels = label_for_doc.vec(unique(temp.df$Endpoint)))

  # Generate the boxplot for variable distributions
  variable.distributions1 <- temp.df %>%
    dplyr::filter(!is.na(MOlevel)) %>%
    ggplot(aes(x = Endpoint, y = Value)) +
    geom_boxplot(aes(colour = MOlevel)) + coord_flip() +
    theme(legend.position = "bottom") + theme(aspect.ratio = 1) + scale_color_manual(values = cbp2) + 
    labs(x = "Variable", 
         y = "Scaled Value", 
         title = "Distributions of Variables",
         color="Marsh Oberhuber Level",
         subtitle = paste0("Reader type: ML\n", global_analysis_set$label.name[1]))
  
  # Save the generated plot as an image
  ggsave(filename = path(figure_path, "variable.distributions.png"), height = 8.5, width = 11, units = "in", dpi = 300)
  
  variable.distributions2 <- temp.df %>% 
    dplyr::filter(!is.na(qMlevel)) %>%
    ggplot(aes(x = Endpoint, y = Value)) +
    geom_boxplot(aes(colour = qMlevel)) + coord_flip() +
    theme(legend.position = "bottom") + theme(aspect.ratio = 1) + scale_color_manual(values = cbp2) + 
    labs(x = "Variable", 
         y = "Scaled Value", 
         title = "Distributions of Variables",
         color="Quantitative Marsh Level",
         subtitle = paste0("Reader type: ML\n", global_analysis_set$label.name[1]))
  
  
  
  
  return(list(variable.distributions1, variable.distributions2))
}

#' Plot Summary Statistics
#'
#' This function generates summary statistics plots by Visit, ARM, and Endpoint.
#' It saves each plot as a PNG file and returns a list of ggplot objects.
#'
#' @param data.in Data frame or list. By default, it uses `sumstats.ML.byARM`. When provided with two data frames, it binds them together.
#' @return A list of ggplot objects with summary statistics plots.
#' @examples
#' # Example usage
#' plot_summary_stats(sumstats.ML.byARM)
#' @export
plot_summary_stats <- function(data.in = sumstats.ML.byARM) {
  
  # Check if data.in consists of two data frames and bind them together
  if (length(data.in) == 2) {
    temp <- bind_rows(
      data.in[[1]] %>% dplyr::mutate(Image.Quality = names(data.in)[1]),
      data.in[[2]] %>% dplyr::mutate(Image.Quality = names(data.in)[2])
    )
  }
  
  if(temp$STUDYID[1] == "TAK-062-2001") temp <- temp %>% dplyr::filter(ARM %in% c("Placebo", "TAK-062"))
  
  visit.pairing <- ifelse("VISIT" %in% colnames(temp),  "VISIT","pairing")
  
  # Combine this_endpoint and comparators to get the desired order
  endpoint_order <- c(this_endpoint, comparators)
  
  # Ensure the endpoint column is a factor with levels in the specified order
  temp$endpoint <- factor(temp$endpoint, levels = endpoint_order)
  temp <- temp %>% mutate(!!sym(visit.pairing) := factor(!!sym(visit.pairing) ))
  
  # Define dodge position for error bars and points
  dodge <- position_dodge(width = 0.2)
  
  # Get unique Image.Quality values from data
  unique_quality_levels <- unique(temp$Image.Quality)
  
  # Extract reader type from the first entry in the data
  reader_type <- temp$reader_type[1]
  
  # Create a named list to store ggplot objects
  plots <- list()
  if(visit.pairing == "pairing") levels(temp$pairing) <- c("Week -4 vs. 12", "Week 12 vs. 24", "Week -4 vs. 24")
  # Loop through each unique Image.Quality value and create a plot
  for (quality in unique_quality_levels) {
    plot <- temp %>%
      filter(Image.Quality == quality,
             !is.na(mean)) %>%
      ggplot(aes(x = as.numeric(factor(!!sym(visit.pairing))), y = mean, color = ARM, group = ARM)) +
      geom_point(position = dodge) +
      geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, position = dodge) +
      facet_wrap(~ endpoint, scales = "free_y") +
      labs(
        x = "Visit",
        y = "Mean with 95% CI",
        color="Arm",
        title = paste("Summary Statistics by Visit, ARM, and Endpoint"),
        subtitle = paste("Reader type:", reader_type, "Image Quality:", quality) )+
      scale_x_continuous(breaks = 1:3, 
                         labels = levels(factor(temp[[visit.pairing]]))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=rel(.75)),
            legend.position = "bottom")
    if(visit.pairing == "VISIT") plot <- plot + geom_line(position = dodge, linetype="dashed")
    if(visit.pairing == "pairing") plot <- plot + geom_hline(yintercept = 0, linetype = "dashed")
    
    # Save the plot in the list
    plots[[quality]] <- plot
    
    # Save each plot as a PNG file
    ggsave(
      filename = file.path(figure_path, paste0("summary_stats-", this_endpoint, "-", reader_type, "-", quality, ".png")),
      plot = plot,
      width = 10, height = 6
    )
  }
  
  return(plots)
}

#' Plot Density Statistics
#'
#' This function generates density statistics plots by Visit, ARM, and Endpoint.
#' It saves each plot as a PNG file and returns a list of ggplot objects.
#'
#' @param data.in Data frame or list. By default, it uses `sumstats.ML.byARM`. When provided with two data frames, it binds them together.
#' @return A list of ggplot objects with density statistics plots.
#' @examples
#' # Example usage
#' plot_density_stats(sumstats.ML.byARM)
#' @export
plot_density_stats <- function(data.in = analysis.data.in.ML) {
  
  visit.pairing <- ifelse("VISIT" %in% colnames(data.in),  "VISIT","pairing")
  # Check if data.in consists of two data frames and bind them together
  if (length(data.in) == 2) {
    temp <- bind_rows(
      data.in[[1]] %>% dplyr::mutate(Image.Quality = names(data.in)[1]),
      data.in[[2]] %>% dplyr::mutate(Image.Quality = names(data.in)[2])
    )
  }

  # Combine this_endpoint and comparators to get the desired order
  endpoint_order <- c(this_endpoint, comparators)

  if(temp$STUDYID[1] == "TAK-062-2001") temp <- temp %>% dplyr::filter(ARM %in% c("Placebo", "TAK-062"))

  
  # Ensure the endpoint column is a factor with levels in the specified order
   # Get unique Image.Quality values from data
  unique_quality_levels <- unique(temp$Image.Quality)
  
  # Extract reader type from the first entry in the data
  reader_type <- temp$READER[1]
  
  # Create a named list to store ggplot objects
  plots <- list()
  
  temp <- temp %>% pivot_longer(cols = c(all_of(this_endpoint), "VHCD", "IELCOUNT", "GISS", "Disease_Burden"), names_to = "endpoint") %>%
    group_by(endpoint, !!sym(visit.pairing)) %>%
    mutate(value = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) %>%
    ungroup()
  if(visit.pairing == "pairing") levels(temp$pairing) <- c("Week -4 vs. 12", "Week 12 vs. 24", "Week -4 vs. 24")
  temp$endpoint <- factor(temp$endpoint, levels = endpoint_order)
  temp <- temp %>% mutate(!!sym(visit.pairing) := factor(!!sym(visit.pairing) ))
  # Loop through each unique Image.Quality value and create a plot
  for (quality in unique_quality_levels) {
    plot <- temp %>%
      filter(Image.Quality == quality) %>%
      ggplot(aes(x = value, fill = ARM)) +
      geom_density(alpha = 0.5) +
      facet_grid(as.formula(paste(visit.pairing, "~ endpoint")), scales = "free")+
      labs(
        x = "Change in value",
        y = "Density",
        fill="Arm",
        title = paste("Density Statistics by Arm, Visit, and Standardized Endpoints"),
        subtitle = paste("Reader type:", reader_type, "Image Quality:", quality)
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme(legend.position = "bottom")
    if(visit.pairing == "pairing") plot <- plot + geom_vline(xintercept = 0, linetype = "dashed")
    
    # Save the plot in the list
    plots[[quality]] <- plot
    
    # Save each plot as a PNG file
    ggsave(
      filename = file.path(figure_path, paste0("density_stats-", this_endpoint, "-", reader_type, "-", quality, ".png")),
      plot = plot,
      width = 10, height = 6
    )
  }
  
  return(plots)
}

# sumstats.report table

#' Generate Summary Statistics Report
#'
#' This function generates a summary statistics report for the given data.
#'
#' @param data.in A list containing input data for analysis, default is `cross.sectional.tests.ML`
#'
#' @return A formatted table with summary statistics
#'
#' @examples
#' sumstats.report(data.in = cross.sectional.tests.ML)
sumstats.report <- function(data.in = sumstats.ML.byARM) {
  
  formatted.out.list <- list()
  
  # Check if the input has more than 0 elements
  if (length(data.in) > 0) {
    
    # If there is only one element in the input list
    if (length(data.in) == 1) {
      temp <- bind_rows(
        data.in[[1]] %>% mutate(`Image.Quality` = "")
      )
      # If there are two elements in the input list
    } else if (length(data.in) == 2) {
      temp <- bind_rows(
        data.in[[1]] %>% mutate(`Image.Quality` = names(data.in)[1]),
        data.in[[2]] %>% mutate(`Image.Quality` = names(data.in)[2])
      )
    }
    if(temp$STUDYID[1] == "TAK-062-2001") temp <- temp %>% dplyr::filter(ARM %in% c("Placebo", "TAK-062"))
    
    visit.pairing <- ifelse("VISIT" %in% colnames(temp),  "VISIT","pairing")
    
    # Convert VISIT to factor
    temp <- temp %>% dplyr::mutate(!!sym(visit.pairing) := factor(!!sym(visit.pairing)))
    
    # Extract reader type for use in filenames
    reader_type <- temp$reader_type[1]
    
    # Write the temporary results to a CSV file
    write.csv(temp, file = path(table_path, paste0("cor-", params$this_endpoint, "-", reader_type, ".csv")))
    
    # Apply label formatting to the Comparator column
    temp$endpoint <- factor(temp$endpoint, levels = c(this_endpoint, "VHCD", "IELCOUNT", "GISS", "VCIEL", "Agg_Histology", "Disease_Burden"))
    
    # Generate formatted output tables for each level of visit.pairing
    visit_levels <- levels(temp[[visit.pairing]])
    
    for (level in visit_levels) {
      temp_level <- temp %>% dplyr::filter(!!sym(visit.pairing) == level)
      
      formatted.out <- temp_level %>%
        dplyr::select(endpoint, everything()) %>%
        dplyr::filter(Image.Quality=="NarrowIQ_Std") %>%
        group_by(Image.Quality, !!sym(visit.pairing)) %>%
        dplyr::arrange(endpoint, !!sym(visit.pairing)) %>%
        dplyr::filter(!is.na(sd)) %>%
        dplyr::select(-reader_type) %>%
        dplyr::rename(Endpoint = endpoint, Arm = ARM) %>%
        gt() %>%
        cols_merge(columns = c(mean, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
        cols_label(mean = "Mean [95% CI]") %>%
        cols_align(align = "center", columns = everything()) %>%
        fmt_number(columns = c(mean, LCL, UCL, sd, median, min, max), decimals = 2) %>%
        tab_header(
          title = paste("Report on Summary Statistics for", label_for_doc(params$this_endpoint)),
          subtitle = paste0("for ", reader_type, " reads - ", level)
        )
      
      # Save the formatted table as an image
      gt::gtsave(
        data = formatted.out,
        filename = path(table_path, paste0("sumstats-report-", params$this_endpoint, "-", reader_type, "-", level, ".formatted.png"))
      )
      
      # Append the formatted table to the list
      formatted.out.list[[length(formatted.out.list)+1]] <- formatted.out
      names(formatted.out.list)[length(formatted.out.list)] <- level
    }
  }
  
  return(formatted.out.list)
}

#' Generate Cross-Sectional Comparison Report
#'
#' This function generates a cross-sectional comparison report for the given data.
#'
#' @param data.in A list containing input data for analysis, default is `cross.sectional.tests.ML`
#'
#' @return A formatted table with summary statistics
#'
#' @examples
#' cross.section.comparison.report(data.in = cross.sectional.tests.ML)
#' Generate Cross-Sectional Comparison Plots
#'
#' This function generates cross-sectional comparison plots for the given data and saves them as PNG files.
#'
#' @param data.in A list containing input data for analysis, default is `cross.sectional.tests.ML`
#'
#' @return A list of ggplot objects for each unique level of `Image.Quality`
#'
#' @examples
#' ggplots.list <- cross.section.comparison.plots(data.in = cross.sectional.tests.ML)
cross.section.comparison.plots <- function(data.in = cross.sectional.tests.ML) {

  plots <- list()
  
  # Check if the input has more than 0 elements
  if (length(data.in) > 0) {
    
    # If there is only one element in the input list
    if (length(data.in) == 1) {
      
      temp <- bind_rows(
        data.in[[1]] %>% mutate(`Image.Quality` = "")
      )
      
      # If there are two elements in the input list
    } else if (length(data.in) == 2) {
      
      temp <- bind_rows(
        data.in[[1]] %>% mutate(`Image.Quality` = names(data.in)[1]),
        data.in[[2]] %>% mutate(`Image.Quality` = names(data.in)[2])
      )
    }
    visit.pairing <- ifelse("VISIT" %in% colnames(temp),  "VISIT","pairing")
    # Convert VISIT to factor
    temp <- temp %>% dplyr::mutate(!!sym(visit.pairing) := factor(!!sym(visit.pairing)))
    
    # Extract reader type for use in filenames
    reader_type <- temp$reader_type[1]
    
    # Write the temporary results to a CSV file
    write.csv(temp, file = path(table_path, paste0("cor-", params$this_endpoint, "-", reader_type, ".csv")))
    
    # Apply label formatting to the Comparator column
    temp$endpoint <- factor(temp$endpoint, levels = c(this_endpoint, "VHCD", "IELCOUNT", "GISS", "VCIEL", "Agg_Histology", "Disease_Burden"))
    
    if(visit.pairing == "pairing") levels(temp$pairing) <- c("Week -4 vs. 12", "Week -4 vs. 24", "Week 12 vs. 24")
  # Loop over each unique level of Image.Quality
  for (quality in unique(temp$`Image.Quality`)) {
    # Filter data for the current Image.Quality
    quality_data <- temp %>% filter(`Image.Quality` == quality)
    
    # Generate the ggplot
    p <- ggplot(quality_data, aes(x = as.numeric(as.factor(!!sym(visit.pairing))), y = Difference)) +
      geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.25) +
      geom_point(size = 3) +
      facet_wrap(~endpoint, scales = "free_y") +
      labs(title = paste("Cross-Sectional Difference in Means for", quality),
           x = "Visit",
           y = "Difference [95% CI]",
           caption=paste("Differences are based on 'PBO - TAK-062'\n",
                         "Error bars represent 95% confidence intervals.\n",
                         "Efficacy suggestive when differences for", this_endpoint,", IELCOUNT, GISS, Disease Burden > 0 and VHCD, VCIEL < 0."
           )
           
           ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=rel(.75)))+
      scale_x_continuous(breaks = 1:3, 
                         labels = levels(factor(temp[[visit.pairing]])))
    if(visit.pairing=="VISIT") p <- p + geom_line()
    if(visit.pairing=="pairing") p <- p + labs(title=paste("Longitudinal Differences in Means for", quality))
    
    
    # Create the filename
    filename <- file.path(
      table_path, 
      paste0("cross.section.comparison-", this_endpoint, "-", reader_type, "-", quality, ".png")
    )
    
    # Save the plot as a PNG file
    ggsave(filename = filename, plot = p, width = 10, height = 6, dpi = 300)
    
    # Store the plot in the list
    plots[[quality]] <- p
  }
  }
  
  return(plots)
}



#' Generate Variance Components Analysis Report
#'
#' @param VCA.fit A list of two elements where each element contains the data and results of VCA analysis with similar structure.
#' @return A gt table summarizing the Variance Components Analysis for the specified endpoint and saves it as a PNG file.
#' @export
#'
#' @examples
#' VCA.report(list(WideIQ_Std = fit_data_1, NarrowIQ_Std = fit_data_2))
VCA.report <- function(VCA.fit) {
  
  # Extract endpoint and reader type from the first element of the list
  endpoint <- VCA.fit[[1]]$data$endpoint[1]
  reader_type <- VCA.fit[[1]]$data$reader[1]
  
  # Initialize VCA_combined
  VCA_combined <- NULL
  
  # Append variance component analysis tables from available elements
  for (i in seq_along(VCA.fit)) {
    VCA_combined <- bind_rows(
      VCA_combined,
      VCA.fit[[i]]$REML.fit$aov.tab %>% 
        rownames_to_column('Source') %>% 
        dplyr::mutate(IQ = names(VCA.fit)[i])
    )
  }
  
  # Process and format the combined data
  VCA_table <- VCA_combined %>%
    dplyr::select(Source, everything())
  
  # Apply `group_by(IQ)` only if there are multiple items in the list
  if (length(VCA.fit) > 1) {
    VCA_table <- VCA_table %>% group_by(IQ)
  }
  
  VCA_table <- VCA_table %>%
    left_join(
      bind_rows(
        lapply(seq_along(VCA.fit), function(i) {
          VCA::getCI(VCA.fit[[i]]$REML.fit, type = "vc", tail = "two-sided") %>%
            mutate(`Variance Component` = VCA.fit[[i]]$REML.fit$aov.tab$VC) %>%
            dplyr::select(Name, `Variance Component`, everything()) %>%
            dplyr::rename(Source = Name) %>% 
            dplyr::mutate(IQ = names(VCA.fit)[i])
        })
      )
    ) %>%
    dplyr::select(Source, DF, SD, `Variance Component`, LCL, UCL) %>%
    gt() %>%
    tab_header(
      title = paste("Variance Components Analysis for", endpoint)
    ) %>%
    cols_merge(
      columns = c(`Variance Component`, LCL, UCL), 
      pattern = "{1} [{2}, {3}]"
    ) %>%
    cols_label(`Variance Component` = "Variance Component [95% CI]") %>%
    fmt_number(columns = c(DF, SD, `Variance Component`, UCL, LCL), decimals = 2) %>%
    fmt_number(columns = c(DF), decimals = 2)
  
  # Generate file name
  filename <- paste0(endpoint, "_", reader_type, "_vce-report.png")
  file_path <- file.path(table_path, filename)
  
  # Save the table as a PNG file
  gtsave(data = VCA_table, filename = file_path)
  
  return(VCA_table)
}

#' Generate Metrics from Variance Component Analysis Report
#'
#' @param VCA.fit A list of two elements where each element contains the data and results of VCA analysis with similar structure.
#' @return A gt table summarizing the metrics from Variance Component Analysis for the specified endpoint and saves it as a PNG file.
#' @export
#'
#' @examples
#' VCA.report2(list(WideIQ_Std = fit_data_1, NarrowIQ_Std = fit_data_2))
VCA.report2 <- function(VCA.fit) {
  
  if(length(VCA.fit) == 2) {
  # Combine VCA total analytic variations for both IQ levels
  VCA_combined <- bind_rows(
    VCA.fit[[1]]$VCA.total.analytic.variation %>% mutate(IQ = names(VCA.fit)[1]),
    VCA.fit[[2]]$VCA.total.analytic.variation %>% mutate(IQ = names(VCA.fit)[2]),
    data.frame(Metric = "Minimum Detectable Effect", Estimate = VCA.fit[[1]]$MDD, LCL = NA, UCL = NA, length = NA, IQ = names(VCA.fit)[1]),
    data.frame(Metric = "Minimum Detectable Effect", Estimate = VCA.fit[[2]]$MDD, LCL = NA, UCL = NA, length = NA, IQ = names(VCA.fit)[2])
  )} else {if (length(VCA.fit)==1)
    VCA_combined <- data.frame(Metric = "Minimum Detectable Effect", Estimate = VCA.fit[[1]]$MDD, LCL = NA, UCL = NA, length = NA, IQ=NA)
    }
  
  # Process and format the combined data
  VCA_table <- VCA_combined %>%
    dplyr::select(-length) %>%
    group_by(IQ) %>%
    gt() %>%
    tab_header(
      title = paste("Metrics from Variance Component Analysis", VCA.fit[[1]]$data$endpoint[1])
    ) %>%
    cols_merge(columns = c(`Estimate`, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
    cols_label(`Estimate` = "Estimate [95% CI]") %>%
    fmt_number(columns = c(`Estimate`, UCL, LCL), decimals = 2) %>%
    tab_footnote("Confidence intervals are not available for MDD.")
  
  # Generate file name
  endpoint <- VCA.fit[[1]]$data$endpoint[1]
  reader_type <- VCA.fit[[1]]$data$reader_type[1]
  filename <- paste0(endpoint, "-", reader_type, "-var-comp-metrics-report.png")
  file_path <- file.path(table_path, filename)
  
  # Save the table as a PNG file
  gtsave(data = VCA_table, filename = file_path)
  
  return(VCA_table)
}

#' Generate Histograms of Subject Means
#'
#' @param VCA.fit A list of two elements where each element contains the data and results of VCA analysis with similar structure.
#' @return A list of ggplot objects for each IQ level, where each plot is a histogram of subject means, and saves them as PNG files.
#' @export
#'
#' @examples
#' VCA.histogram.subject.means(list(WideIQ_Std = fit_data_1, NarrowIQ_Std = fit_data_2))
VCA.histogram.subject.means <- function(VCA.fit) {
  
  # Initialize list to hold plots
  plots <- list()
  
  # Check if VCA.fit is a list of length 1
  if(length(VCA.fit) == 1 && is.null(names(VCA.fit)[1])) {
    names(VCA.fit) <- "Unnamed"
  }
  
  # Loop through each element in VCA.fit
  for (i in seq_along(VCA.fit)) {
    fit <- VCA.fit[[i]]
    
    # Handle case when names(VCA.fit)[i] is NULL
    iq_name <- ifelse(is.null(names(VCA.fit)[i]), paste0("Fit_", i), names(VCA.fit)[i])
    
    data_in <- fit$data
    
    # Extract endpoint and reader type
    this_endpoint <- data_in$endpoint[1]
    reader_type <- data_in$reader_type[1]
    
    # Generate the histogram plot
    plot <- data_in %>%
      group_by(Subject) %>%
      dplyr::summarize(
        mean.y = mean(!!sym(this_endpoint)),
        sd.y = sd(!!sym(this_endpoint)),
        .groups = 'drop'
      ) %>%
      ggplot(aes(x = mean.y)) +
      geom_histogram(alpha = 0.6, bins = 30) +
      geom_rug() +
      labs(
        x = "Subject Means",
        title = paste0("Histogram of Subject ", this_endpoint, " Averages"),
        subtitle = paste0("Image Quality: ", iq_name)
      )
    
    # Generate file name
    filename <- paste0(this_endpoint, "-", reader_type, "-", iq_name, "-histogram.png")
    file_path <- file.path(figure_path, filename)
    
    # Save the plot as a PNG file
    ggsave(plot, filename = file_path, width = 8, height = 6)
    
    # Add plot to the list
    plots[[iq_name]] <- plot
  }
  
  return(plots)
}
#' Generate Histograms of Subject Standard Deviations
#'
#' @param VCA.fit A list of two elements where each element contains the data and results of VCA analysis with similar structure.
#' @return A list of ggplot objects for each IQ level, where each plot is a histogram of subject standard deviations, and saves them as PNG files.
#' @export
#'
#' @examples
#' VCA.histogram.subject.sds(list(WideIQ_Std = fit_data_1, NarrowIQ_Std = fit_data_2))
VCA.histogram.subject.sds <- function(VCA.fit) {
  
  # Initialize list to hold plots
  plots <- list()
  
  # Check if VCA.fit is a list of length 1
  if(length(VCA.fit) == 1 && is.null(names(VCA.fit)[1])) {
    names(VCA.fit) <- "Unnamed"
  }
  
  # Loop through each element in VCA.fit
  for (i in seq_along(VCA.fit)) {
    fit <- VCA.fit[[i]]
    
    # Handle case when names(VCA.fit)[i] is NULL
    iq_name <- ifelse(is.null(names(VCA.fit)[i]), paste0("Fit_", i), names(VCA.fit)[i])
    
    data_in <- fit$data
    
    # Extract endpoint and reader type
    this_endpoint <- data_in$endpoint[1]
    reader_type <- data_in$reader_type[1]
    
    # Generate the histogram plot
    plot <- data_in %>%
      group_by(Subject) %>%
      dplyr::summarize(
        mean.y = mean(!!sym(this_endpoint)),
        sd.y = sd(!!sym(this_endpoint)),
        .groups = 'drop'
      ) %>%
      ggplot(aes(x = sd.y)) +
      geom_histogram(alpha = 0.6, bins = 30) +
      geom_rug() +
      labs(
        x = "Subject Standard Deviations",
        title = paste0("Histogram of Subject ", this_endpoint, " Standard Deviations"),
        subtitle = paste0("Image Quality: ", iq_name)
      )
    
    # Generate file name
    filename <- paste0(this_endpoint, "-", reader_type, "-", iq_name, "-sds-histogram.png")
    file_path <- file.path(figure_path, filename)
    
    # Save the plot as a PNG file
    ggsave(plot, filename = file_path, width = 8, height = 6)
    
    # Add plot to the list
    plots[[iq_name]] <- plot
  }
  
  return(plots)
}
#' Generate Variability Charts
#'
#' @param VCA.fit A list of two elements where each element contains the data and results of VCA analysis with similar structure.
#' @param tofile Logical, indicates whether to save the plots to files.
#' @param toscreen Integer, indicates which plot to display on the screen (1 or 2).
#' @return NULL. The function generates and saves variability charts as PNG files or displays one on the screen.
#' @export
#'
#' @examples
#' VCA.plot(list(WideIQ_Std = fit_data_1, NarrowIQ_Std = fit_data_2), tofile=TRUE, toscreen=2)

VCA.plot <- function(VCA.fit, tofile = TRUE, toscreen = 2) {
  
  # Check if VCA.fit is a list of length 1
  if (length(VCA.fit) == 1 && is.null(names(VCA.fit)[1])) {
    names(VCA.fit) <- "Unnamed"
  }
  
  # Loop through each element in VCA.fit
  for (i in seq_along(VCA.fit)) {
    fit <- VCA.fit[[i]]
    
    # Handle case when names(VCA.fit)[i] is NULL
    iq_name <- ifelse(is.null(names(VCA.fit)[i]), paste0("Fit_", i), names(VCA.fit)[i])
    
    data.in <- fit$data
    
    # Extract endpoint and reader type
    this_endpoint <- data.in$endpoint[1]
    reader_type <- data.in$reader_type[1]
    
    # Calculate mean, standard deviation, and length of the endpoint
    mean.y <- mean(data.in[[this_endpoint]])
    sd.y <- sd(data.in[[this_endpoint]])
    n.y <- length(data.in[[this_endpoint]])
    
    # Create the model formula
    f0 <- as.formula(paste(this_endpoint, "~ (Subject + Traversal + Traversal * Subject) / Retest"))
    
    # Create the variability chart
    if (tofile == TRUE) {
      filename <- paste0(this_endpoint, "-", reader_type, "-", iq_name, "-variability-chart.png")
      file_path <- file.path(figure_path, filename)
      
      # Save the plot as a PNG file
      png(filename = file_path, width = 800, height = 600)
      varPlot(f0, data.in,
              Title = list(main = paste0("Variability Chart for ", this_endpoint, " based on ", iq_name)),
              YLabel = list(text = this_endpoint))
      abline(h = mean.y)
      abline(h = mean.y + qt(df = n.y - 1, p = .975) * sd.y * (1 + 1 / n.y))
      abline(h = mean.y - qt(df = n.y - 1, p = .975) * sd.y * (1 + 1 / n.y))
      dev.off()
      
    } else if (i == toscreen) {
      # Display the plot on the screen
      varPlot(f0, data.in,
              Title = list(main = paste0("Variability Chart for ", this_endpoint)),
              YLabel = list(text = this_endpoint))
      abline(h = mean.y)
      abline(h = mean.y + qt(df = n.y - 1, p = .975) * sd.y * (1 + 1 / n.y))
      abline(h = mean.y - qt(df = n.y - 1, p = .975) * sd.y * (1 + 1 / n.y))
    }
  }
}
#' Generate Scatterplots of Subject Mean vs. Standard Deviation
#'
#' @param VCA.fit A list of two elements where each element contains the data and results of VCA analysis with similar structure.
#' @return A list of ggplot objects for each IQ level, where each plot is a scatterplot of subject mean vs. standard deviation, and saves them as PNG files.
#' @export
#'
#' @examples
#' VCA.scatterplot.subject.mean.vs.sd(list(WideIQ_Std = fit_data_1, NarrowIQ_Std = fit_data_2))
VCA.scatterplot.subject.mean.vs.sd <- function(VCA.fit) {
  
  plots <- list()
  
  # Loop through each element in VCA.fit
  for (i in seq_along(VCA.fit)) {
    
    fit <- VCA.fit[[i]]
    iq_name <- names(VCA.fit)[i]
    data_in <- fit$data
    
    # Extract endpoint and reader type
    this_endpoint <- data_in$endpoint[1]
    reader_type <- data_in$reader_type[1]
    
    # Generate the scatterplot
    plot <- data_in %>%
      group_by(Subject) %>%
      dplyr::summarize(
        mean.y = mean(!!sym(this_endpoint)),
        sd.y = sd(!!sym(this_endpoint)),
        .groups = 'drop'
      ) %>%
      ggplot(aes(x = mean.y, y = sd.y)) +
      geom_point() +
      labs(
        x = paste0("Mean of ", this_endpoint),
        y = paste0("Standard Deviation of ", this_endpoint),
        title = paste0("Scatterplot of ", this_endpoint, " Mean vs. ", this_endpoint, " Standard Deviation Scores"),
        subtitle = paste0("Image Quality: ", iq_name)
      ) +
      scale_x_continuous(breaks = seq(0, 3, .5), limits = c(0, 3))
    
    # Generate file name
    filename <- paste0(this_endpoint, "-", reader_type, "-", iq_name, "-scatterplot-mean-vs-sd.png")
    file_path <- file.path(figure_path, filename)
    
    # Save the plot as a PNG file
    ggsave(filename = file_path, plot = plot, width = 8, height = 6)
    
    # Add plot to the list
    plots[[iq_name]] <- plot
  }
  
  return(plots)
}
VCA.scatterplot.subject.mean.vs.sd <- function(VCA.fit) {
  
  # Initialize list to hold plots
  plots <- list()
  
  # Check if VCA.fit is a list of length 1
  if (length(VCA.fit) == 1 && is.null(names(VCA.fit)[1])) {
    names(VCA.fit) <- "Unnamed"
  }
  
  # Loop through each element in VCA.fit
  for (i in seq_along(VCA.fit)) {
    fit <- VCA.fit[[i]]
    
    # Handle case when names(VCA.fit)[i] is NULL
    iq_name <- ifelse(is.null(names(VCA.fit)[i]), paste0("Fit_", i), names(VCA.fit)[i])
    
    data_in <- fit$data
    
    # Extract endpoint and reader type
    this_endpoint <- data_in$endpoint[1]
    reader_type <- data_in$reader_type[1]
    
    # Generate the scatterplot
    plot <- data_in %>%
      group_by(Subject) %>%
      dplyr::summarize(
        mean.y = mean(!!sym(this_endpoint)),
        sd.y = sd(!!sym(this_endpoint)),
        .groups = 'drop'
      ) %>%
      ggplot(aes(x = mean.y, y = sd.y)) +
      geom_point() +
      labs(
        x = paste0("Mean of ", this_endpoint),
        y = paste0("Standard Deviation of ", this_endpoint),
        title = paste0("Scatterplot of ", this_endpoint, " Mean vs. ", this_endpoint, " Standard Deviation Scores"),
        subtitle = paste0("Image Quality: ", iq_name)
      ) +
      scale_x_continuous(breaks = seq(0, 3, .5), limits = c(0, 3))
    
    # Generate file name
    filename <- paste0(this_endpoint, "-", reader_type, "-", iq_name, "-scatterplot-mean-vs-sd.png")
    file_path <- file.path(figure_path, filename)
    
    # Save the plot as a PNG file
    ggsave(filename = file_path, plot = plot, width = 8, height = 6)
    
    # Add plot to the list
    plots[[iq_name]] <- plot
  }
  
  return(plots)
}
#' Generate Bland-Altman Plots
#'
#' @param VCA.results A list of two elements where each element contains the data and results of VCA analysis with similar structure.
#' @return A list of ggplot objects for each IQ level, where each plot is a Bland-Altman plot, and saves them as PNG files.
#' @export
#'
#' @examples
#' Bland.Altman(list(WideIQ_Std = fit_data_1, NarrowIQ_Std = fit_data_2))
Bland.Altman <- function(VCA.results) {
  
  plots <- list()
  
  # Loop through each element in VCA.results
  for (i in seq_along(VCA.results)) {
    
    fit <- VCA.results[[i]]
    iq_name <- names(VCA.results)[i]
    data_in <- fit$data
    
    # Extract endpoint and reader type
    this_endpoint <- data_in$endpoint[1]
    reader_type <- data_in$reader_type[1]
    
    # Extract Repeatability Coefficient (RC)
    RC <- fit$VCA.total.analytic.variation$Estimate[3]
    
    # Calculate mean and difference for each pair
    temp <- data_in %>%
      group_by(Subject, Traversal) %>%
      dplyr::summarize(
        mean.y = mean(!!sym(this_endpoint)),
        diff.y = (!!sym(this_endpoint))[1] - (!!sym(this_endpoint))[2],
        .groups = 'drop'
      )
    
    max_diff <- max(abs(temp$diff.y))
    for_limits <- round((max_diff) * 4) / 4
    
    # Generate the Bland-Altman plot
    plot <- temp %>%
      ggplot(aes(x = mean.y, y = diff.y)) +
      geom_point() +
      geom_hline(yintercept = c(RC, -RC), linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        x = "Mean of re-tests",
        y = "Difference between re-tests",
        title = paste0("Bland-Altman Plot for ", this_endpoint),
        subtitle = paste0("Image Quality: ", iq_name),
        caption = "Dashed lines at [-RC, RC]; RC = Repeatability Coefficient"
      ) +
      scale_x_continuous(breaks = seq(0, 3, .5), limits = c(0, 3)) +
      scale_y_continuous(breaks = pretty(seq(-for_limits, for_limits, .25)), limits = c(-for_limits, for_limits))
    
    # Generate file name
    filename <- paste0(this_endpoint, "-", reader_type, "-", iq_name, "-bland-altman.png")
    file_path <- file.path(figure_path, filename)
    
    # Save the plot as a PNG file
    ggsave(filename = file_path, plot = plot, width = 8, height = 6)
    
    # Add plot to the list
    plots[[iq_name]] <- plot
  }
  
  return(plots)
}
Bland.Altman <- function(VCA.results=results.ML) {
  
  # Initialize list to hold plots
  plots <- list()
  
  # Check if VCA.results is a list of length 1
  if (length(VCA.results) == 1 && is.null(names(VCA.results)[1])) {
    names(VCA.results) <- "Unnamed"
  }
  
  # Loop through each element in VCA.results
  for (i in seq_along(VCA.results)) {
    fit <- VCA.results[[i]]
    
    # Handle case when names(VCA.results)[i] is NULL
    iq_name <- ifelse(is.null(names(VCA.results)[i]), paste0("Fit_", i), names(VCA.results)[i])
    
    data_in <- fit$data
    
    # Extract endpoint and reader type
    this_endpoint <- data_in$endpoint[1]
    reader_type <- data_in$reader_type[1]
    
    # Extract Repeatability Coefficient (RC)
    RC <- fit$VCA.total.analytic.variation$Estimate[3]
    
    # Calculate mean and difference for each pair
    temp <- data_in %>%
      group_by(Subject, Traversal) %>%
      dplyr::summarize(
        mean.y = mean(!!sym(this_endpoint)),
        diff.y = (!!sym(this_endpoint))[1] - (!!sym(this_endpoint))[2],
        .groups = 'drop'
      )
    
    max_diff <- max(abs(temp$diff.y))
    for_limits <- max_diff 
    
    # Generate the Bland-Altman plot
    plot <- temp %>%
      ggplot(aes(x = mean.y, y = diff.y)) +
      geom_point() +
      geom_hline(yintercept = c(RC, -RC), linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        x = "Mean of re-tests",
        y = "Difference between re-tests",
        title = paste0("Bland-Altman Plot for ", this_endpoint),
        subtitle = paste0("Image Quality: ", iq_name),
        caption = "Dashed lines at [-RC, RC]; RC = Repeatability Coefficient"
      ) +
      scale_x_continuous(breaks = seq(0, 3, .5), limits = c(0, 3)) +
      scale_y_continuous(breaks = pretty(seq(-for_limits, for_limits, .25)), limits = c(-for_limits, for_limits))
    
    # Generate file name
    filename <- paste0(this_endpoint, "-", reader_type, "-", iq_name, "-bland-altman.png")
    file_path <- file.path(figure_path, filename)
    
    # Save the plot as a PNG file
    ggsave(filename = file_path, plot = plot, width = 8, height = 6)
    
    # Add plot to the list
    plots[[iq_name]] <- plot
  }
  
  return(plots)
}

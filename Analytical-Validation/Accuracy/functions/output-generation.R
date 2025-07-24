#' Generate Deming Regression Plots for Accuracy Analysis
#'
#' Creates scatter plots showing the relationship between Expert Panel scores and 
#' Reader scores (ML algorithm or human) with Deming regression line and identity line.
#' Supports both single and dual IQ level configurations.
#'
#' @param deming.fit A list containing analysis results from main_analysis(). Each element
#'   must contain:
#'   \itemize{
#'     \item data: Data frame with Expert.panel and Reader columns
#'     \item deming.report: Results from Deming regression with EST coefficients
#'   }
#'   List length must be 1 or 2 (for single or dual IQ levels).
#'
#' @return A list of ggplot objects. For single IQ level, returns list with one plot.
#'   For dual IQ levels, returns list with two plots (plot1, plot2). Each plot includes:
#'   \itemize{
#'     \item Scatter points of Expert Panel vs Reader scores
#'     \item Deming regression line (black, solid)
#'     \item Identity line (red, dashed)
#'     \item Correlation coefficient in caption
#'   }
#'   Also saves plots as PNG files to figure_path directory.
#'
#' @details
#' The function automatically detects single vs. dual IQ configuration based on list length.
#' File naming follows pattern: [IQ_level]_[reader_type]_[endpoint]_Deming_Plot.png
#' 
#' Plot specifications:
#' \itemize{
#'   \item Scale: 0-3 range for both axes with 0.5 unit breaks
#'   \item Legend: Positioned at bottom
#'   \item Size: 8" width x 6" height when saved
#' }
#'
#' @examples
#' \dontrun{
#'   # Single IQ level
#'   results <- main_analysis(data, "BVA")
#'   plots <- deming_plot(list(Standard = results))
#'   
#'   # Display plot
#'   print(plots$plot1)
#' }
#'
#' @seealso
#' \code{\link{main_analysis}} for generating input data
#' 
#' @export
deming_plot <- function(deming.fit) {
  
  # Check if deming.fit has the appropriate length
  if (length(deming.fit) < 1 || length(deming.fit) > 2) {
    stop("deming.fit must be of length 1 or 2")
  }
  
  # Function to generate and save the plot
  generate_plot <- function(fit_element, name, include_subtitle) {
    reader_type <- fit_element$data$reader_type[1]
    this_endpoint <- fit_element$data$this_endpoint[1]
    
    # Create a data frame for regression coefficients
    regression_coefs <- data.frame(
      Line = c("Deming Regression", "Identity"),
      slope = c(fit_element$deming.report$EST[2], 1),
      intercept = c(fit_element$deming.report$EST[1], 0),
      linetype = c(1, 2),
      color = c("black", "red")
    )
    
    # Basic plot setup
    plot <- fit_element$data %>%
      ggplot(aes(x = Expert.panel, y = Reader)) +
      geom_point() +
      geom_abline(data = regression_coefs, aes(slope = slope, intercept = intercept, colour = Line, linetype = Line), linewidth = 0.75) +
      scale_color_manual(values = c("black", "red")) +
      labs(
        x = "Expert Panel",
        y = reader_type,
        color = "",
        linetype = "",
        title = paste0("Deming Regression Fit for ", reader_type, " vs. Expert Panel on ", this_endpoint),
        caption = paste0(
          "Regression Line: Expert Panel = ", round(fit_element$deming.report$EST[1], 2), " + ",
          round(fit_element$deming.report$EST[2], 2), " * ", reader_type,
          "\\nPearson Correlation: ", round(cor(fit_element$data[, 1], fit_element$data[, 2], use = "complete.obs"), 3)
        )
      ) +
      scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
      scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
      theme(legend.position = "bottom")
    
    # Add subtitle if required
    if (include_subtitle) {
      plot <- plot + labs(subtitle = paste0("Image Quality: ", name))
    }
    
    # Construct the file name including reader_type and this_endpoint
    file_name <- paste0(name, "_", reader_type, "_", this_endpoint, "_Deming_Plot.png")
    
    # Ensure the path exists
    if (!dir.exists(figure_path)) {
      dir.create(figure_path, recursive = TRUE)
    }
    
    # Construct the full file path and save the plot
    full_file_path <- file.path(figure_path, file_name)
    ggsave(filename = full_file_path, plot = plot, width = 8, height = 6)
    
    return(list(plot1=plot))
  }
  
  if (length(deming.fit) == 1) {
    # Generate a single plot without subtitle
    plot1 <- generate_plot(deming.fit[[1]], names(deming.fit)[1], FALSE)
    return(plot1)
  } else {
    # Generate plots for both elements with subtitle
    plot1 <- generate_plot(deming.fit[[1]], names(deming.fit)[1], TRUE)
    plot2 <- generate_plot(deming.fit[[2]], names(deming.fit)[2], TRUE)
    return(list(plot1 = plot1, plot2 = plot2))
  }
}

#' Generate Deming Regression Coefficient Report Table
#'
#' Creates a formatted table summarizing Deming regression coefficients (intercept and slope)
#' with confidence intervals and p-values. Supports both single and dual IQ configurations.
#'
#' @param deming.fit A list containing analysis results from main_analysis(). Each element
#'   must contain a deming.report component with columns:
#'   \itemize{
#'     \item Term: "Intercept" or "Slope"
#'     \item EST: Point estimate
#'     \item LCL, UCL: 95% confidence interval bounds
#'     \item p: P-value for coefficient
#'     \item Endpoint: Endpoint name
#'     \item reader_type: Reader type identifier
#'   }
#'
#' @return A gt table object with formatted regression results. Table includes:
#'   \itemize{
#'     \item Term: Intercept and Slope coefficients
#'     \item Estimate [95% CI]: Combined point estimate and confidence interval
#'     \item p-value: Statistical significance
#'   }
#'   Also saves table as PNG file to table_path directory.
#'
#' @details
#' The function formats confidence intervals as "Estimate [LCL, UCL]" and rounds
#' all numeric values to 2 decimal places. For dual IQ configurations, results
#' are grouped by IQ level.
#' 
#' File naming: deming_report_[endpoint]_[reader_type].png
#'
#' @examples
#' \dontrun{
#'   results <- main_analysis(data, "BVA")
#'   table <- deming_regression_report(list(Standard = results))
#'   print(table)
#' }
#'
#' @seealso
#' \code{\link{main_analysis}} for generating input data
#' \code{\link{gt}} for table formatting
#' 
#' @export

  
deming_regression_report <- function(deming.fit = results.ML) {
  
  # Extract the Endpoint and Reader Type
  endpoint <- deming.fit[[1]]$deming.report$Endpoint[1]
  reader_type <- deming.fit[[1]]$deming.report$reader_type[1]
  
  # Define the filename including Endpoint and Reader Type
  filename <- paste0("deming_report_", endpoint, "_", reader_type, ".png")
  
  # Construct the full file path
  full_file_path <- file.path(table_path, filename)
  
  if (length(deming.fit) == 1) {
    # Generate the GT table without IQ column
    gt_table <- deming.fit[[1]]$deming.report %>%
      dplyr::rename(`Reader Type` = reader_type, `p-value` = p) %>%
      dplyr::select(Term, Endpoint, `Reader Type`, EST, LCL, UCL, `p-value`) %>%
      gt() %>%
      tab_header(
        title = "Deming Regression Output"
      ) %>%
      fmt_number(columns = c(EST, LCL, UCL, `p-value`), decimals = 2) %>%
      cols_merge(columns = c(EST, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(EST = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything())
  } else {
    # Generate the GT table with IQ column
    gt_table <- bind_rows(
      deming.fit[[1]]$deming.report %>% dplyr::mutate(IQ = names(deming.fit)[1]),
      deming.fit[[2]]$deming.report %>% dplyr::mutate(IQ = names(deming.fit)[2])
    ) %>%
      dplyr::rename(`Reader Type` = reader_type, `p-value` = p) %>%
      dplyr::select(IQ, Term, Endpoint, `Reader Type`, EST, LCL, UCL, `p-value`) %>%
      group_by(IQ) %>%
      gt() %>%
      tab_header(
        title = "Deming Regression Output"
      ) %>%
      fmt_number(columns = c(EST, LCL, UCL, `p-value`), decimals = 2) %>%
      cols_merge(columns = c(EST, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(EST = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything())
  }
  
  # Save the GT table as PNG
  gt::gtsave(data = gt_table, filename = full_file_path)
  
  return(gt_table)
}
#' Generate Deming Regression: Bias and Linearity Acceptance Criteria Report
#'
#' @param deming.fit A list where each element contains the data and results of Deming regression analysis.
#' @return A gt table summarizing the Bias and Linearity Acceptance Criteria for the specified endpoint and reader type, and saves it as a PNG file.
#' @export
#'
#' @examples
#' deming_bias_linearity_check(list(WideIQ_Std = deming_fit_data_1, NarrowIQ_Std = deming_fit_data_2))
deming_bias_linearity_check <- function(deming.fit = results.ML) {
  
  # Extract the Endpoint and Reader Type
  endpoint <- deming.fit[[1]]$Linearity.check$Endpoint[1]
  reader_type <- deming.fit[[1]]$Linearity.check$reader_type[1]
  
  # Define the filename including Endpoint and Reader Type
  filename <- paste0("deming_bias_linearity_check_", endpoint, "_", reader_type, ".png")
  
  # Construct the full file path
  full_file_path <- file.path(table_path, filename)
  
  if (length(deming.fit) == 1) {
    # Generate the GT table without IQ column
    gt_table <- deming.fit[[1]]$Linearity.check %>%
      dplyr::rename(Criteria = Check) %>%
      gt() %>%
      tab_header(
        title = paste("Deming Regression: Bias and Linearity Acceptance Criteria")
      ) %>%
      tab_footnote(
        footnote = "Insignificant cubic term in cubic regression model.",
        locations = cells_body(
          columns = Criteria, rows = 1
        )
      ) %>%
      tab_footnote(
        footnote = "Insignificant quadratic term in a quadratic regression model.",
        locations = cells_body(
          columns = Criteria, rows = 2
        )
      ) %>%
      tab_footnote(
        footnote = "Intercept: Absolute value < 0.5",
        locations = cells_body(
          columns = Criteria, rows = 3
        )
      ) %>%
      tab_footnote(
        footnote = "Slope in [0.8, 1.25]",
        locations = cells_body(
          columns = Criteria, rows = 4
        )
      ) %>%
      tab_footnote(
        footnote = "Linearity: All previous criteria met.",
        locations = cells_body(
          columns = Criteria, rows = 5
        )
      )
    
  } else {
    # Generate the GT table with IQ column
    gt_table <- bind_rows(
      deming.fit[[1]]$Linearity.check %>% dplyr::mutate(IQ = names(deming.fit)[1]),
      deming.fit[[2]]$Linearity.check %>% dplyr::mutate(IQ = names(deming.fit)[2])
    ) %>%
      dplyr::rename(Criteria = Check) %>%
      group_by(IQ) %>%
      gt() %>%
      tab_header(
        title = paste("Deming Regression: Bias and Linearity Acceptance Criteria")
      ) %>%
      tab_footnote(
        footnote = "Insignificant cubic term in cubic regression model.",
        locations = cells_body(
          columns = Criteria, rows = 1
        )
      ) %>%
      tab_footnote(
        footnote = "Insignificant quadratic term in a quadratic regression model.",
        locations = cells_body(
          columns = Criteria, rows = 2
        )
      ) %>%
      tab_footnote(
        footnote = "Intercept: Absolute value < 0.5",
        locations = cells_body(
          columns = Criteria, rows = 3
        )
      ) %>%
      tab_footnote(
        footnote = "Slope in [0.8, 1.25]",
        locations = cells_body(
          columns = Criteria, rows = 4
        )
      ) %>%
      tab_footnote(
        footnote = "Linearity: All previous criteria met.",
        locations = cells_body(
          columns = Criteria, rows = 5
        )
      )
  }
  
  # Save the GT table as PNG
  gt::gtsave(data = gt_table, filename = full_file_path)
  
  return(gt_table)
}


#' Generate Summary of Cubic and Quadratic Regression
#'
#' @param deming.fit A list where each element contains the data and results of Deming regression analysis.
#' @return A gt table summarizing the results of cubic and quadratic regression for the specified reader type and endpoint, and saves it as a PNG file.
#' @export
#'
#' @examples
#' quad_cubic_summary(list(WideIQ_Std = deming_fit_data_1, NarrowIQ_Std = deming_fit_data_2))
quad_cubic_summary <- function(deming.fit = results.ML) {
  
  # Extract the Reader Type and Endpoint
  reader_type <- deming.fit[[1]]$data$reader_type[1]
  this_endpoint <- deming.fit[[1]]$data$this_endpoint[1]
  
  # Define the filename including Reader Type and Endpoint
  filename <- paste0("quad_cubic_summary_", reader_type, "_", this_endpoint, ".png")
  
  # Construct the full file path
  full_file_path <- file.path(table_path, filename)
  
  if (length(deming.fit) == 1) {
    # Generate the GT table without IQ column
    gt_table <- tidy(deming.fit[[1]]$cubic.fit, conf.int = TRUE) %>%
      dplyr::mutate(Model = "Cubic Regression") %>%
      dplyr::mutate(Endpoint = this_endpoint, `Reader Type` = reader_type) %>%
      dplyr::select(Model, term, everything(), -std.error, -statistic, -Endpoint, -`Reader Type`) %>%
      dplyr::rename(Estimate = estimate, Term = term, `p-value` = p.value) %>%
      gt() %>%
      fmt_number(columns = c(Estimate, conf.low, conf.high, `p-value`), decimals = 2) %>%
      cols_merge(columns = c(Estimate, conf.low, conf.high), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      tab_header(
        title = paste("Cubic Regression: Expert Panel vs", reader_type, "on", this_endpoint)
      ) %>%
      tab_footnote(
        footnote = "Seeking insignificant cubic term in cubic regression model.",
        locations = cells_body(
          columns = Term, rows = 4
        )
      )
  } else {
    # Generate the GT table with IQ column
    gt_table <- bind_rows(
      tidy(deming.fit[[1]]$cubic.fit, conf.int = TRUE) %>% dplyr::mutate(Model = "Cubic Regression", IQ = names(deming.fit)[1]),
      tidy(deming.fit[[2]]$quadratic.fit, conf.int = TRUE) %>% dplyr::mutate(Model = "Quadratic Regression", IQ = names(deming.fit)[2])
    ) %>%
      dplyr::mutate(Endpoint = this_endpoint, `Reader Type` = reader_type) %>%
      dplyr::select(Model, term, everything(), -std.error, -statistic, -Endpoint, -`Reader Type`) %>%
      dplyr::rename(Estimate = estimate, Term = term, `p-value` = p.value) %>%
      group_by(IQ) %>%
      gt() %>%
      fmt_number(columns = c(Estimate, conf.low, conf.high, `p-value`), decimals = 2) %>%
      cols_merge(columns = c(Estimate, conf.low, conf.high), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      tab_header(
        title = paste("Cubic and Quadratic Regression: Expert Panel vs", reader_type, "on", this_endpoint)
      ) %>%
      tab_footnote(
        footnote = "Seeking insignificant cubic term in cubic regression model.",
        locations = cells_body(
          columns = Term, rows = 4
        )
      ) %>%
      tab_footnote(
        footnote = "Seeking insignificant quadratic term in quadratic regression model.",
        locations = cells_body(
          columns = Term, rows = 7
        )
      )
  }
  
  # Save the GT table as PNG
  gt::gtsave(data = gt_table, filename = full_file_path)
  
  return(gt_table)
}

#' Generate Confidence Intervals Report Comparing Reader Type vs. Expert Panel
#'
#' @param deming.fit A list where each element contains the data and results of Deming regression analysis.
#' @return A gt table summarizing the confidence intervals comparing the reader type vs. expert panel for the specified endpoint, and saves it as a PNG file.
#' @export
#'
#' @examples
#' report_truth_coverage_CIs(list(WideIQ_Std = deming_fit_data_1, NarrowIQ_Std = deming_fit_data_2))
report_truth_coverage_CIs <- function(deming.fit = results.ML) {
  
  # Extract the Reader Type and Endpoint
  reader_type <- deming.fit[[1]]$data$reader_type[1]
  this_endpoint <- deming.fit[[1]]$data$this_endpoint[1]
  
  # Define the filename including Reader Type and Endpoint
  filename <- paste0("truth_coverage_CIs_", reader_type, "_", this_endpoint, ".png")
  
  # Construct the full file path
  full_file_path <- file.path(table_path, filename)
  
  if (length(deming.fit) == 1) {
    # Generate the GT table without IQ column
    gt_table <- deming.fit[[1]]$CIs %>%
      dplyr::rename(Metric = metric, `Reader Type` = reader_type) %>%
      dplyr::select(Metric, n, Estimate, LCL, UCL) %>%
      gt() %>%
      tab_header(
        title = paste("Confidence Intervals comparing:", reader_type, "vs. Expert Panel on", this_endpoint)
      ) %>%
      fmt_number(columns = c(Estimate, `LCL`, `UCL`), decimals = 2) %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything())
  } else {
    # Generate the GT table with IQ column
    gt_table <- bind_rows(
      deming.fit[[1]]$CIs %>% dplyr::mutate(IQ = names(deming.fit)[1]),
      deming.fit[[2]]$CIs %>% dplyr::mutate(IQ = names(deming.fit)[2])
    ) %>%
      dplyr::rename(Metric = metric, `Reader Type` = reader_type) %>%
      dplyr::select(IQ, Metric, n, Estimate, LCL, UCL) %>%
      group_by(IQ) %>%
      gt() %>%
      tab_header(
        title = paste("Confidence Intervals comparing:", reader_type, "vs. Expert Panel on", this_endpoint)
      ) %>%
      fmt_number(columns = c(Estimate, `LCL`, `UCL`), decimals = 2) %>%
      cols_merge(columns = c(Estimate, LCL, UCL), pattern = "{1} [{2}, {3}]") %>%
      cols_label(Estimate = "Estimate [95% CI]") %>%
      cols_align(align = "center", columns = everything())
  }
  
  # Save the GT table as PNG
  gt::gtsave(data = gt_table, filename = full_file_path)
  
  return(gt_table)
}

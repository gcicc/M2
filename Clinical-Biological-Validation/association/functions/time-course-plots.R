# write.csv(analysis.data.in, path(output_dir, paste0("analysis.data.in-", reader_type, "-", "Phase0", ".csv")) )
# analysis.data.in <- read.csv(path(output_dir, paste0("analysis.data.in-", reader_type, "-", "Phase0", ".csv"))) 
# analysis.data.in <- read.csv("C:\\Users\\eri7441\\OneDrive - Takeda\\Documents - MARCS-Validation-Data\\Output
# \\Single Timepoint Association\\2024-10-23 TAK-062-2001 GC\\6a merged_data.ML.full.csv")
#

#' Generate Time Course Plots
#'
#' This function takes a data frame and generates time course plots for specified variables.
#' It processes the data, fits smoothing curves, and saves the resulting plots as an image.
#'
#' @param df.in Data frame containing the input data.
#' @param reader_type The type of reader used, for use in filenames.
#' @param this_endpoint The endpoint of interest in the analysis.
#' @return Generated plot object.
#' @import dplyr
#' @import ggplot2
#' @importFrom dplyr filter select mutate group_by arrange rename ungroup left_join
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs facet_wrap theme scale_colour_manual ggsave
#' @importFrom lubridate ymd
time.course.plots <- function(df.in, reader_type, this_endpoint) {
  
  # Function to process data for plotting
  process_data <- function(df, obsdate_col, select_cols, pivot_cols) {
    if(df$STUDYID[1] == "TAK-062-2001") df <- df %>% dplyr::filter(ARM %in% c("Placebo", "TAK-062"))
    df %>%
      dplyr::filter(!is.na(ENRDATE)) %>%
      dplyr::select(STUDYID, SITE, SUBJID, ARM, VISIT, !!sym(obsdate_col), ENRDATE, all_of(select_cols)) %>%
      group_by(STUDYID, SITE, SUBJID) %>%
      dplyr::mutate(
        key = paste(STUDYID, SITE, SUBJID),
        elapsed.time = as.numeric(difftime(!!sym(obsdate_col), ENRDATE, units = "days")),
        n.visits = n(),
        n = 1:n()
      ) %>%
      dplyr::arrange(key, !!sym(obsdate_col)) %>%
      dplyr::rename(OBSDATE = !!sym(obsdate_col)) %>%
      pivot_longer(cols = all_of(pivot_cols), names_to = "measure", values_to = "value") %>%
      dplyr::mutate(OBSDATE = ymd(OBSDATE)) %>%
      dplyr::filter(!is.na(value), !is.na(OBSDATE))
  }
  
  # Process data for various endpoint and measures
  temp.vce <- process_data(df = df.in, obsdate_col = "OBSDATE.VCE", select_cols = this_endpoint, pivot_cols = this_endpoint)
  temp.hist <- process_data(df = df.in, obsdate_col = "OBSDATE.histology", select_cols = c("VHCD", "IELCOUNT"), pivot_cols = c("VHCD", "IELCOUNT"))
  temp.symp <- process_data(df = df.in, obsdate_col = "OBSDATE.symptoms", select_cols = "GISS", pivot_cols = "GISS")
  
  # Combine processed data
  temp <- bind_rows(temp.vce, temp.hist, temp.symp)
  
  # Standardize scores
  temp <- temp %>% 
    group_by(measure) %>%
    dplyr::mutate(
      value = ifelse(measure == "VHCD", -value, value),
      value = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    dplyr::filter(is.na(value) == FALSE) %>%
    group_by(key, measure)
  
  measures <- c(this_endpoint, "VHCD", "GISS", "IELCOUNT")
  for.plot <- apply(matrix(1:length(measures)), 1, function(y) {
    apply(matrix(1:length(unique(temp$key))), 1, function(x) {
      subject.df <- temp %>% dplyr::filter(key == unique(temp$key)[x], measure == measures[y])
      
      # Subset to those with more than one visit
      if (nrow(subject.df) > 1) {
        spline.df.fmm <- data.frame(spline(subject.df$elapsed.time, subject.df$value, method = "fmm", n = 30)) %>% dplyr::rename(fmm = y)
        spline.df.natural <- data.frame(spline(subject.df$elapsed.time, subject.df$value, method = "natural", n = 30)) %>% dplyr::rename(natural = y)
        
        loess.out <- tryCatch({
          loess_fit <- loess(value ~ elapsed.time, data = subject.df, span = 1)
          time_seq <- seq(min(subject.df$elapsed.time, na.rm = TRUE), max(subject.df$elapsed.time, na.rm = TRUE), length.out = 30)
          loess_estimates <- predict(loess_fit, newdata = data.frame(elapsed.time = time_seq), se = FALSE)
          data.frame(x = time_seq, loess = loess_estimates)
        }, error = function(e) {
          data.frame(x = time_seq, loess = NA)
        })
        
        for.key <- unique(temp$key)[x]
        for.measure <- measures[y]
        df.out <- spline.df.fmm %>% left_join(spline.df.natural) %>% left_join(loess.out) %>% 
          dplyr::mutate(key = for.key, measure = for.measure)
      } else {
        return(NULL)
      }
    })
  }) %>% bind_rows()
  
  for.plot <- for.plot %>% 
    left_join(temp %>% ungroup() %>% dplyr::select(key, ARM), by = c("key")) %>% 
    group_by(key, measure) %>% 
    dplyr::filter(!is.na(ARM)) %>% ungroup() %>%
    dplyr::filter(!is.na(loess))
  
  arms <- length(unique(na.omit(for.plot$ARM)))
  pdf.out <- for.plot %>% dplyr::filter(measure != "VCIEL") %>%
    group_by(key, measure) %>%
    ggplot(aes(x = x, y = fmm, group = key, color = ARM)) +
    labs(
      title = paste0("Time Course Plots for ", this_endpoint, " and comparators"),
      x = "Days Elapsed", y = "Measure (z-score)", 
      caption = "VHCD has been scaled so that higher scores are associated with disease severity"
    ) +
    geom_line(aes(x = x, y = loess), size = 0.5) + scale_colour_manual(values = cbp2) +
    facet_wrap(measure ~ ARM, scales = "free_y", ncol = arms) +
    geom_point(data = temp, aes(x = elapsed.time, y = value, color = ARM, group = key)) +
    theme(legend.position = "bottom")
  
  ggsave(pdf.out, filename = path(figure_path, paste0("time-course-plots", ".png")), height = 8, width = 6.5, units = "in", dpi = 300)
  
  return(pdf.out)
}
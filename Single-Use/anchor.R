#' Title
#'
#' @param data.in
#'
#' @return
#' @export
#'
#' @examples
#' for.anchoring <- data.frame(
#' CE-VAST = sample(c(0, 1, 2, 3), size = 80, replace = TRUE),
#' MARSH = sample(c("0", "1", "2", "3a", "3b", "3c"), size = 80, replace = TRUE)
#' )
anchor <- function(data.in = for.anchoring) {
  # convert MARSH to ordinal
  data.in$MarshO.scale.int <- as.integer(as.factor(data.in$MarshO.scale))
  bind_rows(
    tidy(t.test(x = data.in$MarshO.scale.int)) %>% mutate(metric = "MARSHO"),
    tidy(t.test(x = data.in$CE-VAST)) %>% mutate(metric = "CE-VAST"),
    tidy(t.test(x = data.in$MarshO.scale.int, y = data.in$CE-VAST)) %>% mutate(metric = "Difference")
  ) %>%
    dplyr::select(metric, estimate, conf.low, conf.high) %>%
    gt() %>%
    cols_label(
      matches("metric") ~ "Metric",
      matches("estimate") ~ "Estimate",
      matches("conf.low") ~ "LCL",
      matches("conf.high") ~ "UCL"
    ) %>%
    fmt_number(decimals = 2) %>%
    text_replace(pattern = "MARSHO", replacement = "Marsh-Oberhuber") %>%
    text_replace(pattern = "CE-VAST", replacement = "CE-VAST") %>%
    tab_footnote(
      footnote = "95% Confidence Intervals",
      locations = cells_column_labels(columns = c("conf.low", "conf.high"))
    ) %>%
    tab_header(title = "Comparison of arithmetic means of CE-VAST and Marsh-Oberhuber Histologic Scores (mapped scores)", subtitle="subtitle")
  # NOTE For footnote we have to respect original column name!
}


anchor.c <- function(data.in = histology.correlation.data.long) {
  bind_rows(
    tidy(t.test(x = data.in$MarshO.scale.C)) %>% mutate(metric = "MARSHO"),
    tidy(t.test(x = data.in$CE-VAST.C)) %>% mutate(metric = "CE-VAST"),
    tidy(t.test(x = data.in$MarshO.scale.C, y = data.in$CE-VAST.C)) %>% mutate(metric = "Difference")
  ) %>%
    dplyr::select(metric, estimate, conf.low, conf.high) %>%
    gt() %>%
    cols_label(
      matches("metric") ~ "Metric",
      matches("estimate") ~ "Estimate",
      matches("conf.low") ~ "LCL",
      matches("conf.high") ~ "UCL"
    ) %>%
    fmt_number(decimals = 2) %>%
    text_replace(pattern = "MARSHO", replacement = "Marsh-Oberhuber") %>%
    text_replace(pattern = "CE-VAST", replacement = "CE-VAST") %>%
    tab_footnote(
      footnote = "95% Confidence Intervals",
      locations = cells_column_labels(columns = c("conf.low", "conf.high"))
    ) %>%
    tab_header(title = "Comparison of arithmetic means of CE-VAST and Marsh-Oberhuber Histologic Scores (mapped scores)",
               subtitle="Longitudinal Differences")

  }


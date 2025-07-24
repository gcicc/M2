

LCLUCL.update <- function(gt.in) {
  gt.in %>%
    cols_merge(
      columns = c(LCL, UCL),
      pattern = "({1}, {2})"
    ) %>%
    cols_label(
      LCL = "Confidence Interval"
    )
}
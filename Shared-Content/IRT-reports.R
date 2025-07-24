#' report.common.alpha
#'
#' @param common.alpha
#'
#' @return
#' @export
#'
#' @examples
report.common.alpha <- function(common.alpha = my.common.alpha){
  # Column 1: intercepts, Column2: slopes
  data.frame(common.alpha$parmatrix) %>%
    dplyr::rename(Intercepts=X1, Slopes=X2) %>%
    mutate(`Common SD` = common.alpha$stdmixt,
           Item = c("VHCD", "ILE", "CDSD")) %>% dplyr::select(Item, everything()) %>%
    gt() %>%
    tab_header(
      title = "Intercepts and Slopes from Item Response Model - Common alpha",
      subtitle = "Population: PP"
    ) %>%
    fmt_number(
      columns = c(Intercepts, Slopes, `Common SD`), decimals = 2
    )
}

#' report.uncommon.alpha
#'
#' @param uncommon.alpha
#'
#' @return
#' @export
#'
#' @examples
report.uncommon.alpha <- function(uncommon.alpha = my.uncommon.alpha){
  # Column 1: intercepts, Column2: slopes
  data.frame(uncommon.alpha$parmatrix) %>%
    dplyr::rename(Intercepts=X1, Slopes=X2) %>%
    mutate(`Common SD` = uncommon.alpha$stdmixt,
           Item = c("VHCD", "ILE", "CDSD")) %>% dplyr::select(Item, everything()) %>%
    gt() %>%
    tab_header(
      title = "Intercepts and Slopes from Item Response Model - Uncommon alpha",
      subtitle = "Population: PP"
    ) %>%
    fmt_number(
      columns = c(Intercepts, Slopes, `Common SD`), decimals = 2
    )
}

#' LRT
#'
#' @param model1
#' @param model2
#'
#' @return
#' @export
#'
#' @examples
LRT <- function(model1 = my.common.alpha.model, model2 = my.uncommon.alpha.model){
  tibble(model = c("Common Alpha", "Uncommon Alpha"),
         AIC = c(model1$AIC, model2$AIC),
         BIC = c(model1$BIC, model2$BIC),
         `Log Likelihood` = c(model1$Loglik, model2$Loglik),
         `Test Statistic` = c(2*(model2$Loglik - model1$Loglik), NA)) %>%
    mutate(`p-value` = 1 - pchisq(q = `Test Statistic`, df=2)) %>%
    gt() %>%
    tab_header(title = "Model Fit", subtitle="PP") %>%
    fmt_number(
      columns = c(AIC,	BIC,	`Log Likelihood`), decimals = 1
    ) %>%
    fmt_number(columns = c(`Test Statistic`, `p-value`), decimals = 4) %>%
    sub_missing(
      columns = 5:6,
      missing_text = "--"
    )

}

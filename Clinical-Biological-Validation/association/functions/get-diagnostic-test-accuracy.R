#' Obuchowski.hat
#'
#' @param data.in
#' @param gold
#' @param diag
#'
#' @return
#' @export
#'
#' @examples
Obuchowski.hat  <- function(data.in = cont.GS1, gold = "VhCD", diag = "BVA") {
  build <- expand.grid(i = data.in$SUBJID, j = data.in$SUBJID)
  for.build.1 <- data.in %>% rename(i = SUBJID, MARCS_i = diag, GOLD_i = gold)
  for.build.2 <- data.in %>% rename(j = SUBJID, MARCS_j = diag, GOLD_j = gold)
  build <- build %>%
    left_join(for.build.1) %>%
    left_join(for.build.2)
  build <- build %>% mutate(phi = case_when(
    GOLD_i > GOLD_j & MARCS_i > MARCS_j ~ 1,
    GOLD_i == GOLD_j | MARCS_i == MARCS_j ~ 0.5,
    .default = 0
  ))
  return(list(build, build %>% summarize(1 / (n() * (n() - 1)) * sum(phi))))
}

#' get.diag.test.accuracy
#'
#' @param Obuchowski.in
#'
#' @return
#' @export
#'
#' @examples
Obuchowski.CI <- function(Obuchowski.in = Obuchowski.hat, gold="GS1") {
  var.theta.hat <- Obuchowski.in[[1]] %>%
    filter(i != j) %>%
    group_by(i) %>%
    summarize(phi.sum = 1 / n() * (sum(phi))) %>%
    mutate(
      theta.hat = Obuchowski.in[[2]][1, 1],
      summands = (phi.sum - theta.hat)^2
    ) %>%
    ungroup() %>%
    summarize(var.theta.hat = 1 / (n() / 2) * 1 / (n() / 2 - 1) * sum(summands)) %>% pull()
  data.frame(
    `Gold Standard` = gold,
    Estimate = Obuchowski.in[[2]][1, 1],
    LCI = Obuchowski.in[[2]][1, 1] - sqrt(var.theta.hat) * 1.96,
    UCI = Obuchowski.in[[2]][1, 1] + sqrt(var.theta.hat) * 1.96
  )
}

Obuchowski.report <- function(Obuchowski.CI.in = list(CI1, CI2, CI3, CI4)){
  bind_rows(
  apply(matrix(1:length(Obuchowski.CI.in)), 1, function(x){
    Obuchowski.CI.in[[x]]
  })) %>% gt() %>%
    tab_header(title="Obuchowski estimator and confidence interval of diagnostic test accuracy") %>%
    fmt_number(columns = c(Estimate, LCI, UCI), decimals = 3)


}

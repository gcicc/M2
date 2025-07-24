#' BVA.VhCD.correlation.check
#'
#' @param data.in
#'
#' @return
#' @export
#'
#' @examples
#' histology.correlation.data <- expand.grid(SUBJID = 1:40, VISIT = c("Screen", "12w", "24w")) %>%
#' mutate(
#'   BVA = rnorm(n(), 0, 1),
#'     VhCD = rnorm(n(), 0, 1),
#'       CDSD = rnorm(n(), 7, 7 / 3),
#'         MarshO.scale = sample(x = c(0, 1, 2, 3), replace = T, size = n())
#'         ) %>%
#'           mutate(Agreement = case_when(
#'               MarshO.scale > 2 & CDSD > 7 ~ "Good",
#'                   MarshO.scale <= 2 & CDSD <= 3 ~ "Good",
#'                       MarshO.scale > 2 & CDSD <= 3 ~ "Poor",
#'                           MarshO.scale <= 2 & CDSD > 7 ~ "Poor",
#'                               .default = "Moderate"
#'                                 ))

BVA.VhCD.correlation.check <- function(data.in= histology.correlation.data){
All <- data.in %>%
  group_by(VISIT) %>%
  dplyr::summarize(
    `BVA:VhCD` = cor(BVA, VhCD)^2,
    `BVA:GISS` = cor(BVA, CDSD)^2,
    `VhCD:GISS` = cor(VhCD, CDSD)^2
  ) %>% dplyr::mutate(Bin = "All")

Good <- data.in %>%
  dplyr::filter(Agreement == "Good") %>%
  group_by(VISIT) %>%
  dplyr::summarize(
    `BVA:VhCD` = cor(BVA, VhCD)^2,
    `BVA:GISS` = cor(BVA, CDSD)^2,
    `VhCD:GISS` = cor(VhCD, CDSD)^2
  )%>% dplyr::mutate(Bin = "Good")

Moderate <- data.in %>%
  dplyr::filter(Agreement == "Moderate") %>%
  group_by(VISIT) %>%
  dplyr::summarize(
    `BVA:VhCD` = cor(BVA, VhCD)^2,
    `BVA:GISS` = cor(BVA, CDSD)^2,
    `VhCD:GISS` = cor(VhCD, CDSD)^2
  ) %>% dplyr::mutate(Bin = "Moderate")

Poor <- data.in %>%
  dplyr::filter(Agreement == "Poor") %>%
  group_by(VISIT) %>%
  dplyr::summarize(
    `BVA:VhCD` = cor(BVA, VhCD)^2,
    `BVA:GISS` = cor(BVA, CDSD)^2,
    `VhCD:GISS` = cor(VhCD, CDSD)^2
  )%>% dplyr::mutate(Bin = "Poor")

bind_rows(
  All,
  Good,
  Moderate,
  Poor
) %>% dplyr::select(Bin, VISIT, everything()) %>%
  gt() %>%
  tab_header(
    title = "R\U00B2 values",
    subtitle = "Population: PP"
  ) %>%
  fmt_number(
    columns = c(`BVA:VhCD`, `BVA:GISS`, `VhCD:GISS`), decimals = 2
  ) %>%
  tab_footnote(footnote = "GISS: GI Severity Score")
}


BVA.VhCD.correlation.check.c <- function(data.in= histology.correlation.data.long){
  All <- data.in %>%
    dplyr::summarize(
      `BVA:VhCD` = cor(BVA.C, VhCD.C)^2,
      `BVA:GISS` = cor(BVA.C, CDSD.C)^2,
      `VhCD:GISS` = cor(VhCD.C, CDSD.C)^2
    ) %>% dplyr::mutate(Bin = "All")

bind_rows(
    All
  ) %>% dplyr::select(Bin, everything()) %>%
    gt() %>%
    tab_header(
      title = "R\U00B2 values",
      subtitle = "Population: PP"
    ) %>%
    fmt_number(
      columns = c(`BVA:VhCD`, `BVA:GISS`, `VhCD:GISS`), decimals = 2
    )%>%
  tab_footnote(footnote = "GISS: GI Severity Score")
}

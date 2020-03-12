#' Linear Models
#'
#' A function that takes a primary dependent variable and a primary independent variable and runs a series 
#' of ordinary least squares regressions with clustered standard errors. This function also creates .tex 
#' outputs and saves them to the global workspace to be copied into Overleaf. This function will be sourced 
#' into 03_tjbrailey_data_analysis.Rmd.
#' 
#' @param dv Dependent variable.
#' @param dv Independent variable.
#' @return lm_full_tex Regression table.

psp_lm <- function(dv, iv){
  
  # Load packages
  library(magrittr)
  library(ggplot2)
  
  # Load data
  psp <- read.csv(paste0(here::here(), "/data/tjbrailey_psp_clean.csv"))

  psp <- psp %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    dplyr::filter(qog_fe_etfra > median(psp$qog_fe_etfra, na.rm = TRUE))
  
  lm1 <- estimatr::lm_robust(!!sym(dv) ~ !!sym(iv) + as.factor(country), 
                             data = psp, se_type = "CR2", clusters = country)
  
  lm2 <- estimatr::lm_robust(!!sym(dv) ~ !!sym(iv) + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + polity4_polity_score + tb_other_provis
                             , 
                             data = psp, se_type = "CR2", clusters = country)
  
  lm3 <- estimatr::lm_robust(!!sym(dv) ~ !!sym(iv) * tb_other_provis + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + polity4_polity_score
                               , 
                             data = psp, se_type = "CR2", clusters = country)
  
  
  lm1_tex <- texreg::texreg(lm1, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm2_tex <- texreg::texreg(lm5, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm3_tex <- texreg::texreg(lm6, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  
  
  lm_full_tex <- texreg::texreg(list(lm1, lm2, lm3), 
                                mfrow = TRUE, 
                                omit.coef = "as.factor", 
                                include.ci = FALSE, 
                                custom.gof.rows = list(`Country fixed effects` = c("Y", "Y", "Y"),
                                                        `Year fixed effects` = c("N", "Y", "Y")))
  
  lm_full_tex
  
}
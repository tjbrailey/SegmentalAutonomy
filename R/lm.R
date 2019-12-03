# Linear Models

### A function that takes a primary dependent variable and a primary independent variable and runs a series 
### of ordinary least squares regressions with clustered standard errors. This function also creates .tex 
### outputs and saves them to the global workspace to be copied into Overleaf. This function will be sourced 
### into 03_tjbrailey_data_analysis.Rmd.

# Input Variables
# - dv - Dependent Variable
# - iv - Dependent Variable

psp_lm <- function(dv, iv){
  
  # Load packages
  library(magrittr)
  library(ggplot2)
  
  # Load data
  psp <- rio::import(paste0(here::here(), "/data/tjbrailey_psp_clean.csv"))
  psp <- psp[,-1]
  
  lm1 <- estimatr::lm_robust(get(dv) ~ get(iv) + as.factor(country), data = psp, se_type = "CR2", clusters = country)
  lm2 <- estimatr::lm_robust(get(dv) ~ get(iv) + as.factor(country) + as.factor(year), data = psp, se_type = "CR2", clusters = country)
  lm3 <- estimatr::lm_robust(get(dv) ~ get(iv) + as.factor(country) + as.factor(year) + qog_gle_gdp, data = psp, se_type = "CR2", clusters = country)
  lm4 <- estimatr::lm_robust(get(dv) ~ get(iv) + as.factor(country) + as.factor(year) + qog_gle_gdp + qog_wbgi_pve, data = psp, se_type = "CR2", clusters = country)
  lm5 <- estimatr::lm_robust(get(dv) ~ (get(iv) * other_provis) + as.factor(country) + as.factor(year) + qog_gle_gdp + qog_wbgi_pve, data = psp, se_type = "CR2", clusters = country)
  
  lm1_tex <- texreg::texreg(lm1, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm2_tex <- texreg::texreg(lm2, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm3_tex <- texreg::texreg(lm3, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm4_tex <- texreg::texreg(lm4, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm5_tex <- texreg::texreg(lm5, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  
  lm_full_tex <- texreg::texreg(list(lm1, lm2, lm3, lm4, lm5), 
                                mfrow = TRUE, 
                                omit.coef = "as.factor", 
                                include.ci = FALSE, 
                                custom.gof.rows = list(`Country fixed effects` = c("Y", "Y", "Y", "Y", "Y"),
                                                        `Year fixed effects` = c("N", "Y", "Y", "Y", "Y")))
  lm_full_tex
  
}
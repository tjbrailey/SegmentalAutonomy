# Linear Models

### A function that takes a primary dependent variable and a primary independent variable and runs a series 
### of ordinary least squares regressions with clustered standard errors. This function also creates .tex 
### outputs and saves them to the global workspace to be copied into Overleaf. This function will be sourced 
### into 03_tjbrailey_data_analysis.Rmd.

# Input Variables
# - dv - Dependent Variable
# - iv - Independent Variable

psp_lm <- function(dv, iv){
  
  # Load packages
  library(magrittr)
  library(ggplot2)
  
  # Load data
  #ttt <- "https://raw.githubusercontent.com/tjbrailey/SeniorThesis/master/data/tjbrailey_psp_clean.csv?token=ALBH6OPEF7NLZMCVGMCHFEK6IHP44"
  #psp <- read.csv(url(ttt))
  psp <- read.csv(paste0(here::here(), "/data/tjbrailey_psp_clean.csv"))
  #psp <- psp[,-1]
  
  psp <- psp %>%
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    dplyr::filter(qog_fe_etfra > median(psp$qog_fe_etfra, na.rm = TRUE))
  
  lm1 <- estimatr::lm_robust(!!sym(dv) ~ !!sym(iv) + as.factor(country), 
                             data = psp, se_type = "CR2", clusters = country)
  
  lm5 <- estimatr::lm_robust(!!sym(dv) ~ !!sym(iv) + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + polity4_polity_score + tb_other_provis
                             , 
                             data = psp, se_type = "CR2", clusters = country)
  
  lm6 <- estimatr::lm_robust(!!sym(dv) ~ (!!sym(iv) * tb_other_provis) + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + polity4_polity_score
                               , 
                             data = psp, se_type = "CR2", clusters = country)
  
  
  lm1_tex <- texreg::texreg(lm1, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  #lm2_tex <- texreg::texreg(lm2, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  #lm3_tex <- texreg::texreg(lm3, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  #lm4_tex <- texreg::texreg(lm4, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm5_tex <- texreg::texreg(lm5, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm6_tex <- texreg::texreg(lm6, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  
  
  lm_full_tex <- texreg::texreg(list(lm1, #lm2, lm3, lm4, 
                                     lm5, lm6), 
                                mfrow = TRUE, 
                                omit.coef = "as.factor", 
                                include.ci = FALSE, 
                                custom.gof.rows = list(`Country fixed effects` = c("Y", #"Y", "Y", "Y", 
                                                                                   "Y", "Y"),
                                                        `Year fixed effects` = c("N", #"Y", "Y", "Y", 
                                                                                 "Y", "Y")))
  
  lm_full_tex
  
  
}
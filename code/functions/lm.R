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

psp_lm <- 
  function(dv, iv){

  lm1 <- 
    estimatr::lm_robust(
      !!sym(dv) ~ !!sym(iv) + as.factor(country), 
      data = psp, 
      se_type = "CR2",
      clusters = country
      )
  
  lm2 <- 
    estimatr::lm_robust(
      !!sym(dv) ~ !!sym(iv) + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + vdem_e_polity2 + tb_other_provis, 
      data = psp, 
      se_type = "CR2", 
      clusters = country
      )
  
  lm3 <- 
    estimatr::lm_robust(
      !!sym(dv) ~ !!sym(iv) * tb_other_provis + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + vdem_e_polity2, 
      data = psp, 
      se_type = "CR2", 
      clusters = country
      )
  
  
  lm1_tex <- texreg::texreg(lm1, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm2_tex <- texreg::texreg(lm2, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  lm3_tex <- texreg::texreg(lm3, omit.coef = "as.factor", include.ci = FALSE, custom.gof.rows = list(`Fixed effects` = ("Y")))
  
  
  lm_full_tex <- 
    texreg::texreg(
      list(lm1, lm2, lm3), 
      mfrow = TRUE, 
      omit.coef = "as.factor", 
      include.ci = FALSE, 
      custom.gof.rows = list(
        `Country fixed effects` = c("Y", "Y", "Y"),
        `Year fixed effects` = c("N", "Y", "Y")
      )
    )
  
  lm_full_tex
  
}
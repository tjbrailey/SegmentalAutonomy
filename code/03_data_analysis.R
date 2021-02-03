### 03_data_analysis.R
###
### Goal: Analyze data and produce 
###
### Content: 
### 1. Cross-sectional time-series analyses
### 2. Multivariate OLS regressions
### 3. Cox Proportional Hazards models
### 4. Multivariate comparison across operationalizations
###

### 1. Cross-sectional time-series analyses

# Conflict 
csts("vdem_e_miinterc", "epr_reg_aut_dum")
csts("vdem_e_miinterc", "dpi_auton")
csts("vdem_e_miinterc", "rai_n_RAI")

# Support for democracy
csts("wvs_E117", "epr_reg_aut_dum")
csts("wvs_E117", "dpi_auton")
csts("wvs_E117", "rai_n_RAI")

# Polity scores
csts("vdem_e_polity2", "epr_reg_aut_dum")
csts("vdem_e_polity2", "dpi_auton")
csts("vdem_e_polity2", "rai_n_RAI")

### 2. Multivariate OLS regressions

# Support for democracy
psp_lm("vdem_e_miinterc", "dpi_auton")
psp_lm("vdem_e_miinterc", "epr_reg_aut_dum")
psp_lm("vdem_e_miinterc", "rai_n_RAI")

# Support for democracy
psp_lm("wvs_E117", "dpi_auton")
psp_lm("wvs_E117", "epr_reg_aut_dum")
psp_lm("wvs_E117", "rai_n_RAI")

# Support for democracy
psp_lm("vdem_e_polity2", "dpi_auton")
psp_lm("vdem_e_polity2", "epr_reg_aut_dum")
psp_lm("vdem_e_polity2", "rai_n_RAI")

### 3. Cox Proportional Hazards models

# With DPI measure
cox1 <- 
  survival::coxph(
    survival::Surv(year, vdem_e_miinterc) ~ (dpi_auton * tb_other_provis) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + vdem_e_polity2, 
    data = psp
  )

summary(cox1)
# reporttools::displayCoxPH(cox1)

# With EPR measure
cox2 <- 
  survival::coxph(
    survival::Surv(year, vdem_e_miinterc) ~ (epr_reg_aut_dum * tb_other_provis) + as.factor(country) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + vdem_e_polity2, 
    data = psp
  )

summary(cox2)
#reporttools::displayCoxPH(cox2)

# RAI
cox3 <- 
  survival::coxph(
    survival::Surv(year, vdem_e_miinterc) ~ (rai_n_RAI * tb_other_provis) + as.factor(country) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop + vdem_e_polity2,
    data = psp
  )

summary(cox3)
#reporttools::displayCoxPH(cox3)

### 4. Multivariate comparison across operationalizations

psp <- 
  psp %>%
  dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::filter(qog_fe_etfra > median(psp$qog_fe_etfra, na.rm = TRUE))

dpi_reg <- 
  estimatr::lm_robust(
    vdem_e_polity2 ~ (dpi_auton * tb_other_provis) + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop, 
    data = psp, 
    se_type = "CR2", 
    clusters = country
  )

epr_reg <- 
  estimatr::lm_robust(
    vdem_e_polity2 ~ (epr_reg_aut_dum * tb_other_provis) + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop, 
    data = psp, 
    se_type = "CR2", 
    clusters = country
  )

rai_reg <- 
  estimatr::lm_robust(
    vdem_e_polity2 ~ (rai_n_RAI * tb_other_provis) + as.factor(country) + as.factor(year) + qog_wbgi_pve + qog_wdi_gini + qog_gle_pop, 
    data = psp, 
    se_type = "CR2", 
    clusters = country
  )

tex_fin <- 
  texreg::texreg(
    list(dpi_reg, epr_reg, rai_reg), 
    mfrow = TRUE,
    omit.coef = "as.factor",
    include.ci = FALSE,
    custom.gof.rows = list(
      `Country fixed effects` = c("Y", "Y", "Y"),
      `Year fixed effects` = c("Y", "Y", "Y")
    )
  )
tex_fin

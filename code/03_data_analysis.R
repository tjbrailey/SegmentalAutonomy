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

### 1. Cross-sectional time-series plots

# Conflict 
csts("epr_incidence_terr_eth_flag", "epr_reg_aut_dum")

# Support for democracy
csts("hum_de_2", "epr_reg_aut_dum")

# Satisfaction with democracy 
csts("hum_de_1", "epr_reg_aut_dum")

# Social trust
csts("hum_at_0", "epr_reg_aut_dum")

# Polity scores
csts("vdem_e_polity2", "epr_reg_aut_dum")

### 2. Prepare data for LASSO Regressions

iv_names <-
  c("qog_bmr_demdur",
    "qog_wbgi_pve", 
    "qog_wdi_gini",
    "qog_gle_pop", 
    "qog_fe_etfra", 
    "qog_ccp_freerel",
    "qog_ccp_hr")

lasso_dat <- psp

for (var in iv_names) {
  print(var)
  print( table(is.na(lasso_dat[[var]])) )
}

# Drop if missing for any of the candidate vars
for (var in iv_names) {
  lasso_dat <- subset(lasso_dat, !is.na(lasso_dat[[var]]))
}

# Create formula from all predictor candidate vars
form_template <- formula(paste0("~ ", 
                          paste(iv_names, collapse=" + ")))
form_template

# Store as matrix
x <- model.matrix(form_template, data = lasso_dat)


### 3. Run LASSO regressions 

# Conflict
var <- "epr_incidence_terr_eth_flag"
conf_lm <- lm_with_lasso_vars(var)
conf_binom <- binom_with_lasso_vars(var)
conf_cox <- cox_with_lasso_vars(var)

# Democracy
var <- "qog_bmr_dem"
dem_lm <- lm_with_lasso_vars(var)
dem_binom <- binom_with_lasso_vars(var)
dem_cox <- cox_with_lasso_vars(var)

# Satisfaction with democracy
var <- "hum_de_1"
satdem_lm <- lm_with_lasso_vars(var)

# Support for democracy
var <- "hum_de_2"
supdem_form <- formula(paste0(var, " ~ (", "epr_reg_aut_dum", "* tb_other_provis) + as.factor(country) + as.factor(year) + ", paste0(iv_names, collapse=" + ")))
supdem_lm <-
  estimatr::lm_robust(
    supdem_form,
    data = psp, 
    se_type = "CR2", 
    clusters = country
  )

# Social trust
var <- "hum_at_0"
trust_lm <- lm_with_lasso_vars(var)

# Alternative measure of democracy (free and fair elections)
var <- "vdem_v2elfrfair"

free_fair_elections_lm <- lm_with_lasso_vars(var)

ffe_tex <- 
  texreg::texreg(
    free_fair_elections_lm,
    omit.coef = "as.factor", 
    include.ci = FALSE,
    custom.coef.map = list(
      "(Intercept)" = "(Intercept)",
      "epr_reg_aut_dum" = "Regional Autonomy", 
      "tb_other_provis" = "Other Provisions", 
      "qog_bmr_demdur" = "Years of Democracy", 
      "qog_wbgi_pve" = "Political Stability", 
      "qog_wdi_gini" = "Gini Index", 
      "qog_gle_pop" = "Population", 
      "qog_fe_etfra" = "Ethnic Fractionalization",
      "qog_ccp_freerel" = "Freedom of Religion",
      "qog_ccp_hr" = "HRC in Constitution",
      "epr_reg_aut_dum:tb_other_provis" = "Regional Autonomy:Other Provisions"),
    float.pos = "!htbp",
    custom.gof.rows = list(
      `Country fixed effects` = c("Y"),
      `Year fixed effects` = c("Y")
    ), 
    caption = "Alternative Measure of Democracy (Free and Fair Elections)",  
    file = paste0(here::here(), "/paper/ffe_lm.tex")
  )
ffe_tex

# Convert to .tex
lm_full_tex <- 
  texreg::texreg(
    list(trust_lm, conf_binom[[1]], dem_binom[[1]], satdem_lm, supdem_lm), 
    mfrow = TRUE, 
    omit.coef = "as.factor", 
    include.ci = FALSE, 
    custom.model.names = c("(OLS)", "(Logit)", "(Logit)", "(OLS)", "(OLS)"),
    custom.header = list("Social Trust" = 1, "Conflict" = 2, "Democracy Score" = 3, "Satisfaction with Democracy" = 4, "Support for Democracy" = 5), 
    custom.coef.map = list(
      "(Intercept)" = "(Intercept)",
      "epr_reg_aut_dum" = "Regional Autonomy", 
      "tb_other_provis" = "Other Provisions", 
      "qog_bmr_demdur" = "Years of Democracy", 
      "qog_wbgi_pve" = "Political Stability", 
      "qog_wdi_gini" = "Gini Index", 
      "qog_gle_pop" = "Population", 
      "qog_fe_etfra" = "Ethnic Fractionalization",
      "qog_ccp_freerel" = "Freedom of Religion",
      "qog_ccp_hr" = "HRC in Constitution",
      "epr_reg_aut_dum:tb_other_provis" = "Regional Autonomy:Other Provisions"),
    override.pvalues = list(NA, conf_binom[[2]][,4], dem_binom[[2]][,4], NA, NA),
    override.se = list(NA,conf_binom[[2]][,2], dem_binom[[2]][,2], NA, NA),
    float.pos = "!htbp",
    custom.gof.rows = list(
      `Country fixed effects` = c("Y", "Y", "Y", "Y", "Y"),
      `Year fixed effects` = c("Y", "Y", "Y", "Y", "Y")
    ), 
    caption = "Impacts of segmental autonomy", 
    file = paste0(here::here(), "/paper/lm_full.tex")
  )

lm_full_tex

lm_full_tex_primary_outcomes_only <- 
  texreg::texreg(
    list(trust_lm, conf_binom[[1]], dem_binom[[1]]), 
    mfrow = TRUE, 
    omit.coef = "as.factor", 
    include.ci = FALSE, 
    custom.model.names = c("(OLS)", "(Logit)", "(Logit)"),
    custom.header = list("Social Trust" = 1, "Conflict" = 2, "Democracy Score" = 3), 
    custom.coef.map = list(
      "(Intercept)" = "(Intercept)",
      "epr_reg_aut_dum" = "Regional Autonomy", 
      "tb_other_provis" = "Other Provisions", 
      #"qog_bmr_demdur" = "Years of Democracy", 
      #"qog_wbgi_pve" = "Political Stability", 
      #"qog_wdi_gini" = "Gini Index", 
      #"qog_gle_pop" = "Population", 
      #"qog_fe_etfra" = "Ethnic Fractionalization",
      #"qog_ccp_freerel" = "Freedom of Religion",
      #"qog_ccp_hr" = "HRC in Constitution",
      "epr_reg_aut_dum:tb_other_provis" = "Regional Autonomy:Other Provisions"),
    override.pvalues = list(NA, conf_binom[[2]][,4], dem_binom[[2]][,4]),
    override.se = list(NA,conf_binom[[2]][,2], dem_binom[[2]][,2]),
    float.pos = "!htbp",
    custom.gof.rows = list(
      `Country fixed effects` = c("Y", "Y", "Y"),
      `Year fixed effects` = c("Y", "Y", "Y")
    ), 
    caption = "Impacts of segmental autonomy", 
    file = paste0(here::here(), "/paper/lm_full_primary_outcomes_only.tex")
  )

cox_rownames = c(
  "Regional Autonomy", 
  "Other Provisions", 
  "Years of Democracy", 
  "Political Stability", 
  "Gini Index", 
  "Population", 
  "Freedom of Religion",
  "HRC in Constitution",
  "Regional Autonomy:Other Provisions")

prepare_cox_for_latex(conf_cox[[1]], "conf_cox_lag1", "Cox regression model: conflict (1 year lag)")
prepare_cox_for_latex(conf_cox[[2]], "conf_cox_lag5", "Cox regression model: conflict (5 year lag)")
prepare_cox_for_latex(conf_cox[[3]], "conf_cox_lag10", "Cox regression model: conflict (10 year lag)")


prepare_cox_for_latex(dem_cox[[1]], "dem_cox_lag1", "Cox regression model: democracy (1 year lag)")
prepare_cox_for_latex(dem_cox[[2]], "dem_cox_lag5", "Cox regression model: democracy (5 year lag)")
prepare_cox_for_latex(dem_cox[[3]], "dem_cox_lag10", "Cox regression model: democracy (10 year lag)")

### Additional stuff

### Summary stats
cols <- c("epr_reg_aut_dum",
          "epr_incidence_terr_eth_flag",
          "hum_at_0",
          "hum_de_1",
          "hum_de_2",
          "vdem_e_polity2",
          "qog_vdem_partipdem")

res <- pastecs::stat.desc(psp[cols])
xtable::print.xtable(
  xtable::xtable(
    round(res, 2), 
    caption = "Tabulated Summary of Main Variables"
    ),table.placement = "!htbp",
  file = paste0(here::here(), "/paper/sum_stats.tex")
)

# Comparison of DV
var <- "qog_bmr_dem" # Democracy trust

epr_dem_binom <- binom_with_lasso_vars(var, "epr_reg_aut_dum")
dpi_dem_binom <- binom_with_lasso_vars(var, "dpi_auton")
rai_dem_binom <- binom_with_lasso_vars(var, "rai_n_RAI")

dv_comparison_tex <- 
  texreg::texreg(
    list(epr_dem_binom[[1]], dpi_dem_binom[[1]], rai_dem_binom[[1]]), 
    mfrow = TRUE, 
    omit.coef = "as.factor", 
    include.ci = FALSE, 
    custom.model.names = c("EPR", "DPI", "RAI"),
    custom.coef.map = list(
      "(Intercept)" = "(Intercept)",
      "epr_reg_aut_dum" = "EPR", 
      "tb_other_provis" = "Other Provisions", 
      "qog_bmr_demdur" = "Years of Democracy", 
      "qog_wbgi_pve" = "Political Stability", 
      "qog_wdi_gini" = "Gini Index", 
      "qog_gle_pop" = "Population", 
      "qog_fe_etfra" = "Ethnic Fractionalization",
      "qog_ccp_freerel" = "Freedom of Religion",
      "qog_ccp_hr" = "HRC in Constitution",
      "epr_reg_aut_dum:tb_other_provis" = "EPR:Other Provisions",
      "dpi_auton" = "DPI",
      "dpi_auton:tb_other_provis" = "DPI:Other Provisions",
      "rai_n_RAI" = "RAI",
      "rai_n_RAI:tb_other_provis" = "RAI:Other Provisions"),
    float.pos = "!htbp",
    caption = "Comparison of measurements of autonomy", 
    override.pvalues = list(epr_dem_binom[[2]][,4], dpi_dem_binom[[2]][,4], rai_dem_binom[[2]][,4]),
    override.se = list(epr_dem_binom[[2]][,2], dpi_dem_binom[[2]][,2], rai_dem_binom[[2]][,2]),
    custom.gof.rows = list(
      `Country fixed effects` = c("Y", "Y", "Y"),
      `Year fixed effects` = c("Y", "Y", "Y")
    ), 
    file = paste0(here::here(), "/paper/dv_comparison_full.tex")
  )

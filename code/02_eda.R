## 02_eda.R
###
### Goal: Explore variables
###
### Content: 
### 1. Explore consocational variables
### 2. Explore segmental autonomy
###   2.1. Explore segmental autonomy implementation
###   2.2. Visualize segmental autonomy implementation
###   2.3. Explore sub-facets of segmental autonomy
### 3. Create correlation matrix of regional autonomy variables
### 4. Comparative regressions
### 5. Additional calculations
###

### 1. Explore consocational variables

# States that employ mutual veto provisions
psp %>% 
  dplyr::filter(idc_mveto == 1) %>%
  dplyr::distinct(country)

# States that employ grand coalition provisions
psp %>%
  dplyr::filter(idc_gcman == 1 & idc_gcimp == 1) %>%
  dplyr::distinct(country)

# States that employ proportionality
psp %>%
  dplyr::filter(dpi_pr == 1) %>%
  dplyr::distinct(country)

# States that employ segmental autonomy
psp %>%
  dplyr::filter(epr_reg_aut_cont > 0) %>% 
  dplyr::distinct(country)

psp %>%
  dplyr::filter(epr_reg_aut_dum > 0) %>% 
  dplyr::distinct(country)

psp %>%
  dplyr::filter(rai_n_RAI > 0) %>% 
  dplyr::distinct(country)

### 2. Explore segmental autonomy

# epr_reg_aut_dum missingness/summary
auton_sum <- 
  psp %>%
  dplyr::select(country, year, epr_reg_aut_dum) %>%
  dplyr::group_by(epr_reg_aut_dum) %>%
  dplyr::summarize(summary = dplyr::n())

# States that have employed segmental autonomy provisions
seg_states <- 
  psp %>%
  dplyr::filter(epr_reg_aut_dum == 1) %>% 
  dplyr::distinct(country) %>%
  dplyr::pull(country)

seg_states_dat <-
  psp %>% 
  dplyr::filter(country %in% c(seg_states))

# States that have employed segmental autonomy provisions
seg_imp <- 
  seg_states_dat %>%
  dplyr::filter(epr_reg_aut_dum == 0) %>%
  dplyr::distinct(country) %>%
  dplyr::pull()

seg_imp_dat <- 
  psp %>%
  dplyr::filter(country %in% c(seg_imp))

# 2.1. Explore segmental autonomy implementation

# States that exhibit change in the auton variable
aut_t <- 
  psp %>%
  dplyr::filter(epr_reg_aut_dum == 1) %>%
  dplyr::distinct(country) %>%
  dplyr::pull()

# States that do not exhibit change in the auton variable
aut_f <- 
  psp %>%
  dplyr::filter(epr_reg_aut_dum == 0) %>%
  dplyr::distinct(country) %>%
  dplyr::pull()

aut_t <- dplyr::as_tibble(aut_t)
aut_f <- dplyr::as_tibble(aut_f)

country_aut <- 
  aut_t %>%
  dplyr::filter(value %in% unique(aut_f$value))

psp_aut_variation <- 
  psp %>%
  dplyr::filter(country %in% c(country_aut$value))
unique(psp_aut_variation$country)

# By states that have never had autonomy
psp_no_aut <- 
  psp %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(no_aut = sum(epr_reg_aut_dum, na.rm = TRUE)) %>%
  dplyr::filter(sum(no_aut) != 0)

# By auton and ethnic fractionalization
mean(psp$qog_al_ethnic2000, na.rm = TRUE)
median(psp$qog_al_ethnic2000, na.rm = TRUE)
quantile(psp$qog_al_ethnic2000, .67, na.rm = TRUE)

psp_no_aut_frac <- 
  psp_no_aut %>%
  dplyr::filter(qog_al_ethnic2000 > median(qog_al_ethnic2000, na.rm = TRUE))

# 2.2. Visualize segmental autonomy implementation

gp1 <- 
  ggplot(seg_imp_dat, aes(x = year, y = epr_reg_aut_dum)) +
  geom_line(size = 2) +
  facet_wrap(~ country) + 
  labs(x = "Year",
       y = "Segmental Autonomy 
       (1 = Provision Implemented, 0 = Provision Not Present)",
       title = "Implementation of Segmental Autonomy Provisions") + 
  theme_bw()
gp1

ggsave(gp1, filename = paste0(here::here(), "/paper/seg_auton_implement.png"), units = "in", width = 11, height = 7)

# 2.3. Explore sub-facets of segmental autonomy

# Subnational authority
author <- 
  psp %>%
  dplyr::filter(dpi_author == 1) %>%
  dplyr::distinct(country, year)
unique(author$country)

# Subnational education authority
subed <- 
  psp %>%
  dplyr::filter(idc_subed == 1) %>%
  dplyr::distinct(country, year)
unique(subed$country)

# Subnational tax authority
subtax <- 
  psp %>%
  dplyr::filter(idc_subtax == 1) %>%
  dplyr::distinct(country, year)
unique(subtax$country)

# Subnational police authority
subpolice <-
  psp %>%
  dplyr::filter(idc_subpolice == 1) %>%
  dplyr::distinct(country, year)
unique(subpolice$country)

# Federal units
fedunits <- 
  psp %>%
  dplyr::filter(idc_fedunits == 1) %>%
  dplyr::distinct(country, year)
unique(fedunits$country)

### 3. Create correlation matrix of regional autonomy variables

psp_sub <- 
  psp %>%
  dplyr::select(
    cown, cowc, year, dpi_auton, dpi_author,
    idc_subed, idc_subtax, idc_subpolice,
    vdem_e_polity2
  )

# Join datasets
join1 <- dplyr::left_join(psp_sub, rai_psp_sub)
join2 <- dplyr::left_join(join1, epr_psp_sub)

reg_data <- 
  join2 %>%
  dplyr::mutate(
    dpi_auton = as.logical(dpi_auton),
    dpi_author = as.logical(dpi_author),
    epr_reg_aut_dum = as.logical(epr_reg_aut_dum)
  )

reg_data_complete <- 
  reg_data[complete.cases(reg_data), ] %>%
  dplyr::select(
    cown, year, 
    dpi_auton, rai_n_RAI, epr_reg_aut_dum,
    idc_subtax, idc_subed, idc_subpolice, 
    dpi_author, vdem_e_polity2
  )

# Plot correlation tables
auton_cor_pear <- 
  xtable::xtable(
    round(
      cor(
        reg_data_complete[, 3:9]
      ), 2)
  )

upper <- auton_cor_pear
upper[upper.tri(auton_cor_pear)] <- ""
upper <- as.data.frame(upper)
upper <- xtable::xtable(upper)
upper

auton_cor_spear <- 
  xtable::xtable(
    round(
      cor(
        reg_data_complete[, 3:9],
        method = "spearman"
      ), 2)
  )

auton_cor_spear

auton_cor_ken <- 
  xtable::xtable(
    round(
      cor(
        reg_data_complete[, 3:9],
        method = "kendall"
      ), 2)
  )

auton_cor_ken

### 4. Comparative regressions

reg1 <- lm(vdem_e_polity2 ~ dpi_auton, data = reg_data_complete)
reg1_tex <- texreg::texreg(reg1, include.ci = FALSE)
reg1_tex

plot(x = reg_data_complete$dpi_auton, y = reg_data_complete$vdem_e_polity2)
abline(reg = reg1)

reg2 <- lm(vdem_e_polity2 ~ idc_subtax, data = reg_data_complete)
reg2_tex <- texreg::texreg(reg2, include.ci = FALSE)
reg2_tex

plot(x = reg_data_complete$idc_subtax, y = reg_data_complete$vdem_e_polity2)
abline(reg = reg2)

reg3 <- lm(vdem_e_polity2 ~ idc_subed, data = reg_data_complete)
reg3_tex <- texreg::texreg(reg3, include.ci = FALSE)
reg3_tex

plot(x = reg_data_complete$idc_subed, y = reg_data_complete$vdem_e_polity2)
abline(reg = reg3)

reg4 <- lm(vdem_e_polity2 ~ idc_subpolice, data = reg_data_complete)
reg4_tex <- texreg::texreg(reg4, include.ci = FALSE)
reg4_tex

plot(x = reg_data_complete$idc_subpolice, y = reg_data_complete$vdem_e_polity2)
abline(reg = reg4)

reg5 <- lm(vdem_e_polity2 ~ rai_n_RAI, data = reg_data_complete)
reg5_tex <- texreg::texreg(reg5, include.ci = FALSE)
reg5_tex

jpeg(filename = paste0(here::here(), "/paper/polity_score_rai_reg.jpeg"))
plot(x = reg_data_complete$rai_n_RAI, 
     y = reg_data_complete$vdem_e_polity2,
     xlab = "RAI Measure of Autonomy", 
     ylab = "Polity Score", 
     main = "Predicting Polity Score with Regional Autonomy Provisions")
abline(reg = reg5, col = "red")
dev.off()

reg6 <- lm(vdem_e_polity2 ~ epr_reg_aut_dum, data = reg_data_complete)
reg6_tex <- texreg::texreg(reg6, include.ci = FALSE)
reg6_tex

plot(x = reg_data_complete$epr_reg_aut_dum, y = reg_data_complete$vdem_e_polity2)
abline(reg = reg6)

reg_full_tex <- 
  texreg::texreg(
    list(reg1, reg5, reg6),
    mfrow = TRUE,
    omit.coef = "as.factor", 
    include.ci = FALSE
  ) 

reg_full_tex

### 5. Additional calculations

# Percentage of ethnically fractionalized states
psp_eth <- 
  psp %>%
  dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::filter(qog_fe_etfra > median(psp$qog_fe_etfra, na.rm = TRUE))

pct <- length(unique(psp_eth$country)) / length(unique(psp$country)) * 100
pct

# Summary table
vars <- psp[, c("vdem_e_polity2", "vdem_e_miinterc", "dpi_auton", "qog_fe_etfra")]
cap <- "Summary Statistics"
reporttools::tableContinuous(vars = vars, cap = cap, longtable = FALSE)

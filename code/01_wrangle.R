### 01_wrangle.R
###
### Goal: Join and clean datasets so that 
### they are ready for analysis. 
###
### Content: 
### 1. Clean datasets
###   1.1. IDC
###   1.2. VDEM
###   1.3. QoG
###   1.4. DPI
###   1.5. RAI
###   1.6. EPR
###   1.7. WVS
### 2. Join datasets
### 3. Post-merge cleaning
### 4. Save data
###

### 1. Clean datasets

# 1.1. IDC

idc_psp_sub <- 
  idc %>% 
  dplyr::mutate(
    
    # Rename unmatched countries
    country = dplyr::case_when(
      country == "Cent. Af. Rep." ~ "Central African Republic",
      country == "Dom. Rep." ~ "Dominican Republic",
      country == "GDR" ~ "German Democratic Republic",
      country == "PRC" ~ "China",
      country == "ROK" ~ "South Korea",
      country == "S. Africa" ~ "South Africa",
      country == "Serbia and Montenegro" ~ "Montenegro",
      TRUE ~ as.character(country)
    )
  )

# Identify edge cases
unmatched <- 
  idc_psp_sub %>% 
  dplyr::filter(
    is.na(country) & is.na(ifs)
  )

# Drop edge cases
idc_psp_sub <- 
  idc_psp_sub %>% 
  dplyr::filter(
    !(is.na(country) & is.na(ifs))
  ) %>% 
  dplyr::mutate(
    cowc = countrycode::countrycode(country, "country.name", "cowc"),
    cown = countrycode::countrycode(country, "country.name", "cown")
  ) %>%
  
  # Select key variables
  dplyr::select(
    cowc, cown, year, 
    mveto, gcman, gcimp, 
    jrevman, relconstd, relconstp,
    milleg, partynoethnic, jtenure, 
    jconst, gcseats1, gcseats2, 
    gcseats3, unity, resman, 
    resseats, resseats2, resseatsimp,
    miman, subtax, subed, subpolice, 
    fedunits, state, muni
  ) %>% 
  
  dplyr::rename_at(
    dplyr::vars(!dplyr::matches("country|cowc|cown|year")), 
    ~paste0("idc_", .)
  )

# 1.2. VDEM

# Select key variables
vdem_psp_sub <- 
  vdem %>%
  dplyr::select(
    country_name, year, e_miinterc, e_civil_war, v2elfrfair, e_polity2
  ) %>% 
  dplyr::rename(country = country_name) %>% 
  
  # Recode country names
  dplyr::mutate(
    country = countrycode::countrycode(country, "country.name", "country.name"), 
    cown = countrycode::countrycode(country, "country.name", "cown"), 
    cowc = countrycode::countrycode(country, "country.name", "cowc")
  ) %>% 
  dplyr::select(-country) %>%
  dplyr::rename_at(
    dplyr::vars(!dplyr::matches("country|cowc|cown|year")), 
    ~paste0("vdem_", .)
  )

# 1.3. QoG

# Select key variables and clean.
qog_ts_psp_sub <- 
  qog_ts %>%
  dplyr::select(
    cname, year, fe_etfra, iaep_ebbp, gle_gdp,
    bti_ci, cspf_sfi, gtm_unit, ccp_hr,
    ffp_hr, jw_bicameral,
    bti_ig, vdem_partipdem, iaep_nr, bti_sop,
    bti_ffe, gol_est, gol_mt, iaep_es,
    no_ef, no_ce, iaep_eccdt, iaep_ecdl,
    iaep_eml, iaep_epmf, iaep_evp, iaep_lcre,
    iaep_lego, iaep_lrit, wbgi_pve, wdi_gini, gle_pop,
    fe_etfra, al_ethnic2000, pt_federal, wel_trstd
  ) %>%
  dplyr::rename(country = cname) %>% 
  
  # Recode country names
  dplyr::mutate(
    country = dplyr::case_when(
      country == "Micronesia" ~ "Federated States of Micronesia", 
      country == "Serbia and Montenegro" ~ "Montenegro",
      TRUE ~ as.character(country)
    ),
    country = countrycode::countrycode(country, "country.name", "country.name"),
    cown = countrycode::countrycode(country, "country.name", "cown"),
    cowc = countrycode::countrycode(country, "country.name", "cowc")
  )  %>% 
  dplyr::select(-country) %>% 
  dplyr::rename_at(
    dplyr::vars(!dplyr::matches("country|cowc|cown|year")),
    ~paste0("qog_", .)
  )

# 1.4. DPI

# Select key variables
dpi_psp_sub <- 
  dpi %>%
  dplyr::select(
    countryname, year, system, auton, author, pr, sensys, eiec, housesys
  ) %>%
  dplyr::rename(country = countryname) %>%
  dplyr::mutate(
    year = as.numeric(year),
    country = dplyr::case_when(
      country == "Cent. Af. Rep." ~ "Central African Republic",
      country == "Dom. Rep." ~ "Dominican Republic",
      country == "GDR" ~ "German Democratic Republic",
      country == "PRC" ~ "China",
      country == "ROK" ~ "South Korea",
      country == "S. Africa" ~ "South Africa",
      TRUE ~ as.character(country)
    ),
    country = countrycode::countrycode(country, "country.name", "country.name"), 
    cowc = countrycode::countrycode(country, "country.name", "cowc"),
    cown = countrycode::countrycode(country, "country.name", "cown")
  ) %>% 
  dplyr::select(-country) %>% 
  dplyr::rename_at(
    dplyr::vars(!dplyr::matches("country|cowc|cown|year")), 
    ~paste0("dpi_", .)
  )

# 1.5. RAI

rai_psp_sub <- 
  rai %>% 
  dplyr::select(country_name, year, n_RAI) %>%
  dplyr::mutate(
    cown = countrycode::countrycode(country_name, "country.name", "cown"),
    cowc = countrycode::countrycode(cown, "cown", "cowc"),
    year = as.numeric(year)
  ) %>%
  dplyr::select(cown, cowc, year, n_RAI) %>% 
  dplyr::rename_at(
    dplyr::vars(!dplyr::matches("country|cowc|cown|year")),
    ~paste0("rai_", .)
  )

# 1.6. EPR

epr_psp_sub <- 
  epr %>%
  dplyr::mutate(cown = countrycode::countrycode(gwid, "gwn", "cown")) %>%
  dplyr::select(cown, from, group, reg_aut) %>%
  dplyr::rename(year = from) %>% 
  dplyr::group_by(cown, group) %>%
  tidyr::complete(
    cown, group,
    year = 1946:2017,
    fill = list(incidents = 0)
  ) %>% 
  tidyr::pivot_wider(
    names_from = group,
    values_from = reg_aut
  ) %>%
  dplyr::group_by(cown) %>%
  tidyr::fill(dplyr::everything()) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    reg_aut_cont = rowSums(.[,3:length(.)] == TRUE, na.rm = TRUE)
  ) %>%  
  dplyr::mutate(
    reg_aut_dum = ifelse(reg_aut_cont >= 1, 1, 0),
    cowc = countrycode::countrycode(cown, "cown", "cowc")) %>%
  dplyr::select(cown, cowc, year, reg_aut_dum, reg_aut_cont) %>% 
  dplyr::rename_at(
    dplyr::vars(!dplyr::matches("country|cowc|cown|year")), 
    ~paste0("epr_", .)
  )

# 1.7. WVS

wvs_psp_sub <- 
  wvs %>% 
  dplyr::select(
    S003, S020,
    E069_07, E069_11, E069_12,
    E111, E117
  ) %>%
  dplyr::rename(year = S020) %>%
  dplyr::mutate(
    cown = countrycode::countrycode(S003, "iso3n", "cown"),
    cowc = countrycode::countrycode(cown, "cown", "cowc"),
    year = as.numeric(year)
  ) %>%
  dplyr::select(-S003) %>% 
  dplyr::rename_at(
    dplyr::vars(!dplyr::matches("cowc|cown|year")),
    ~paste0("wvs_", .)
  ) %>% 
  dplyr::group_by(year, cowc, cown) %>%
  dplyr::summarise_all(mean, na.rm=TRUE) %>%
  dplyr::group_by(cowc, cown) %>%
  tidyr::complete(
    year = 1946:2017
  ) %>% 
  tidyr::fill(dplyr::everything())

### 2. Join datasets

# Join data one-by-one. Check for discrepancies
tb_2 <- dplyr::left_join(wvs_psp_sub, qog_ts_psp_sub)
tb_3 <- dplyr::left_join(tb_2, idc_psp_sub)
tb_4 <- dplyr::left_join(tb_3, epr_psp_sub)
tb_5 <- dplyr::left_join(tb_4, rai_psp_sub)
tb_6 <- dplyr::left_join(tb_5, dpi_psp_sub)
tb_7 <- dplyr::left_join(tb_6, vdem_psp_sub)

# Check duplicates
test <- which(duplicated(tb_7[c("cowc", "year")]) == TRUE)

# Collapse duplicate country/years while retaining values
tb_8 <- tb_7 %>% 
  dplyr::group_by(cown, year) %>%
  dplyr::summarise_all(list(~dplyr::first(na.omit(.))))

### 3. Post-merge cleaning

# Remove codings for NA values
psp_na_recode <- tb_8

psp_na_recode[psp_na_recode == "-999" | psp_na_recode == -999] <- NA
psp_na_recode[psp_na_recode == "-44" | psp_na_recode == -44] <- NA
psp_na_recode[psp_na_recode == "-66" | psp_na_recode == -66] <- NA
psp_na_recode[psp_na_recode == "-77" | psp_na_recode == -77] <- NA
psp_na_recode[psp_na_recode == "-88" | psp_na_recode == -88] <- NA
psp_na_recode$qog_ccp_hr[psp_na_recode$qog_ccp_hr == 96] <- 3

psp_na_recode[psp_na_recode == ".a"] <- NA
psp_na_recode[psp_na_recode == ".b"] <- NA
psp_na_recode[psp_na_recode == ".e"] <- NA

# Rename variables
psp_rename <- 
  psp_na_recode %>%
  # Fill out remaining variables
  dplyr::group_by(cown) %>% 
  tidyr::fill(
    dplyr::everything()
  ) %>%
  
  dplyr::rowwise() %>%
  # Other provisions
  dplyr::mutate(
    tb_other_provis = sum(c(idc_mveto, idc_gcman, idc_gcimp, dpi_pr), na.rm = TRUE)
  ) %>%
  
  # Length of conflict
  dplyr::group_by(cown) %>% 
  dplyr::mutate(
    tb_conflict_length = cumsum(
      vdem_e_miinterc
    )
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(year),
                !is.na(cowc),
                year >= 1980,
                year != 2020)

psp_rename$country <- countrycode::countrycode(psp_rename$cown, "cown", "country.name")

# Visualize after recoding
DataExplorer::plot_missing(psp_rename)

DataExplorer::plot_histogram(psp_rename)

Amelia::missmap(psp_rename)

png(file = paste0(here::here(), "/paper/tjbrailey_psp_clean_missingness.png"),
    width = 2000, 
    height = 1000)
Amelia::missmap(psp_rename)
dev.off()

### 4. Save data

# Save as a .csv file 
write.csv(psp_rename, file = paste0(here::here(), "/data/tjbrailey_psp_clean.csv"))

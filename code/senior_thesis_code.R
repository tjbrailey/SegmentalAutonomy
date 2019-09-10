  ### Senior Thesis Code ### 
    ### Thomas Brailey ###
### POLI 191A // POLI 191B ###


rm(list = ls())


### Install data

# Set working directory
setwd(paste0(here::here(), '/data/'))
wd <- paste0(here::here(), '/data/')
getwd()

# Load for piping function
library(magrittr)
library(ggplot2)
library(ggmap)

# Parse out sheets from PSED excel workbook
files <- list.files(wd, 'PSED_agreement.xlsx')
files <- files[]

read_excel_allsheets <- function(filename) { 
  sheets <- readxl::excel_sheets(filename) 
  x <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x)) 
  names(x) <- sheets 
  x 
} 

out <- lapply(files, read_excel_allsheets)
basename(files)

# Install datasets
 # Power-sharing-specific
psed_prom <- out[[1]]$PSED_agreement_promises
psed_prac <- out[[1]]$PSED_agreement_practices
idc <- rio::import('IDC_country-year_v1_0.RData')
impact <- rio::import('c_656154-l_1-k_impact--version2.0.csv') # IMPACT data previously in .xls. Updated to .csv.
dtd <- rio::import('Democracy Timeseries Data January 2009 Excel2007.csv')
epr <- rio::import('data-epr_countryyear.csv')
cah_pshare_and_dem <- rio::import('pshare_and_democracy_for_world_politics_publication.dta')
cah_craft_peace <- rio::import('book_project_49_cases_long_time_version_feb_2004.dta')
bumba <- rio::import('Bumba(peaceduration).RData')
pax <- rio::import('pax_20_02_2018_1_CSV.csv')
di <- rio::import('Diplomatic Interventions data.dta')
dme <- rio::import('DME data.dta')

  # General (the Y)
dpi <- rio::import('DPI2012.xls')
qog_cs <- rio::import('qog_std_cs_jan19.csv')
qog_ts <- rio::import('qog_std_ts_jan19.csv')
polityiv <- rio::import('p4v2017.xls')
ucdp <- rio::import('ucdp-prio-acd-191.xlsx')

# Clean workspace
rm(out, files, read_excel_allsheets)


### Join data

# Set baseline for new data
tb <- idc

tb$country[tb$country == "Cent. Af. Rep."] <- "Central African Republic"
tb$country[tb$country == "Dom. Rep."] <- "Dominican Republic"
tb$country[tb$country == "GDR"] <- "German Democratic Republic"
tb$country[tb$country == "PRC"] <- "China"
tb$country[tb$country == "ROK"] <- "South Korea"
tb$country[tb$country == "S. Africa"] <- "South Africa"
tb$country[tb$country == "Serbia and Montenegro"] <- "Montenegro"

tb <- tb[!is.na(tb$country) & !is.na(tb$ifs),]

tb$cowc <- countrycode::countrycode(tb$country, 'country.name', 'cowc')
tb$cown <- countrycode::countrycode(tb$country, 'country.name', 'cown')

tb <- tb %>%
  dplyr::select(country, 
                cowc, 
                cown, 
                year, 
                gwno, 
                ifs, 
                mveto,
                gcman,
                gcimp,
                auton,
                jrevman,
                relconstd,
                relconstp,
                milleg,
                partynoethnic,
                jtenure,
                jconst,
                gcseats1,
                gcseats2,
                gcseats3,
                unity,
                resman,
                resseats,
                resseats2,
                resseatsimp,
                miman,
                subtax,
                subed,
                subpolice,
                state
                )

# Prepare other data
dtd_psp_sub <- dtd %>%
  dplyr::select(Nation,
                CCode,
                Year,
                Natcode,
                NatWVS,
                Natmap,
                Natabrv,
                auton,
                Coalition,
                coalition2,
                coalitiongov,
                VANstand,
                Admin,
                mixed,
                proportional,
                prDPI,
                roseprop,
                xrcomp
                ) %>%
  dplyr::rename(country = Nation,
                year = Year)
dtd_psp_sub$country <- countrycode::countrycode(dtd_psp_sub$country, 'country.name', 'country.name')
dtd_psp_sub$cowc <- countrycode::countrycode(dtd_psp_sub$country, 'country.name', 'cowc')
dtd_psp_sub$cown <- countrycode::countrycode(dtd_psp_sub$country, 'country.name', 'cown')  

pax_psp_sub <- pax %>%
  dplyr::select(Con,
                Dat,
                Agt,
                PpsAut,
                TpsAut,
                PpsEx,
                CenBan,
                PpsOro,
                PpsOthPr,
                StRef
                ) %>%
  dplyr::rename(country = Con,
                year = Dat) %>%
  dplyr::mutate(year = as.numeric(stringr::str_sub(year, start= -4))) %>%
  dplyr::group_by(PpsAut,
                  TpsAut,
                  PpsEx,
                  CenBan,
                  PpsOro,
                  PpsOthPr,
                  StRef, 
                  country, year) %>%
  dplyr::summarise(Agt = paste(Agt, collapse=", ")) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(country = sub("\\/.*", "", country)) %>%
  dplyr::select(-Agt)
pax_psp_sub$country <- countrycode::countrycode(pax_psp_sub$country, 'country.name', 'country.name')
pax_psp_sub$cown <- countrycode::countrycode(pax_psp_sub$country, 'country.name', 'cown')
pax_psp_sub$cowc <- countrycode::countrycode(pax_psp_sub$country, 'country.name', 'cowc')

qog_ts_psp_sub <- qog_ts %>%
  dplyr::select(ccode, 
                cname, 
                year, 
                ccodealp,
                fe_etfra,
                iaep_ebbp,
                gtm_unit,
                ccp_hr,
                ffp_hr,
                iiag_phr,
                dpi_housesys,
                dpi_sensys,
                jw_bicameral,
                bti_ig,
                vdem_partipdem,
                iaep_nr,
                bti_sop,
                gol_est,
                gol_mt,
                iaep_es,
                no_ef,
                no_ce,
                iaep_eccdt,
                iaep_ecdl,
                iaep_eml,
                iaep_epmf,
                iaep_evp,
                iaep_lcre,
                iaep_lego,
                iaep_lrit,
                wbgi_pve
  ) %>%
  dplyr::rename(country = cname)
qog_ts_psp_sub$country[qog_ts_psp_sub$country == "Micronesia"] <- "Federated States of Micronesia"
qog_ts_psp_sub$country[qog_ts_psp_sub$country == "Serbia and Montenegro"] <- "Montenegro"
qog_ts_psp_sub$country <- countrycode::countrycode(qog_ts_psp_sub$country, 'country.name', 'country.name')
qog_ts_psp_sub$cown <- countrycode::countrycode(qog_ts_psp_sub$country, 'country.name', 'cown')
qog_ts_psp_sub$cowc <- countrycode::countrycode(qog_ts_psp_sub$country, 'country.name', 'cowc')

dpi_psp_sub <- dpi %>%
  dplyr::select(countryname, 
                ifs, 
                year,
                system,
                auton,
                pr,
                sensys,
                eiec
                ) %>%
  dplyr::rename(country = countryname) %>%
  dplyr::mutate(year = as.numeric(year))
dpi_psp_sub$country[dpi_psp_sub$country == "Cent. Af. Rep."] <- "Central African Republic"
dpi_psp_sub$country[dpi_psp_sub$country == "Dom. Rep."] <- "Dominican Republic"
dpi_psp_sub$country[dpi_psp_sub$country == "GDR"] <- "German Democratic Republic"
dpi_psp_sub$country[dpi_psp_sub$country == "PRC"] <- "China"
dpi_psp_sub$country[dpi_psp_sub$country == "PRK"] <- "North Korea"
dpi_psp_sub$country[dpi_psp_sub$country == "ROK"] <- "South Korea"
dpi_psp_sub$country[dpi_psp_sub$country == "S. Africa"] <- "South Africa"
dpi_psp_sub$country <- countrycode::countrycode(dpi_psp_sub$country, 'country.name', 'country.name')
dpi_psp_sub$cowc <- countrycode::countrycode(dpi_psp_sub$country, 'country.name', 'cowc')
dpi_psp_sub$cown <- countrycode::countrycode(dpi_psp_sub$country, 'country.name', 'cown')

impact_psp_sub <- impact %>%
  dplyr::select(`Country name`,
                `PA name`,
                `PA year`, 
                TERRPACT,
                POLPACT
                ) %>%
  dplyr::group_by(`Country name`, `PA year`, TERRPACT, POLPACT) %>%
  dplyr::summarise(`PA name` = paste(`PA name`, collapse=", ")) %>%
  dplyr::rename(country = `Country name`,
                pa_name = `PA name`,
                year = `PA year`)
impact_psp_sub$country[impact_psp_sub$country == "Israel (Palestine)"] <- "Israel"
impact_psp_sub$country[impact_psp_sub$country == "UK (Northern Ireland)"] <- "United Kingdom"
impact_psp_sub$country[impact_psp_sub$country == "Yugoslavia (Slovenia)"] <- "Yugoslavia"
impact_psp_sub$country <- countrycode::countrycode(impact_psp_sub$country, 'country.name', 'country.name')
impact_psp_sub$cowc <- countrycode::countrycode(impact_psp_sub$country, 'country.name', 'cowc')
impact_psp_sub$cown <- countrycode::countrycode(impact_psp_sub$country, 'country.name', 'cown')

psed <- dplyr::left_join(psed_prac, psed_prom)
psed_psp_sub <- psed %>% 
  dplyr::select(location,
                GWNo,
                PostStartDate,
                tps_autonomy,
                other_proprep,
                pps_cabinet,
                pps_sencabinet,
                pps_nsencabinet
  ) %>%
  dplyr::group_by(location, PostStartDate) %>%
  dplyr::distinct() %>%
  dplyr::rename(country = location,
                year = PostStartDate) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(year = as.numeric(stringr::str_extract(year, "^.{4}")))
psed_psp_sub$country <- countrycode::countrycode(psed_psp_sub$country, 'country.name', 'country.name')
psed_psp_sub$cowc <- countrycode::countrycode(psed_psp_sub$country, 'country.name', 'cowc')
psed_psp_sub$cown <- countrycode::countrycode(psed_psp_sub$country, 'country.name', 'cown')  


# Join data

tb_1 <- dplyr::left_join(tb, dtd_psp_sub)
tb_2 <- dplyr::left_join(tb_1, psed_psp_sub)
tb_3 <- dplyr::left_join(tb_2, impact_psp_sub)
tb_4 <- dplyr::left_join(tb_3, qog_ts_psp_sub)
tb_5 <- dplyr::left_join(tb_4, pax_psp_sub)
tb_6 <- dplyr::left_join(tb_5, dpi_psp_sub)

DataExplorer::plot_missing(tb_5)
Amelia::missmap(tb_6)


### Clean

t <- dplyr::select(tb_6, country, year, Coalition, coalition2, coalitiongov, gcman, gcimp, PpsEx)

### Data analysis

# Summary statistics

# Core model 

# Supplementary models 

# Robustness checks

# Model comparison (cPASS)
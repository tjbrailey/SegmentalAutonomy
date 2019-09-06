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
  dplyr::select(country, cowc, cown, year, gwno, ifs, dplyr::everything())

# Prepare other data
dtd_psp_sub <- dtd %>%
  dplyr::select(Refno,
                Nation,
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
                )
  
pax_psp_sub <- pax %>%
  dplyr::select(PpsAut,
                TpsAut,
                PpsEx,
                CenBan,
                
                PpsOro,
                PpsOthPr,
                StRef
                )

qog_ts_psp_sub <- qog_ts %>%
  dplyr::select(fe_etfra,
                iaep_ebbp,
                dpi_pr,
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
  )

dpi_psp_sub <- dpi %>%
  dplyr::select(auton,
                pr,
                sensys,
                eiec
                )

impact_psp_sub <- impact %>%
  dplyr::select(TERRPACT,
                POLPACT
                )

psed <- dplyr::left_join(psed_prac, psed_prom)
psed_psp_sub <- psed %>% 
  dplyr::select(tps_autonomy,
                other_proprep,
                pps_cabinet,
                pps_sencabinet,
                pps_nsencabinet
  )

# Join data

### Clean



### Data analysis

# Summary statistics

# Core model 

# Supplementary models 

# Robustness checks

# Model comparison (cPASS)
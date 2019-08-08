  ### Senior Thesis Code ### 
    ### Thomas Brailey ###
### POLI 191A // POLI 191B ###

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

  # General
dpi <- rio::import('DPI2012.xls')
qog <- rio::import('qog_std_cs_jan19.csv')
polityiv <- rio::import('p4v2017.xls')

# Clean workspace
rm(out, files, read_excel_allsheets)



# Join data



# Clean



# Data analysis

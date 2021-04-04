### Main script
###
### Goal: Run all scripts
###
### 1. Preliminaries
### 2. Cleaning
### 3. Analysis
### 4. Visualizations 
###

### 1. Preliminaries

rm(list = ls())

library(magrittr)
library(ggplot2)

# Load data

# Set working directory 
wd <- paste0(here::here(), "/data/") 

# Load functions
source(paste0(here::here(), "/code/functions/csts.R"))
source(paste0(here::here(), "/code/functions/lm.R"))
source(paste0(here::here(), "/code/functions/read_all_excel.R"))
source(paste0(here::here(), "/code/functions/lasso.R"))

# Load datasets
idc    <- rio::import(paste0(wd, "IDC_country-year_v1_0.RData"))
vdem   <- vdemdata::vdem
dpi    <- rio::import(paste0(wd, "DPI2017_basefile_Jan2018.csv"))
qog_ts <- rio::import(paste0(wd, "qog_std_ts_jan21.csv"))
rai    <- rio::import(paste0(wd, "RAI_country jan2021.xlsx"))
wvs    <- rio::import(paste0(wd, "WVS_TimeSeries_R_v1_6.rds"))
lai    <- rio::import(paste0(wd, "LAI_data_v6_temp2.sav"))
epr_cf <- rio::import(paste0(wd, "epr_conflict.csv"))
hum    <- rio::import(paste0(wd, "HUMAN Surveys - Country-Year Data.dta"))

files <- list.files(paste0(here::here(), "/data/"), "tjbrailey_psp_ontology.xlsx", full.names = TRUE)
files <- files[2]
ontology_data <- lapply(files, read_excel_allsheets)

mali  <- sf::st_read(paste0(here::here(), "/data/map/gadm36_MLI_shp/gadm36_MLI_0.shp"))
niger <- sf::st_read(paste0(here::here(), "/data/map/gadm36_NER_shp/gadm36_NER_0.shp"))

tuareg <- 
  sf::st_read(paste0(here::here(), "/data/map/GeoEPR-2019/GeoEPR.shp")) %>%
  dplyr::filter(
    group == "Tuareg" & 
      statename %in% c("Mali", "Niger") & 
      from == 1960
  )

africa <- sf::st_read(paste0(here::here(), "/data/map/Africa_SHP/Africa.shp"))
sf::st_crs(africa) <- 4326

### 2. Cleaning

source(paste0(here::here(), "/code/01_wrangle.R"))

### 3. Analysis

# Load ready-for-analysis data
psp <- rio::import(paste0(here::here(), "/data/tjbrailey_psp_clean.csv"))

# Exploratory data analysis
source(paste0(here::here(), "/code/02_eda.R"))

# Analysis for paper
source(paste0(here::here(), "/code/03_data_analysis.R"))

### 4. Visualizations 

source(paste0(here::here(), "/code/04_ontology.R"))

source(paste0(here::here(), "/code/05_other_vis.R"))


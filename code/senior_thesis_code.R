
getwd()

idc <- rio::import(file = paste0(here::here(), '/data/IDC_country-year_v1_0.RData'))
idc <- rio::import(file = paste0(here::here(), '/data/PSED_agreement.xlsx'))

dplyr::as_data_frame(idc)
summary(idc)
idc 
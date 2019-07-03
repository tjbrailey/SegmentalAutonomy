
getwd()
idc <- rio::import("IDC_country-year_v1_0.RData")
dplyr::as_data_frame(idc)
summary(idc)
idc 
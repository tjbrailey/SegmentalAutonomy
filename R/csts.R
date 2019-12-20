# Cross-Section Time-Series Anlaysis

### A function that takes an independent and a dependent variable and generates a cross-sectional analysis plot of the 
### two variables. This function will be sourced into the 03_tjbrailey_data_analysis.Rmd file and will run several 
### iterations using different IVs and DVs. 

# Input Variables
  # - dv - Dependent Variable
  # - iv - Independent Variable

csts <- function(dv, iv){
  
  # Load packages
  library(magrittr)
  library(ggplot2)
  
  # Load data
  psp <- rio::import(paste0(here::here(), "/data/tjbrailey_psp_clean.csv"))
  psp <- psp[,-1]
  
  # Subset by ethnically diverse states 
  #psp <- psp %>%
   # dplyr::filter(qog_al_ethnic > median(psp$qog_al_ethnic))
  
  # Make text look pretty 
  if(dv == "ucdp_cumulative_intensity"){
    dv_text <- "Conflict Intensity"
  } else if(dv == "qog_hum_trust"){
      dv_text <- "Social Trust"
  } else if(dv == "qog_hum_satdem"){
      dv_text <- "Satisfaction with Democracy"
  } else if(dv == "qog_hum_supdem"){
      dv_text <- "Support for Democracy"
  } else if(dv == "polity4_polity_score"){
      dv_text <- "Polity Score"
    }

  # Subset data
  dv_avg <- psp %>%
    dplyr::group_by(country) %>%
    dplyr::filter(any(!is.na(get(dv)))) %>%
    dplyr::summarize(avg = mean(get(dv), na.rm = TRUE))

  iv_yr <- psp %>%
    dplyr::group_by(country) %>%
    dplyr::filter(any(!is.na(get(dv)))) %>%
    dplyr::tally(get(iv) == 1) %>%
    dplyr::mutate(n = (n / ((max(psp$year) - min(psp$year)) + 1) * 100 ))

  iv_dv <- dplyr::left_join(iv_yr, dv_avg)

  # Plot
  plot1 <- ggplot(iv_dv, aes(x = n, y = avg)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    theme_bw() +
    labs(title = paste0(dv_text,  " Value by Percentage of Years Under Segmental Autonomy"),
       x = paste0("Percentage of Years Under Segmental Autonomy (", iv, ")"),
       y = paste0(dv_text, " Value")) +
    theme(title = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 14))
  
  ggsave(plot = plot1, filename = paste0(here::here(), "/vis/csts_",dv, ".png"), width = 12, height = 8)
  
  return(plot1)

} 
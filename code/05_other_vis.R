### 05_other_vis.R
###
### Goal: Create additional visualizations for paper. 
###
### Content: 
### 1. Map of Mali and Niger
###

### 1. Map of Mali and Niger

# Create map of Mali and Niger
map <- 
  ggplot() +
  geom_sf(data = tuareg, fill = "#e6d073", aes(color = "Tuareg Region"), alpha = .7) +
  geom_sf(data = africa, color = "gray", fill = NA) +
  geom_sf(data = mali, color = "black", fill = NA) +
  geom_sf(data = niger, color = "black", fill = NA) +
  geom_sf_label(data = mali, aes(label = NAME_0), fill = NA) +
  geom_sf_label(data = niger, aes(label = NAME_0), fill = NA) +
  scale_color_manual(values = "#e6d073") +
  labs(color = "") +
  coord_sf(xlim = c(-18, 20), ylim = c(0, 28)) +
  theme_void() + 
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),   
    panel.border = element_rect(colour = "black", fill = NA, size = 1)#,
    #panel.background = element_blank(), 
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #plot.background = element_blank()
  ) 
map

# Save data 
ggsave(plot = map, filename = paste0(here::here(), "/paper/tuareg_map.jpeg"))

### 2. Plot of autonomy implementation

plot1 <-
  psp %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    row_num = dplyr::row_number(),
    lag_aut = dplyr::lag(epr_reg_aut_dum)
    ) %>%
  dplyr::mutate(years_of_auton = cumsum(tidyr::replace_na(lag_aut, 0))) %>%
  dplyr::mutate(start = -max(row_num - max(years_of_auton) - 1)) %>%
  dplyr::mutate(standard = tidyr::full_seq(c(max(years_of_auton), start), period=1)) %>%
  dplyr::filter(country %in% c("Mali", "Ethiopia")) %>%
  ggplot(., mapping = aes(x = standard, y = vdem_v2elfrfair, group = country, color = country)) +
  geom_line() +
  scale_color_viridis_d()

plot1

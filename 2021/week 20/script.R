#' -------------------------------------------------------------------------------
#' Internet Access
#' The data this week comes from [Microsoft](https://github.com/microsoft/USBroadbandUsagePercentages) via #'   The Verge (https://www.theverge.com/22418074/broadband-gap-america-map-county-microsoft-data)
#' This [Microsoft](https://github.com/microsoft/USBroadbandUsagePercentages) talks about Internet Access over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 20
data_source <- "Microsoft via The Verge"
#libraries -----------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, extrafont, tigris, sf, stringr, patchwork)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2021, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)

# US map data
#us_counties <- counties()  %>% rmapshaper::ms_simplify()
us_states <- states() %>% select(STUSPS, NAME) %>%
  rename(ST = STUSPS) %>% rmapshaper::ms_simplify() %>% rename_all(tolower)

broadband_new <-
  broadband %>%
  select(-c(2, 3)) %>%
  mutate(broad_fcc = as.numeric(`BROADBAND AVAILABILITY PER FCC`)) %>%
  rename_all(tolower)

broad_state_medians <-
  broadband_new %>%
  group_by(st) %>%
  summarise(med_fcc = median(broad_fcc, na.rm = T))

# plots - map of the states filled by median broadband fcc
state_map <-
  broad_state_medians %>%
  left_join(us_states, by = "st") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = med_fcc)) +
  geom_sf_text(aes(label = str_wrap(name, 13)),
               check_overlap = T,
               color = 'grey20', size=4) +
  scale_fill_viridis_c(option = 'E', alpha = 0.7, labels=scales::percent) +
  coord_sf(xlim = c(-125, -67), ylim = c(23, 52), expand = c(0,0)) +
  theme_void() +
  labs(title="Distribution of Internet Access in the US",
       subtitle = "<br>The **Median** percent of people per State with access to fixed terrestrial<br>broadband at speeds of **25Mbps/3Mbps** as of the end of 2017<br><br><br>",
       caption = paste0(
         "**Graphic:** #TidyTuesday week",
         week_num,
         "<br> **Data:** ",
         data_source,
         "<br> **GitHub:** @johnmutiso"))+
  theme(legend.position = c(0.5,0.95),
        legend.key.width = unit(4, 'cm'),
        legend.direction = 'horizontal',
        legend.text = element_text(size=12, face='bold', color='grey60'),
        legend.title = element_blank(),
        plot.title = element_text(size=30, face='bold', color = 'grey60', hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(size = 20, box.colour = 'grey80', hjust=0.5),
        plot.caption = ggtext::element_markdown(size=13), plot.caption.position = 'plot')

# plot <-
state_boxplots <- 
broadband_new %>%
  left_join(us_states, by = "st") %>%
  left_join(broad_state_medians, by = 'st') %>%
  ggplot() +
  geom_boxplot(aes(
    reorder(name, broad_fcc, median, na.rm = T),
    x = broad_fcc,
    fill = med_fcc
  ), show.legend = F) +
  scale_x_reverse(labels = scales::percent) +
  scale_fill_viridis_c(option = 'E', alpha = 0.7) +
  theme_bw() +
  labs(x="% of people",y="State")+
  theme(axis.text = element_text(face='bold'))


pdesign <- 
"
1#######
12222222
12222222
12222222
12222222
12222222
12222222
12222222
12222222
"

final_plot <- 
  state_boxplots + state_map + plot_layout(design = pdesign) 

# save plot
ggsave(
  plot = final_plot,
  height = 11,
  width = 18,
  dpi = 500,
  device = 'png',
  filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')

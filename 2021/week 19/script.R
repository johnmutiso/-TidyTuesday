#' -------------------------------------------------------------------------------
#' Water Sources
#' The data this week comes from [ Water Point Data Exchange](https://data.waterpointdata.org/dataset/Water-Point-Data-Exchange-WPDx-Basic-/jfkt-jmqa)  
#' This [WPDX](https://www.waterpointdata.org/) talks about Water Sources over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 19
data_source <- "Water Point Data Exchange"
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
tuesdata <- tidytuesdayR::tt_load(2021, week = week_num)

#source('read_TT_data.R')
#read_TTdata(week = week_num, year=2021)

kenya_map <- 
  rKenyaCensus::KenyaCounties_SHP %>% 
  sf::st_as_sf() %>% sf::st_transform(crs = 4326, check=T) %>% 
  select(County, geometry) %>% 
  rmapshaper::ms_simplify()

water <- 
  tuesdata$water %>% 
  filter(country_name == "Kenya" & lat_deg < 5 & lon_deg>30 & lon_deg<50) %>% 
  mutate(report_date1=lubridate::as_date(report_date, format="%m/%d/%Y"),
         water_source = case_when(is.na(water_source) ~ '~Unknown Facility',
                                  TRUE ~ water_source),
         facility_type = stringr::str_wrap(case_when(is.na(facility_type) ~ '~Missing',
                                   TRUE ~ facility_type), 15))

colors <- c('#d11141','#00b159','#00aedb','#f37735','#ffc425')
# plot
plot <- 
  ggplot() +
    geom_sf(data=kenya_map, fill='snow1', col='grey70') +
  geom_point(data=water, aes(lon_deg, lat_deg, col=facility_type), size=0.8)+
    coord_sf(datum = sf::st_crs(4326)) +
    facet_wrap(.~stringr::str_wrap(water_source, width = 15), nrow = 3)+
    scale_color_manual(values = colors)+
  labs(fill = '', x = "", y = "", title = 'Water Sources in Kenya', 
       subtitle = "With facility type as <span style='color:#00b159'>Improved</span>, <span style='color:#f37735'>Unimproved</span>, <span style='color:#00aedb'>No facilities</span>, <span style='color:#ffc425'>Unknown</span><br> and those <span style='color:#d11141'>Missing</span> facility type info", 
       caption = paste0("**Graphic:** #TidyTuesday week",week_num, "<br> **Data:** ",data_source, "<br> **GitHub:** @johnmutiso"))  +
    theme_bw()+
  theme(plot.caption=ggtext::element_markdown(), 
        axis.text = element_text(size=7),
        strip.text = element_text(size=11, face='bold'),
        legend.position = 'none',
        strip.background = element_rect(fill='grey95', color='grey70'),
        legend.text = element_text(size=12, face='bold'),
        panel.border = element_rect(color='grey70'),
        plot.title = element_text(size=30, face = 'bold', family = 'Calibri', color='grey40'),
        plot.subtitle = ggtext::element_markdown(size=15, face='bold', color='grey20'))

 # save plot
ggsave(
    plot = plot,
    height = 9.2,
    width = 8.2,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')
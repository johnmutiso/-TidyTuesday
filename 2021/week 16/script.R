#' -------------------------------------------------------------------------------
#' TITLE
#' The data this week comes from [Data_source](data_source_link)  
#' This [article_source](source_link) talks about TITLE over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 16
data_source <- "Cameron Blevins & Richard W. Helbock(2021)<br> doi.org/10.7910/DVN/NUKCNA"
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)
post_offices1 <-
  post_offices %>% filter(!is.na(longitude),
                          discontinued > established,
                          discontinued < 2021,
                          established > 1600) %>%
  select(name,longitude, latitude, established, discontinued, continuous) %>% 
  mutate(exist=factor(case_when(between(discontinued, 1750, 1799)  ~ 1,
                         between(discontinued, 1800, 1849)  ~ 2,
                         between(discontinued, 1850, 1899)  ~ 3,
                         between(discontinued, 1900, 1949)  ~ 4,
                         between(discontinued, 1950, 1999)  ~ 5,
                         between(discontinued, 2000, 2020)  ~ 6),
                      levels = 1:6, 
                      labels = c('1750 - 1799', '1800 - 1849', '1850 - 1899', '1900 - 1949','1950 - 1999','2000 - date')))

map_data <- rnaturalearth::ne_countries(returnclass = 'sf')[,c('adm0_a3','geometry')] %>% 
  mutate(us = factor(if_else(adm0_a3=='USA', 1, 0)))
# plot
plot <- 
  ggplot() +
    geom_sf(data=map_data, aes(fill=us), show.legend = F) +
    geom_density_2d(data=post_offices1,aes(longitude, latitude), col='grey60', alpha=0.7, show.legend = F) +
    geom_point(data=post_offices1,aes(longitude, latitude, col=continuous), alpha=0.5,size=0.8, show.legend = F) +
  labs(x = "", y = "",
       title = 'US Post Offices',
       subtitle = '<span style="color:grey60">Geolocated Post Offices that Existed/Existing <br><span style="color:#5ac18e">CONTINUOUS</span> or <span style="color:#ff7373">NOT</span> since Establishment</span>',
       caption = paste0("**Graphic:** #TidyTuesday week",week_num, "<br> **Data:** ",data_source, "<br> **GitHub:** @johnmutiso"))  +
    coord_sf(xlim = c(-180, -39), ylim = c(20,80))+
    facet_wrap(exist~., ncol = 2)+
    scale_fill_manual(values = c('#f5f5f5', 'snow2'))+
    scale_color_manual(values=c('#ff7373','#5ac18e')) +
  theme_minimal()+
  theme(plot.caption=ggtext::element_markdown(color="grey50"),
        plot.background = element_rect(fill='#f5f5f5'),
        strip.text = element_text(size=11, face='bold'),
        plot.title = element_text(size=25, face='bold',color='grey80', hjus=0.5),
        plot.subtitle = ggtext::element_markdown(size=14, face='bold', family = 'Gadugi', hjust=0.5))

# save plot
ggsave(
    plot = plot,
    height = 10,
    width = 7.6,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')

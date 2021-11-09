#' -------------------------------------------------------------------------------
#' TITLE
#' The data this week comes from [Data_source](data_source_link)  
#' This [article_source](source_link) talks about TITLE over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 46
data_source <- "package: afrilearndata"
#libraries -----------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, extrafont, patchwork, stringr, sf, afrilearndata)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2021, week = week_num)

source('read_TT_data.R')
#read_TTdata(week = week_num, year=2021)
large_airports <-
  afriairports %>% 
  filter(type=='large_airport')
  

# plot
plot <- 
  ggplot() +
  geom_sf(data=st_as_sf(africountries), col='grey55', fill='#6a6a77') +
  geom_sf(data=st_as_sf(afrihighway), color='#ffff66', size=1.2) +
  geom_sf(data=large_airports, pch=21, size=3, stroke=2, color='white') +
  geom_sf(data=large_airports, size=3, color='#f7347a')+
  ggtext::geom_richtext(aes(x=-9, y=-10), 
                        label='<span style="color:#ffff66">**Highway Connectivity**</span><br> & <span style="color:#f7347a;"> **Large Airports**',
                        size=8, fill='grey60')+
  labs(fill = '', x = "", y = "",
       caption = paste0("**Graphic:** #TidyTuesday week",week_num, "<br> **Data:** ",data_source, "<br> **GitHub:** @johnmutiso"))  +
  coord_sf() +
  theme_light() +
  theme(plot.caption=ggtext::element_markdown())

# save plot
ggsave(
    plot = plot,
    height = 10,
    width = 12,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')
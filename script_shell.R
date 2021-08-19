#' -------------------------------------------------------------------------------
#' TITLE
#' The data this week comes from [Data_source](data_source_link)  
#' This [article_source](source_link) talks about TITLE over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 22
data_source <- ""
#libraries -----------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, extrafont, patchwork, stringr)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2021, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

# plot
plot <- 
  ggplot() +
  labs(fill = '', x = "", y = "",
       caption = paste0("**Graphic:** #TidyTuesday week",week_num, "<br> **Data:** ",data_source, "<br> **GitHub:** @johnmutiso"))  +
  theme(plot.caption=ggtext::element_markdown())

# save plot
ggsave(
    plot = plot,
    height = 8.8,
    width = 3.5,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')
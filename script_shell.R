#' -------------------------------------------------------------------------------
#' TITLE
#' The data this week comes from [Data_source](data_source_link)  
#' This [article_source](source_link) talks about TITLE over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 9
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

# plot
plot <- 
  ggplot()
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
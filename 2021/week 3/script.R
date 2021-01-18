#' -------------------------------------------------------------------------------
#' Art Collections
#' The data this week comes from [Tate Art Museum](https://github.com/tategallery/collection). 
#' This [Data visualisations by Florian Kräutli](http://research.kraeutli.com/index.php/2013/11/the-tate-collection-on-github/) talks about Art Collections in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 3
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
    artwork %>% 
    ggplot() +
    geom_jitter(aes(year, width/height), size=1/3, col='snow1', alpha=0.3) +
    geom_text(aes(1700,0.04, label="Art Collections"), size=16, family='Old English Text MT', col='snow3')+
    geom_text(aes(1700,0.007, label="Width:Height ratio Over time"), size=8, family='Chiller', col='snow2')+
    scale_y_log10(label=scales::comma) +
    labs(y='Width:Height ratio',
        x='Year of creation',
        caption = "Graphic: #TidyTuesday week 3\n GitHub: @johnmutiso\nData: Tate Art Museum(GitHub:@tategallery)")+
    theme_minimal() +
    theme(plot.background = element_rect(fill='#133337'),
        panel.grid = element_blank(),
        axis.line = element_line(color = 'grey40'),
        axis.title = element_text(color='grey80', size = 15, family='Calibri'),
        axis.text = element_text(color = 'grey70'),
        plot.caption = element_text(size = 7, color='snow3'))

# save plot
ggsave(
    plot = plot,
    height = 5.5,
    width = 8,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

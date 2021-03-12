#' -------------------------------------------------------------------------------
#' INPUT THE SUBJECT TITLE OF THE DATASET
#' The data this week comes from [SOURCE_OF_DATA](URL_TO_DATA). 
#' This [ARTICLE_SOURCE](LINK_TO_ARTICLE) talks about SUBJECT TITLE in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 4
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

kenya_sf <- sf::st_as_sf(rKenyaCensus::KenyaCounties_SHP)

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')

read_TTdata(week = week_num, year=2021)

plot <- 
gender %>% 
    mutate(County=str_to_upper(County)) %>%
    slice(-1) %>% 
    left_join(kenya_sf, by = 'County') %>% sf::st_as_sf() %>% 
ggplot() +
    geom_sf(aes(fill=Male/Female), size=0.01, color='snow2') +
    geom_sf_text(aes(label=County), check_overlap = T, fontface='bold', col='grey25', size=4)+
    scale_color_fermenter(aesthetics = c('fill'), palette = 1, labels=c('Equal\ngenders','10%\nmore men','20%\nmore men')) +
    labs(title = "Kenya 2019 Census",
        subtitle = "\nMale to Female Ratio",
        fill="",
        caption = "Graphic: #TidyTuesday week 4\n GitHub: @johnmutiso\nData:  rKenyaCensus R package by Shelmith Kariuki")+
    theme_void()+
    theme(plot.background = element_rect(fill = 'grey40'),
        legend.position = c(0.6,0.9),
        legend.text = element_text(size=15, family='Calibri', color='snow1', face='bold'),
        legend.direction = 'horizontal',
        legend.key.size = unit(2, 'cm'),
        legend.key.height = unit(0.8, 'cm'),
        plot.caption = element_text(size=13, color = 'snow1', face='bold'),
        plot.title = element_text(color = 'snow2', family = 'Arial', size = 35,hjust=0.5, face='bold'),
        plot.subtitle = element_text(size = 26, color='snow1', hjust=0.5, family = 'Calibri', face = 'bold.italic'))


# save plot
ggsave(
    plot = plot,
    height = 13,
    width = 8.9,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)


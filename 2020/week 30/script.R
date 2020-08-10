#' -------------------------------------------------------------------------------
#' Australian Pets
#' The data this week comes from [ RSPCA, Townsville Animal Complaints and Brisbane Open Data - Animal Complaints]
#' (https://www.rspca.org.au/what-we-do/our-role-caring-animals/annual-statistics,
#'  https://data.gov.au/data/dataset/animal-complaints,
#'  https://www.data.brisbane.qld.gov.au/data/dataset/96bec69c-6170-4ef0-93f1-eda279149b97).
#' 
#' This [RSPCA Report](https://www.rspca.org.au/sites/default/files/RSPCA%20Report%20on%20animal%20outcomes%202018-2019.pdf) 
#' talks about Australian Pets in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 30
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

# plot
plot <-
    animal_outcomes %>% 
    pivot_longer(cols = 4:11, names_to = 'states', values_to = 'count') %>% 
    ggplot() +
    geom_bar(aes(year, count, fill = outcome),stat = 'identity') +
    facet_grid(animal_type~states) +
    scale_fill_brewer(palette = 'Set1') +
    labs(title = 'Australian Pets', subtitle = "In VICTORIA, More DOGS are RECLAIMED; More CATS are EUTHANIZED!! Are Dogs becoming Dearer than Cats?!
         \nWhat's happening in Queensland with the Euthanization of Wildife pets?",
         caption = "Credit: @johnmutiso\n Graphic: #TidyTuesday week 30\nData: RSPCA via @geokaramanis") +
    theme_minimal(base_family = 'Tahoma', base_size = 14) +
    theme(plot.title = element_text(size = 25, family = 'Tahoma'),
          plot.subtitle = element_text(size = 14, family = 'Perpetua'))


    
# save plot
ggsave(
    plot = plot,
    height = 11,
    width = 20,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png') ,
    path = paste0('2020/week ', week_num)
    
)

# data via Georgios Karamanis (@geokaramanis)
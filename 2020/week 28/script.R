#' -------------------------------------------------------------------------------
#' Coffee Ratings
#' The data this week comes from [Coffee Ratings](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-07/readme.md). 
#' This [Yorgos Askalidis - TWD](https://towardsdatascience.com/the-data-speak-ethiopia-has-the-best-coffee-91f88ed37e84) talks about Coffee Rating in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

week_num = 28
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

source('read_TT_data.R')
read_TTdata(week = week_num)

# plot
plot <- 
    coffee_ratings %>%
    filter(altitude_mean_meters < 9000) %>% 
    group_by(country_of_origin, species) %>% 
    summarise(mean_points = mean(total_cup_points),
              altitude = mean(altitude_mean_meters),
              flavor = mean(flavor),
              sweetness = mean(sweetness),
              aftertaste = mean(aftertaste)) %>% 
    ggplot(aes(sweetness, reorder(country_of_origin, desc(sweetness)), size = flavor, fill = aftertaste, color = species)) +
    geom_point(shape = 21) +
    scale_fill_viridis_c('After\n    taste') +
    scale_color_manual(values = c('#ff0000','snow1')) +
    scale_size_binned(nice.breaks = T) +
    labs(y = "", title = 'Coffee Ratings', 
         caption = "Credit: @johnmutiso\nData: Coffee Quality Database via James LeDoux (@jmzledoux)\nGraphic: TidyTuesday Week 28") +
    theme_minimal()+
    theme(plot.background = element_rect(fill = '#20b2aa'),
          panel.grid = element_line(colour = '#468499'), 
          axis.text = element_text(size = 12, face = 'bold'),
          plot.title = element_text(size = 20, family = 'Tahoma', hjust = 0.5))


# save plot
ggsave(
    plot = plot,
    height = 8.8,
    width = 8,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2020/week ', week_num)
)

# Data: James LeDoux (@jmzledoux)
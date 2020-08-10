#' -------------------------------------------------------------------------------
#' Palmer Penguins
#' The data this week comes from [ Dr. Kristen Gorman](https://www.uaf.edu/cfos/people/faculty/detail/kristen-gorman.php)
#'  via palmerpenguins R package. 
#' This [Palmer Penguins](https://allisonhorst.github.io/palmerpenguins/) talks about Palmer Penguins in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 31
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
    penguins %>% 
    pivot_longer(cols = 3:6, names_to = 'measure', values_to = 'value') %>% 
    mutate(measure = case_when(
        str_detect(measure, 'bill_depth')~'Bill Depth (mm)',
        str_detect(measure, 'bill_length')~'Bill Length (mm)',
        str_detect(measure, 'mass')~'Body Mass (grams)',
        str_detect(measure, 'flipper')~'Flipper Length (mm)',
    )) %>% 
    ggplot() +
    geom_boxplot(aes(island, value, fill = species, col = species), notch = T) +
    facet_wrap(ncol = 4, .~measure, scales = 'free_y') +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = rep('snow1', 3)) +
    labs(y = '', title = 'Palmer Penguins',
         caption = 'Credit: @johnmutiso\nData: Gorman, Williams and Fraser, 2014\n      via palmerpenguins R package\nGraphic: #TidyTuesday week 31') +
    theme_minimal(base_family = 'Courier New', base_size = 15) +
    theme(strip.background = element_rect(fill = 'grey70', color = 'grey70'),
          plot.background = element_rect(fill = '#cdc49e'),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = 'top',
          axis.text.x = element_text(face = 'bold', size = 10),
          strip.text = element_text(size = 14, face = 'bold'),
          plot.title = element_text(face = 'bold', family = 'Tahoma', size = 20),
          plot.caption = element_text(size = 7, hjust=0), plot.caption.position = 'plot')
    

# save plot
ggsave(
    plot = plot,
    height = 6,
    width = 13,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2020/week ', week_num)
)

# Data source: Gorman, Williams and Fraser, 2014
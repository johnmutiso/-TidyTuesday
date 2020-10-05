#' -------------------------------------------------------------------------------
#' Himalayan Climbing Expeditions
#' The data this week comes from [Himalayan Climbers](https://www.himalayandatabase.com/).
#' This [Analyzing Himalayan peaks and first ascents by Alex Cookson](https://www.alexcookson.com/post/analyzing-himalayan-peaks-first-ascents/)
#'   talks about Himalayan Climbing Expeditions in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 39
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
    members %>%
    filter(!is.na(sex)) %>%
    mutate(sex = case_when(sex == 'F' ~ 'Female',
                           sex == 'M' ~ 'Male')) %>%
    ggplot() +
    geom_density(aes(age, fill = season), alpha = 0.7, col = 'snow1') +
    scale_fill_brewer('Season', palette = 'Set1') +
    facet_wrap(. ~ sex, ncol = 1) +
    labs(
        title = 'Himalayan Climbers',
        x = 'Age (years)',
        y = 'Density',
        subtitle = 'Age distribution by Season',
        caption = 'Credit: @johnmutiso\nData: Himalayan Climbers via @alexcookson\nGraphic: #TidyTuesday Week 39'
    ) +
    theme_linedraw() +
    theme(
        legend.position = c(0.89, 0.8),
        panel.grid = element_line(color = 'grey70'),
        legend.background = element_blank(),
        plot.title = element_text(
            size = 26,
            family = 'Calibri',
            face = 'bold',
            hjust = 0.5,
            color = 'grey70'
        ),
        plot.subtitle = element_text(
            size = 15,
            family = 'Tahoma',
            face = 'bold',
            hjust = 0.5,
            color = 'grey30'
        ),
        strip.text = element_text(face = 'bold', size = 12),
        plot.caption = element_text(color = 'grey40')
    )

# save plot
ggsave(
    plot = plot,
    height = 9,
    width = 5,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

# Data via @alexcookson
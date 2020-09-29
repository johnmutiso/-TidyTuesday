#' -------------------------------------------------------------------------------
#' Friends
#' The data this week comes from [friends R package](https://github.com/EmilHvitfeldt/friends).
#' This [ceros interactive article](https://www.ceros.com/originals/friends-scripts-25th-anniversary-catchphrase-scenes-quotes/) talks about Friends in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 37
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

#
season_ns <-
    friends_emotions %>%
    group_by(season, episode) %>%
    summarise(season_n = n())

plot <-
    friends_emotions %>%
    group_by(season, episode, emotion) %>%
    summarise(n_utter = n()) %>%
    ungroup() %>%
    left_join(season_ns, by = c('season', 'episode')) %>%
    mutate(perc = n_utter / season_n,
           season = paste0("Season ", season)) %>%
    ggplot() +
    geom_bar(aes(factor(episode), perc, fill = emotion),
             stat = 'identity',
             col = 'grey80') +
    facet_wrap(. ~ season, nrow = 1) +
    scale_fill_brewer('Utterance\n Emotion',
                      palette = 'Set1',
                      direction = -1) +
    scale_y_continuous(expand = c(0.2, 0), labels = scales::percent) +
    coord_polar() +
    labs(
        y = 'Percent',
        x = 'Episode',
        title = "F.R.I.E.N.D.S",
        caption = "Credit: @johnmutiso\nData: Friends R package via @Emil_Hvitfeldt\nGraphic: TidyTuesday week 37"
    ) +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(family = 'Calibri'),
        axis.text.y = element_text(color = 'red', face = 'bold'),
        legend.title = element_text(face = 'bold'),
        axis.title = element_text(size = 13, family = 'Arial'),
        legend.position = 'top',
        legend.key.width = unit(1, 'cm'),
        legend.box = 'horizontal',
        strip.text = element_text(
            size = 14,
            family = 'Arial',
            face = 'bold'
        ),
        plot.title = element_text(
            size = 70,
            family = 'Chiller',
            face = 'bold.italic',
            hjust = 0.5
        )
    )


# save plot
ggsave(
    plot = plot,
    height = 6.5,
    width = 14,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

# Via @Emil_Hvitfeldt
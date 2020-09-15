#' -------------------------------------------------------------------------------
#' Avatar: The Last Airbender
#' The data this week comes from [appa R package](https://github.com/averyrobbins1/appa).
#' This [Exploring Avatar: The Last Airbender transcript data article](https://www.avery-robbins.com/2020/07/11/avatar-eda/)
#'  talks about Avatar: The Last Airbender in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 33
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

# plot
avatar <- avatar %>%
    mutate_at(.vars = c(2, 4, 6:10), .funs = as.factor)

plot <-
    avatar %>%
    group_by(book, book_num, chapter, chapter_num, director) %>%
    summarise(Rating = mean(imdb_rating, na.rm = T)) %>%
    ungroup() %>%
    group_by(book_num) %>%
    arrange(book_num, chapter_num) %>%
    mutate(
        id = 1:n(),
        chapter = str_replace_all(chapter, '\\,', ',\n'),
        book = fct_relevel(book, 'Water')
    ) %>%
    ggplot(aes(chapter_num, Rating)) +
    facet_wrap(. ~ book, nrow = 1) +
    geom_line(aes(group = book), size = 4, col = 'grey60') +
    geom_point(
        aes(fill = director),
        pch = 21,
        stroke = 2,
        size = 6,
        col = 'snow1'
    ) +
    geom_text(
        aes(x = chapter_num, y = Rating + 0.05 , label = chapter),
        show.legend = F,
        hjust = 0,
        check_overlap = T,
        family = "Tahoma",
        color = "snow1"
    ) +
    scale_y_continuous(breaks = seq(7, 10, 0.5)) +
    scale_x_continuous(limits = c(0, 28), breaks = seq(0, 20, 5)) +
    scale_colour_brewer("Director",
                        palette = 'Set1',
                        aesthetics = c('colour', 'fill')) +
    labs(
        x = 'Chapter Number',
        y = 'IMDB Rating',
        title = "Avatar: The Last Airbender",
        subtitle = "\n IMDB Ratings",
        caption = 'Credit: @johnmutiso\nData:Appa R Package by @robbins_ave\nGraphic: #TidyTuesday week 33'
    ) +
    theme_dark() +
    theme(
        axis.text = element_text(size = 14, color = 'snow1'),
        axis.title = element_text(size = 16, color = '#ffa500'),
        legend.position = c(0.59, 0.17),
        plot.title = element_text(
            color = 'snow1',
            family = 'Segoe Script',
            size = 25,
            face = 'bold',
            hjust = 0.5
        ),
        plot.subtitle = element_text(
            size = 20,
            family = 'Tahoma',
            color = "#ffa500",
            hjust = 0.5
        ),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = 'bold'),
        plot.background = element_rect(fill = '#576675'),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(color = 'grey75'),
        strip.text.x = element_text(size = 20, family = 'Segoe Script')
    )

plot

# save plot
ggsave(
    plot = plot,
    height = 9.4,
    width = 20,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

# Appa R Package by #Avery Robbins @robbins_ave
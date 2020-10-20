#' -------------------------------------------------------------------------------
#' NCAA Women's Basketball
#' The data this week comes from [FiveThirtyEight](https://fivethirtyeight.com/features/louisiana-tech-was-the-uconn-of-the-80s/).
#' This [FiveThirtyEight](https://fivethirtyeight.com/features/louisiana-tech-was-the-uconn-of-the-80s/)
#'   talks about NCAA Women's Basketball in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 41
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
    tournament %>%
    mutate(x1st_game_at_home = str_replace_all(x1st_game_at_home, 'Y\\^', 'Y')) %>%
    ggplot(aes(year %>% factor(), full_w)) +
    geom_jitter(aes(col = x1st_game_at_home)) +
    geom_boxplot(
        aes(fill = x1st_game_at_home),
        alpha = 0.8,
        outlier.alpha = 0.7,
        outlier.shape = 5
    ) +
    geom_text(
        aes(label = year, y = 8),
        size = 3.5,
        family = 'Tahoma',
        fontface = 'bold',
        col = '#407294',
        check_overlap = T
    ) +
    facet_wrap( ~ year, scales = 'free_x', nrow = 1) +
    scale_color_manual(values = c('#0086ad', '#74d680'),
                       aesthetics = c('color', 'fill')) +
    scale_y_continuous(limits = c(8, 42)) +
    geom_hline(yintercept = 9,
               col = 'grey80',
               size = 2) +
    labs(
        title = '\n\nNCAA Women`s Basketball: Total Wins per Team (1982-2018)',
        subtitle = '<span style="color:#74d680">Green:</span> Teams Whose First Game was Played at Home Court;\n <span style="color:#0086ad">Blue</span> Otherwise',
        y = 'Total sum of wins',
        caption = "\n\n\nCredit: @johnmutiso\nData: FiveThirtyEight\nGraphic: #TidyTuesday Week 41"
    ) +
    theme_minimal() +
    theme(
        strip.placement = 'inside',
        strip.text = element_blank(),
        plot.background = element_rect(fill = 'snow1', color = 'grey70', size = 4),
        panel.border = element_rect(color = 'grey80', fill = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_blank(),
        legend.position = 'none',
        panel.grid = element_blank(),
        plot.title = element_text(
            size = 25,
            family = 'Lucida Handwriting',
            hjust = 0.5,
            face = 'bold',
            color = 'grey40'
        ),
        plot.subtitle = ggtext::element_markdown(
            face = 'bold',
            size = 16,
            hjust = 0.5,
            family = 'Tahoma',
            color = 'grey70'
        ),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(color = 'grey70')
    )

# save plot
ggsave(
    plot = plot,
    height = 7,
    width = 18,
    dpi = 400,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

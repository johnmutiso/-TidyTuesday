#' -------------------------------------------------------------------------------
#' CHOPPED
#' The data this week comes from [Kaggle & IMDB](https://www.kaggle.com/jeffreybraun/chopped-10-years-of-episode-data,
#' https://www.imdb.com/title/tt1353281/episodes?ref_=tt_eps_sn_mr).
#' This [vice](https://www.vice.com/en_us/article/wj8q39/how-chopped-became-tvs-greatest-cooking-show) talks about CHOPPED in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 35
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

chopped <-
    readr::read_tsv(
        'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv'
    )
# source('read_TT_data.R')
# read_TTdata(week = week_num)

# plot
chopped <-
    chopped %>%
    mutate(air_date = lubridate::as_date(air_date, format = "%B %d, %Y"))

set.seed(3453)
colors <- randomcoloR::distinctColorPalette(20)

mean_IDMb  = mean(chopped$episode_rating, na.rm = T)
plot <-
    chopped %>%
    ggplot(aes(factor(season), episode_rating)) +
    # geom_boxplot() +
    geom_point(
        aes(fill = factor(season_episode)),
        pch = 21,
        stroke = 1,
        size = 5.5,
        col = '#eef1b6',
        alpha = 0.8,
        show.legend = F
    ) +
    geom_text(
        aes(label = season_episode),
        size = 2.5,
        col = 'grey30',
        fontface = 'bold',
        family = 'Tahoma',
        check_overlap = F
    ) +
    geom_boxplot(alpha = 0) +
    geom_hline(yintercept = mean_IDMb,
               lty = 3,
               size = 0.4) +
    scale_fill_manual(values = colors) +
    geom_smooth() +
    facet_grid(1 ~ season, scales = 'free', space = 'free') +
    labs(y = 'iMDb Episode Ratings', title = 'CHOPPED (TV Series) IMDb Ratings',
         caption = 'Credit: @johnmutiso\nGraphic: #TidyTuesday week 35\nData: Kaggle & IDMb') +
    theme_minimal() +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text.y = element_blank(),
        panel.grid = element_line(color = 'grey90'),
        strip.text.x = element_text(family = 'Calibri'),
        plot.background = element_rect(fill = '#faebd7'),
        plot.title.position = 'panel',
        plot.title = element_text(
            family = 'Tahoma',
            face = 'bold',
            color = '#003366'
        )
        
    )



# save plot
ggsave(
    plot = plot,
    height = 5.5,
    width = 12,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

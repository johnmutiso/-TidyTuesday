#' -------------------------------------------------------------------------------
#' INPUT THE SUBJECT TITLE OF THE DATASET
#' The data this week comes from [SOURCE_OF_DATA](URL_TO_DATA).
#' This [ARTICLE_SOURCE](LINK_TO_ARTICLE) talks about SUBJECT TITLE in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 52
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

# plot
countries <- c("ZAF", "USA", "GBR", "CHE")

flags_data <-
    big %>%
    filter(iso_a3 %in% countries) %>%
    group_by(name, iso_a3) %>%
    summarise(date = max(date),
        dollar_price = big$dollar_price[big$date == date &
                big$iso_a3 == iso_a3]) %>%
    ungroup() %>%
    mutate(iso_a2 = str_to_lower(str_sub(iso_a3, 1, 2)))

# plot <-
plot <-
    ggplot(big, aes(date, dollar_price)) +
    geom_line(aes(
        group = name,
        col = iso_a3 %in% countries,
        size = iso_a3 %in% countries
    ),
        show.legend = F) +
    #add flag lables
    ggflags::geom_flag(data = flags_data, aes(x = date, country = iso_a2), size =
            6) +
    geom_text(
        data = flags_data,
        aes(x = date, label = name),
        size = 4,
        hjust = 1.1,
        family = 'Calibri',
        col = 'grey40'
    ) +
    geom_text(
        data = NULL,
        aes(
            x = as.Date('2003-01-01'),
            y = 7.5,
            label = str_wrap("The Big Mac Index", 12)
        ),
        size = 12,
        hjust = 0.5,
        col = 'grey50',
        family = 'Tunga',
        fontface = 'bold'
    ) +
    labs(x = 'Date',
        y = 'Dollar Price',
        caption = "Graphic: #TidyTuesday week 52\nGitHub: @johnmutiso\nData: TheEconomist") +
    scale_size_manual(values = c(0.3, 1.2)) +
    scale_color_manual(values = c('grey90', 'grey75')) +
    scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
    theme_bw() +
    theme(axis.title = element_text(size = 12, color = 'grey60'),
        plot.caption = element_text(color = 'grey80'))

# save plot
ggsave(
    plot = plot,
    height = 5,
    width = 8,
    dpi = 350,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

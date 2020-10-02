#' -------------------------------------------------------------------------------
#' US Spending on Kids
#' The data this week comes from [Urban Institute](https://datacatalog.urban.org/dataset/state-state-spending-kids-dataset)
#'  via Joshua Rosenberg's(https://twitter.com/jrosenberg6432) *tidykids* R package.
#' This [Joshua Rosenberg's tidykids package article](https://jrosen48.github.io/tidykids/index.html) talks about
#'   US Spending on Kids in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 38
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(gganimate)
library(sf)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

# plot
plot <-
    kids %>%
    filter(year %in% c(2000, 2016) &
               variable == 'Medicaid_CHIP') %>%
    pivot_wider(
        id_cols = c(1, 2),
        names_from = 'year',
        values_from = 'inf_adj_perchild',
        names_prefix = 'yr'
    ) %>%
    ggplot(aes(y = reorder(state, desc(yr2016)))) +
    geom_linerange(aes(xmin = yr2000, xmax = yr2016),
                   col = '#065535',
                   size = 1.5) +
    geom_point(
        aes(x = yr2000),
        pch = 21,
        size = 3,
        stroke = 2.5,
        fill = '#ffe4e1',
        col = '#5ac18e'
    ) +
    geom_point(
        aes(x = yr2016),
        pch = 21,
        size = 3,
        stroke = 2.5,
        fill = '#ffe4e1',
        col = '#5ac18e'
    ) +
    scale_x_continuous(labels = scales::dollar_format(scale = 1000)) +
    labs(
        x = 'Inflation Adjusted Medicaid/CHIP expenditure per Child',
        y = 'State',
        title = 'Change in Medicaid/CHIP (USA): 2000 - 2016',
        subtitle = '* {CHIP} Children`s Health Insurance Program',
        caption = "\nCredit: @johnmutiso\nData: Urban Institute via Joshua Rosenberg's *tidykids* R package\nGraphic: TidyTuesday week 38"
    ) +
    theme_minimal() +
    theme(
        axis.text = element_text(
            family = 'Calibri',
            size = 14,
            face = 'bold'
        ),
        plot.background = element_rect(fill = '#d5e3e3'),
        panel.grid = element_line(color = '#cbcba9'),
        plot.title = element_text(
            color = '#003366',
            face = 'bold',
            size = 16,
            family = 'Nirmala UI'
        ),
        plot.subtitle = element_text(family = 'Vani'),
        axis.title = element_text(size = 13)
    )


# save plot
ggsave(
    plot = plot,
    height = 11.5,
    width = 7,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

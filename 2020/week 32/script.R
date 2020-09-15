#' -------------------------------------------------------------------------------
#' European energy
#' The data this week comes from [Eurostat.](https://ec.europa.eu/eurostat/statistics-explained/index.php/Electricity_generation_statistics_%E2%80%93_first_results). 
#' This [Washington Post Energy](https://www.washingtonpost.com/climate-environment/2020/07/30/biden-calls-100-percent-clean-electricity-by-2035-heres-how-far-we-have-go/?arc404=true&utm_medium=social&utm_source=twitter&utm_campaign=wp_graphics) talks about European energy in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 32
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

energy_types_L <-
    energy_types %>%
    pivot_longer(cols = 5:7, names_to = 'year')

# plot
plot <-
    energy_types_L %>%
    filter(!is.na(country_name)) %>%
    ggplot(aes(year, value, group = country, col = country)) +
    geom_point(size = 1.8, show.legend = F) +
    geom_line(size = 1.5, show.legend = F) +
    geom_text(
        data = energy_types_L %>% filter(year == '2016'),
        aes(year, value + 5000, label = country_name),
        check_overlap = T,
        fontface = 'bold',
        family = 'Courier New',
        show.legend = F,
        hjust = 0.2
    ) +
    facet_grid(. ~ type) +
    scale_y_continuous(labels = scales::unit_format(suffix = 'k', scale = 0.001)) +
    labs(
        x = '',
        y = 'Energy (GWh)',
        title = 'European Energy',
        caption = 'Credit: @johnmutiso\nData:Eurostat Energy\nGraphic: #TidyTuesday week 32'
    ) +
    theme_bw() +
    theme(
        strip.text = element_text(
            color = 'grey30',
            family = 'Bauhaus 93',
            size = 13
        ),
        plot.title = element_text(
            size = 30,
            face = 'bold',
            family = 'Kristen ITC',
            color = '#ffa700'
        ),
        axis.text = element_text(
            family = 'Courier New',
            size = 11,
            face = 'bold'
        )
    )

# save plot
ggsave(
    plot = plot,
    height = 9,
    width = 16,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2020/week ', week_num)
)

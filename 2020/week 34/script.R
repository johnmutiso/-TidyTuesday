#' -------------------------------------------------------------------------------
#' Plants in Danger
#' The data this week comes from [International Union for Conservation of Nature (IUCN) Red list of Threatened Species (Version 2020-1) ]
#'   (https://www.iucnredlist.org/).
#' This [A third of the tropical African flora is potentially threatened with extinction]
#' (https://advances.sciencemag.org/content/5/11/eaax9444) talks about Plants in Danger in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 34
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
plants %>%
    pivot_longer(cols = c(6:23),
                 names_to = 'category',
                 values_to = 'value') %>%
    mutate(
        group1 = case_when(
            str_detect(category, 'threat') ~ 'Threat',
            str_detect(category, 'action') ~ 'Action'
        ),
        group2 = case_when(
            str_detect(category, 'AA') ~ 'Agriculture & Aquaculture',
            str_detect(category, 'BRU') ~ 'Biological Resource Use',
            str_detect(category, 'RCD') ~ 'Commercial Development',
            str_detect(category, 'ISGD') ~ 'Invasive Species',
            str_detect(category, 'EPM') ~ 'Energy Production & Mining',
            str_detect(category, 'CC') ~ 'Climate Change',
            str_detect(category, 'HID') ~ 'Human Intrusions',
            str_detect(category, '_P') ~ 'Pollution',
            str_detect(category, 'TS') ~ 'Transportation Corridor',
            str_detect(category, 'NSM') ~ 'Natural System Modifications',
            str_detect(category, 'GE') ~ 'Geological Events',
            str_detect(category, 'threat_NA') ~ 'Threat Uknown',
            str_detect(category, 'LWP') ~ 'Land & Water Protection',
            str_detect(category, 'SM') ~ 'Species Management',
            str_detect(category, 'LP') ~ 'Law & Policy',
            str_detect(category, 'RM') ~ 'Research & Monitoring',
            str_detect(category, 'EA') ~ 'Education & Awareness',
            str_detect(category, 'action_NA') ~ 'Current Action Unknown'
        ),
        year_last_seen = fct_relevel(factor(year_last_seen), c('Before 1900')),
        group1 = fct_relevel(group1, "Threat"),
        group2 = fct_relevel(group2, "Threat Uknown")
    ) %>%
    group_by(continent,
             group,
             year_last_seen,
             red_list_category,
             group1,
             group2) %>%
    summarise(tot = sum(value, na.rm = T)) %>%
    filter(!tot == 0) %>%
    ggplot() +
    stat_bin_2d(
        aes(year_last_seen,  group2, fill = tot),
        na.rm = T,
        drop = T,
        col = 'grey70'
    ) + facet_grid(group1 ~ continent, scales = 'free', space = 'free_y') +
    scale_fill_viridis_c("No. of plants") +
    labs(title = "Plant Extinction", x = 'Year last seen', y='Source', subtitle = "\nThreats & Actions",
         caption = 'Plot Credit: @johnmutiso\nData:IUCN via behance.net/florentlavergne\n    (Credit: Florent Lavergne and Cédric Scherer)\nGraphic: #TidyTuesday week 34') +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'grey60'),
        strip.text = element_text(
            family = 'Tahoma',
            face = 'bold',
            size = 14
        ),
        axis.text = element_text(family = 'Courier New', size = 13),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 25, family = 'Arial', face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 20, color = 'dark green')
    )

# save plot
ggsave(
    plot = plot,
    height = 8,
    width = 18,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

# via Florent Lavergne
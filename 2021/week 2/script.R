#' -------------------------------------------------------------------------------
#' Transit Costs Project
#' The data this week comes from [Transit Costs Project](https://transitcosts.com/). 
#' This [ARTICLE_SOURCE](LINK_TO_ARTICLE) talks about Transit Costs Project in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 2
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

transit_cost_1 <- 
    transit_cost %>% 
    mutate(real_cost_n = as.double(real_cost),
        time_comp = as.numeric(end_year)-as.numeric(start_year)) %>% 
    filter(!is.na(city) | real_cost_n >0) %>% 
    rename(iso2c=country) %>% 
    left_join(countrycode::codelist %>% select(iso2c, country.name.en), by='iso2c') %>% 
    rename(country=country.name.en)

# plot
plot <- 
ggplot(data = transit_cost_1, aes(y = real_cost_n, x = length)) +
    geom_point(aes(size = time_comp), alpha = 0.5) +
    geom_smooth(col='snow1') +
    scale_y_log10(labels = scales::dollar) +
    ggtext::geom_richtext(data = transit_cost_1 %>% filter(length >
            100, time_comp>0), aes(label = str_wrap(str_trim(paste0(line, ' in **', city, '**: <span>', country, '<br>From: ***', start_year, '-' , end_year, '***')), 10)), hjust=1.05, size=2.5, family='Calibri') +
    geom_text(aes(150,1300, label="Transit Costs"), size=18, family='Monotype Corsiva', col='grey70')+
    annotation_logticks(sides = 'l') +
    scale_size_area(max_size = 10) +
    labs(size="Completion period",
        x = 'Length',
        y = 'Real Cost in Millions of USD',
        caption = "Graphic:#TidyTuesday week 2\nGitHub: @johnmutiso\nData:Transit Costs Project")+
    scale_x_continuous(labels = scales::unit_format(suffix = 'km')) +
    theme_minimal() +
    theme(legend.position = c(0.4, 0.2),
        axis.title = element_text(size = 14, color='grey60'))
            
# save plot
ggsave(
    plot = plot,
    height = 8,
    width = 10,
    dpi = 300,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)


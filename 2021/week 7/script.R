#' -------------------------------------------------------------------------------
#' Wealth and income over time
#' The data this week comes from [Urban Institute ](https://apps.urban.org/features/wealth-inequality-charts/) and [US Census](https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html). 
#' This [Urban.org](https://apps.urban.org/features/wealth-inequality-charts/) talks about Wealth and income over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 7
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)


# plot
top_bottom_bracket <- c('Under $15,000', '$200,000 and over')
plot <- 
    income_distribution %>%
    mutate(
        income_bracket_new = factor(case_when(
            income_bracket %in% 'Under $15,000' ~ 1,
            income_bracket %in% '$200,000 and over' ~ 2,
            TRUE ~ 3
        ), labels = c('Under $15,000', '$200,000 and over', 'Others') )
    ) %>% 
    ggplot() +
    geom_line(aes(year, income_distribution, col=income_bracket_new, size =income_bracket_new , group = income_bracket), show.legend = F)+
    facet_wrap(race ~ . , ncol = 2) +
    scale_color_manual(values = c('#f27373', '#958bd8', '#c0c0c0')) +
    scale_size_manual(values = c(1.5, 1.5, 0.3)) +
    scale_x_continuous(n.breaks = 6) +
    scale_y_continuous(labels = scales::unit_format(suffix = "%"))+
    labs(title="Household Income Over Time", 
        subtitle = 'Highlights on <b style="color:#f27373">Under $15,000</b> and <b style="color:#958bd8">$200,000 and over</b>',
        x="\nYear",
        y='Percent',
        caption = "Graphic: #TidyTuesday week 7\nData: Urban Institute and the US Census\nGitHub: @Jjohnmutiso")+
    theme_minimal() +
    theme(strip.text = element_text(size = 16, face='bold', color='grey60'), 
        text=element_text(size=13),
        plot.title = element_text(size=25, hjust = 0.5, face = 'bold'),
        plot.subtitle = ggtext::element_markdown(size = 12, hjust = 0.5, face = 'bold'))
# save plot
ggsave(
    plot = plot,
    height = 11,
    width = 8,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

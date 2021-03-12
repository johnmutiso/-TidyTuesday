#' -------------------------------------------------------------------------------
#' Employed Status
#' The data this week comes from [BLS](https://www.bls.gov/cps/tables.htm#charemp_m)  
#' This [BLS](https://www.bls.gov/careeroutlook/2018/article/blacks-in-the-labor-force.htm) talks about Employed Status over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 9
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

# plot
plot <- 
  earn %>% 
    group_by(race, age, sex, year) %>% 
    summarise(median=median(median_weekly_earn)) %>% 
    filter(age %in% c('16 to 24 years', '25 to 54 years', '55 years and over')) %>%
    mutate(agegr=factor(case_when(str_detect(age, '16')~1,
      str_detect(age, '25')~2,
      str_detect(age, '55')~3), levels=1:3, labels = c('16-24','25-54','>54'), ordered = T)) %>% 
  ggplot() +
    geom_point(aes(agegr, race, size=median, fill=median, col=median), pch=21) +
    facet_grid(sex~year) +
    scale_fill_viridis_c(aesthetics = c('color','fill')) +
    scale_x_discrete(guide = guide_axis(angle = 90))+
    scale_size_area(max_size = 8, breaks=c(600,800,1200,1500), labels=rep('',4)) +
    labs(size='', fill='', col='', x='Age Groups (years)' , y='Race',
      title = '\nU.S. Employment Weekly Median Earnings ($)\n    by Age group & Race\n',
      caption = 'Data: U.S. BUREAU OF LABOUR STATISTICS\nGraphic: #TidyTuesday week 9\nGitHub: @johnmutiso\n') +
    theme_minimal() +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(1.4, 'cm'),
      legend.box = 'vertical',
      legend.spacing = unit(0.0001, 'cm'),
      legend.justification = 0,
      legend.text.align = 1,
      text = element_text(size = 15, face = 'bold'),
      plot.background = element_rect(fill = 'grey85'),
      plot.caption = element_text(size=10, color='grey50')
    )
  
# save plot
ggsave(
    plot = plot,
    height = 7,
    width = 14,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

#' -------------------------------------------------------------------------------
#' Dubois Challenge - #
#' The data this week comes from [Anthony Starks, Allen Hillery Sekou Tyler](https://twitter.com/ajstarks, https://twitter.com/AlDatavizguy/status/1358454676497313792?s=20, https://twitter.com/sqlsekou/status/1360281040657522689?s=20)  
#' This [Medium](https://medium.com/nightingale/recreating-w-e-b-du-boiss-data-portraits-87dd36096f34) talks about Dubois Challenge over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 8
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

# plot
# Georgoa population
gp_plot <- 
georgia_pop %>% 
  pivot_longer(cols = 2:3, names_to='grp', values_to='pop_per') %>% 
  mutate(Year=str_c(Year, ' '),
    grp = str_to_upper(grp)) %>% 
  ggplot()+
  geom_line(aes(y=pop_per, x=factor(Year), group=grp, lty=grp), size=0.6) +
  scale_y_continuous(trans = 'reverse', expand = c(0,0), breaks = seq(100,0, by=-5))+
  scale_x_discrete(expand=c(0,0))+
  scale_linetype_manual(values = c(1,5),labels=c(paste0('= COLORED',str_flatten(rep("   ", 14)) ,'WHITE='), ''))+
  coord_flip() +
  labs(y='PERCENTS',
    x="",
    lty="")+
  theme_bw() +
  theme(axis.ticks = element_blank(),
    axis.text.y = element_text(size = 12, color='grey65', family = 'Verdana'),
    axis.text.x = element_text(size = 9, color='grey65', family = 'Verdana'),
    axis.title.x = element_text(color='grey65', size = 10),
    panel.grid.major = element_line(size=0.7, color="#ffded7"),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom', legend.key.width = unit(3.2, 'cm'))

plot <-
  gp_plot + plot_annotation(
    title = "\nCOMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA.\n\n",
    caption = 'Graphic: #TidyTuesday week 8|#DuBoisChallenge\nData: By Anthony Starks(@ajstarks)|Allen Hillery (@AlDatavizguy)|Sekou Tyler(@sqlsekou)\nGithub: @johnmutiso',
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        family = 'Verdana',
        size = 16,
        face = 'bold'
      ),
      plot.title.position = 'plot', 
      plot.caption.position = 'plot',plot.margin=unit(c(0,2.5,0,2.5),'cm'), aspect.ratio = 0.5
    )
  )

# save plot
ggsave(
    plot = plot,
    height = 9.5,
    width = 8,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

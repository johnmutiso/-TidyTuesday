#' -------------------------------------------------------------------------------
#' Ninja Warrior
#' The data this week comes from [Data.World](https://data.world/ninja/anw-obstacle-history) 
#'   and originally from [sasukepedia](https://sasukepedia.fandom.com/wiki/List_of_American_Ninja_Warrior_obstacles). 
#' This [sasukepedia](https://sasukepedia.fandom.com/wiki/List_of_American_Ninja_Warrior_Obstacles_(Description)) talks about SUBJECT TITLE in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 51
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

ninja_warrior <- 
    ninja_warrior %>% 
    group_by(obstacle_name) %>% 
    mutate(count_n=n(), min_order=min(obstacle_order), max_order=max(obstacle_order)) %>% 
    ungroup()

# plot
plot <- 
ninja_warrior %>% 
ggplot(aes(x=obstacle_order, y=reorder(obstacle_name, max_order))) +
    geom_point(aes(x=max_order, size=max_order), pch=23, fill='#5ac18e', stroke=0, show.legend = F) +#max_order
    geom_point(aes(x=min_order, size=max_order), pch=23, fill='#5ac18e', stroke=0, show.legend = F) +#min_order
    geom_linerange(aes(xmin=min_order, xmax=max_order,  size=max_order), col='#cbcba9', show.legend = F) +
    geom_text(aes(label=obstacle_name, x= 0, size=max_order), hjust=1, show.legend = F) +
    scale_x_continuous(limits = c(-10,10), breaks = seq(0,10, by=2.5)) +
    facet_wrap(~season, scales = 'free', nrow = 2, labeller = label_both) +
    labs(x = '\nMin - Max Obstacle order',
        y = 'Obstacle Name\n(Decreasing Max order #)',
        title = 'Ninja Warrior', subtitle = "Obstacle Ordering by Season",
        caption = "Graphic: #TidyTuesday week 51\nGitHub: @johnmutiso\nData: Data.World and originally from sasukepedia") +
    theme_bw() +
    theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        strip.text = element_text(size=30),
        axis.text.x = element_text(size = 20, family = 'Calibri'),
        axis.title = element_text(size=30, family = 'Calibri Light'), 
        panel.background = element_rect(color='snow1'),
        plot.title = element_text(size=50, family='Arial', hjust = 0.5, face='bold', color='grey50'),
        plot.subtitle = element_text(size = 25, hjust = 0.5),
        plot.caption = element_text(size=20, color = 'grey50'))



# save plot
ggsave(
    plot = plot,
    height = 25,
    width = 25,
    dpi = 350,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2020/week ', week_num)
)


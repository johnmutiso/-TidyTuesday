#' -------------------------------------------------------------------------------
#' Superbowl commercials
#' The data this week comes from [FiveThirtyEight](https://github.com/fivethirtyeight/superbowl-ads)
#' This [FiveThirtyEight](https://github.com/fivethirtyeight/superbowl-ads) talks about Superbowl commercials over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 10
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(patchwork)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)

# plot ----
# plot <-
set.seed(10)
colors <- randomcoloR::distinctColorPalette(k = 10)
vars <-
  c("like_count",
    "dislike_count",
    "favorite_count",
    "comment_count")
titles <- c('Likes', 'Dislikes', 'Favourite', 'Comments')
y_text <- c(0.01, 0.002, 0.003, 0.003)


for (i in 1:length(vars)) {
  ytitle <- paste0('Percent ', titles[i])
  plot <-
    youtube %>%
    mutate(prop = get(vars[i]) / view_count) %>%
    mutate_at(.vars = 5:11, as_factor) %>% 
    mutate_at(.vars = 5:11, fct_relevel, 'TRUE') %>% 
    group_by(brand, year) %>%
    arrange(brand, year, desc(prop)) %>%
    filter(row_number() == 1) %>%
    ggplot(aes(as.factor(year), prop)) +
    geom_text(aes(x=1, y= 0), 
      label=titles[i], size=16, col='grey80', fontface='bold', vjust=1, hjust=0, angle=90)+
    geom_line(
      aes(group = brand, col = brand),
      size = 1.3,
      show.legend = ifelse(i %in% c(2:4), F, T), alpha=0.8
    ) +
    geom_point(
      pch = 21,
      size = 3.5,
      stroke = 1.5,
      aes(fill = funny, col = brand),
      show.legend = ifelse(i %in% c(2:4), F, T), alpha=0.8
    ) +
    scale_x_discrete(guide = guide_axis(check.overlap = T))+
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c('lightblue', 'red2')) +
    labs(
      x = '',
      y = ytitle,
      fill = 'Funny',
      color = 'Brand'
    ) +
    scale_color_manual(values = colors) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = 'snow1'),
      text = element_text(size = ifelse(i %in% 1, 17, 11), family = 'Tahoma'),
      axis.title.y = element_text(face = 'bold')
    )
  
  assign(paste0('plot_', vars[i]), plot)
  
}

plot <- 
  (plot_like_count) / (plot_comment_count | plot_dislike_count) +
  plot_annotation(title = "Superbowl commercials", 
    subtitle = "Proportion of viewers who Liked, Commented or Disliked Ads\n    (Best* Ad per Brand Per year)\n       **For Disklikes - Best is Highest proportion of Dislikes",
    caption = "Graphic: #TidyTuesday week 10\nData:FiveThirtyEight\nGitHub: @johnmutiso",
    theme = theme(plot.title = element_text(size=35, family = 'Tahoma', face = 'bold', color='darkblue'),
      plot.subtitle = element_text(size=22, family = 'Arial', color='grey70'),
      plot.caption = element_text(size=14, color='grey70'))) 

# save plot ----
ggsave(
  plot = plot,
  height = 12,
  width = 16,
  dpi = 500,
  device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

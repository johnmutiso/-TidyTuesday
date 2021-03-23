#' -------------------------------------------------------------------------------
#' Video Games and Sliced
#' The data this week comes from [Steam by way of Kaggle originally from SteamCharts](https://www.kaggle.com/michau96/popularity-of-games-on-steam)
#' This [article_source](source_link) talks about Video Games and Sliced in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 12
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)


# plot
plot <-
games %>%
  left_join(data.frame(month = month.name, month_id = 1:12), by = 'month') %>%
  mutate(
    ap_perc = as.numeric(str_remove(avg_peak_perc, '%')),
    month2 = factor(
      month_id,
      ordered = T,
      levels = 1:12,
      labels = month.name
    )
  ) %>%
  group_by(year, month2) %>%
  summarise(avg_peak_per = mean(ap_perc, na.rm = T),
            sd = sd(ap_perc, na.rm = T)) %>%
  arrange(year, month2) %>%
  group_by(month2) %>%
  mutate(
    y_txt = ifelse(row_number() == n(), avg_peak_per, NA),
    x_txt = ifelse(row_number() == n(), year, NA)
  ) %>%
  ggplot(aes(year, avg_peak_per)) +
  geom_line(aes(group = month2, col = month2),
            size = 2,
            show.legend = F) +
  geom_point(
    pch = 21,
    aes(col = month2),
    size = 2.5,
    stroke = 2.5,
    fill = '#088da5',
    show.legend = F
  ) +
  geom_text(
    aes(x_txt, label = paste0("  ", month2), col = month2),
    size = 4.5,
    hjust = 0,
    fontface = 'bold',
    show.legend = F
  ) +
  scale_x_continuous(
    expand = c(0, 0.2),
    breaks = seq(2012, 2021, by = 1),
    limits = c(2012, 2022)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_color_brewer(palette = 'Paired') +
  labs(
    x = 'Year',
    y = 'Grand average of\n(Average # of players/Peak)',
    title = "Video Games and Sliced",
    subtitle = "   Grand Montly Average for the proportion of mean number\n     of video game players by Peak ",
    caption = "Graphic: #TidyTuesday week 12\nData: Steam by way of Kaggle\nOriginally: from steamcharts.com\nGitHub: @johnmutiso"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = '#088da5'),
    panel.grid = element_line(color = '#6897bb'),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16, color = 'snow3'),
    axis.text = element_text(color = 'snow2', face = 'bold'),
    plot.title = element_text(size = 30, color = 'snow2', face = 'bold'),
    plot.subtitle = element_text(size = 22)
  )

# save plot
ggsave(
  plot = plot,
  height = 8,
  width = 12,
  dpi = 500,
  device = 'png',
  filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

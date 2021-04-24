#' -------------------------------------------------------------------------------
#' Netflix Shows
#' The data this week comes from [Kaggle](https://www.kaggle.com/shivamb/netflix-shows?select=netflix_titles.csv)
#' This [article_source](source_link) talks about Netflix Shows over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 17
data_source <- "Kaggle w/ credit to Shivam Bansal"
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(extrafont)
library(patchwork)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)

netflix_titles1 <-
  netflix_titles %>%
  mutate(
    add_year = year(as_date(date_added, format = "%B%d,%Y")),
    type = paste0(type, 's'),
    time_diff = add_year - release_year
  )
# plot
plot1 <-
  netflix_titles1 %>%
  filter(!is.na(rating)) %>%
  group_by(type, add_year, rating) %>%
  summarise(n = n()) %>%
  filter(!is.na(add_year)) %>%
  ggplot(aes(as.factor(add_year), rating)) +
  geom_tile(aes(fill = n), linejoin = 'mitre', col = 'grey70') +
  facet_wrap( ~ type, ncol = 1) +
  scale_x_discrete(guide = guide_axis(check.overlap = T)) +
  scale_fill_gradient(low = 'snow1', high = 'red3') +
  labs(
    x = "",
    y = "Rating",
    fill = "Number of TV Shows\n/Movies Added",
    caption = paste0(
      "**Graphic:** #TidyTuesday week",
      week_num,
      "<br> **Data:** ",
      data_source,
      "<br> **GitHub:** @johnmutiso"
    )
  )  +
  theme_minimal(base_family = 'Tahoma', base_size = 13) +
  theme(
    plot.caption = ggtext::element_markdown(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'snow'),
    legend.position = 'bottom',
    axis.line = element_line(size = 0.8),
    axis.text = element_text(size = 12, family = 'Tahoma'),
    strip.placement = 'outside',
    legend.key.width = unit(1.5, 'cm'),
    strip.text = element_text(
      size = 20,
      color = 'red4',
      face = 'bold',
      family = 'Tahoma'
    )
  )

# plot2
plot2 <-
  netflix_titles1 %>%
  filter(!is.na(rating)) %>%
  ggplot(aes(y = time_diff, rating)) +
  geom_point(
    aes(col = type),
    position = position_jitter(width = 0.4, seed = 66),
    size = 1.6,
    alpha = 0.5
  ) +
  ggtext::geom_textbox(
    data = . %>% filter(time_diff > 90),
    aes(label = paste0('**', title, '** <br>', description)),
    hjust = -0.3,
    vjust = 0.9,
    width = unit(7, 'cm'),
    family = 'Arial',
    fill=NA
  ) +
  geom_curve(
    data = NULL,
    aes(
      x = 8.6,
      xend = 7.5,
      y = 89,
      yend = 93
    ),
    arrow = arrow(length = unit(0.4, 'cm')),
    curvature = 0.4,
    size = 1
  ) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous(n.breaks = 8,
                     limits = c(-4, 100),
                     expand = c(0, 0)) +
  scale_color_manual(values = c('red3', 'darkgreen')) +
  labs(fill = '',
       y = "Time from release to when\nadded in Netflix (years)",
       x = "Rating",
       col = "") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = 'snow'),
    axis.title = element_text(size = 16, color = 'grey60'),
    axis.line = element_line(size = 0.8),
    axis.text = element_text(size = 14, family = 'Tahoma'),
    legend.position = c(0.2, 0.8),
    legend.text = element_text(size = 14)
  )


plot3 <-
  plot2 / plot1 + plot_annotation(title = "Netflix Shows",
                                  theme = theme(
                                    plot.title = element_text(
                                      size = 40,
                                      family = 'Felix Titling',
                                      color = 'red3',
                                      face = 'bold',
                                      hjust = 0.5
                                    )
                                  ))
# save plot
ggsave(
  plot = plot3,
  height = 14,
  width = 9,
  dpi = 500,
  device = 'png',
  filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')

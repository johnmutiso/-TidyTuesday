#' -------------------------------------------------------------------------------
#' Makeup Shades
#' The data this week comes from [The Pudding](https://github.com/the-pudding/data/tree/master/foundation-names)
#' This [pudding.cool](https://pudding.cool/2021/03/foundation-names/) talks about Makeup Shades over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 14
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)

# plot
text_labs <-
  allShades %>% filter(hue > 100) %>%
  mutate(xend = hue - 30, yend = sat - 0.3)

plot <-
  allShades %>%
  ggplot(aes(hue, sat)) +
  geom_curve(
    data = text_labs,
    aes(
      x = hue,
      xend = xend,
      y = sat,
      yend = yend
    ),
    curvature = 0.4,
    arrow = arrow(length = unit(0.15, 'in'), type = 'closed'),
    size = 1,
    col = '#fc7572'
  ) +
  geom_point(
    col = allShades$hex,
    show.legend = F,
    alpha = allShades$lightness,
    size = 3
  ) +
  ggtext::geom_textbox(
    data = text_labs,
    aes(xend, yend, label = str_squish(
      paste0(
        '**Brand:** ',
        brand,
        '<br>**Product:** ',
        product,
        '<br>**Descr:** ',
        description
      )
    )),
    hjust = 0.5,
    family = 'Arial',
    size = 3,
    vjust = 1,
    col = 'grey50'
  ) +
  theme_minimal() +
  labs(
    x = 'Hue',
    y = 'Saturation',
    title = 'Makeup Shades',
    subtitle = 'The Balance between Hue and Saturation',
    caption = "**Graphic:** #TidyTuesday week 14<br>**Data:** The Pudding<br>**GitHub:** @johnmutiso"
  ) +
  theme(
    plot.background = element_rect(fill = 'snow3'),
    text = element_text(color = 'snow', size = 14),
    plot.title = element_text(
      size = 25,
      face = 'bold',
      color = 'snow1',
      family = 'Tahoma'
    ),
    plot.subtitle = element_text(
      size = 16,
      family = 'Tempus Sans ITC',
      color = 'grey30'
    ),
    plot.caption = ggtext::element_markdown(
      halign = 0,
      color = 'grey50',
      size = 9
    )
  )

# save plot
ggsave(
  plot = plot,
  height = 5,
  width = 9,
  dpi = 500,
  device = 'png',
  filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')
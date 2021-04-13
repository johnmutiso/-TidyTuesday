#' -------------------------------------------------------------------------------
#' Deforestation
#' The data this week comes from [Our World in Data](https://ourworldindata.org/forests-and-deforestation)
#' This ["Forests and Deforestation". Published online at OurWorldInData.org](https://ourworldindata.org/forests-and-deforestation) talks about Deforestation over time in greater detail.Additional Article is in [ucsdnews.ucsd.edu/](https://ucsdnews.ucsd.edu/feature/deforestation-drives-disease-climate-change-and-its-happening-at-a-rapid-rate)
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 15
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(patchwork)
loadfonts(device = 'win')


# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)

#continents
cont <- countrycode::codelist %>%
  select(iso3c, continent) %>%
  rename(code = iso3c)


forest_area1 <-
  forest_area %>%
  filter(!code %in% c(NA, 'OWID_WRL')) %>%
  pivot_wider(
    id_cols = 1:2,
    names_from = year,
    values_from = forest_area,
    names_prefix = 'yr'
  )

#calculate change by year
chg <- function(data, year, span) {
  yr1 <- paste0('yr', year - span)
  yr2 <- paste0('yr', year)
  
  chg <- (data[, yr2] / data[, yr1]) - 1
  names(chg) <- paste0('chg', year)
  chg
}

# 5-year change in forest cover
years <- seq(1995, 2020, by = 5)
forest_areaCHG <- forest_area1[, 1:2]
for (yr in years) {
  forest_areaCHG <-
    bind_cols(forest_areaCHG, chg(forest_area1, yr, 5))
}

# forest cover change from 1995 to 2020
CHG95_20 <-
  forest_area %>%
  filter(!code %in% c(NA, 'OWID_WRL'), year %in% c(1995, 2020)) %>%
  pivot_wider(
    id_cols = 1:2,
    names_from = year,
    values_from = forest_area,
    names_prefix = 'yr'
  ) %>%
  mutate(chg = (yr2020 / yr1995) - 1) %>%
  filter(!is.na(chg)) %>%
  left_join(cont, by = 'code')

# plot: forest cover change from 1995 - 2020
plot1 <-
  CHG95_20 %>%
  ggplot(aes(reorder(code, desc(chg)), chg)) +
  geom_col(aes(fill = continent), alpha = 0.7) +
  geom_text(
    aes(label = paste0(' ', entity), col = continent),
    angle = 90,
    vjust = 0.3,
    hjust = if_else(CHG95_20$chg < 0, 1, 0),
    family = if_else(CHG95_20$code == 'KEN', 'Segoe Script', 'Tahoma'),
    size = 3,
    alpha = if_else(CHG95_20$code == 'KEN', 0.8, 0.65),
    show.legend = F
  ) +
  ggtext::geom_textbox(aes(x=65,y=-0.25, label="Change in Global Forest Cover(%)"), size=30, fontface='bold', hjust=0.5,vjust=0.5, width = unit(25,'cm'), col="#5ac18e", fill='#696966', family='Niagara Engraved')+
  labs(fill = '', x = "", y = "% Change in Forest cover(%) from 1995-2020",
       caption = "**Graphic:** #TidyTuesday week 15 <br> **Data:** ourworldindata.org <br> **GitHub:** @johnmutiso") +
  scale_color_brewer(palette = 'Set1', aesthetics = c('fill', 'color')) +
  scale_y_continuous(
    n.breaks = 8,
    labels = scales::percent,
    limits = c(-0.45, 1.1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = c(0.1, 0.9),
    plot.background = element_rect(fill = 'snow2'),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 17, color = 'grey70', face = 'bold'),
    plot.caption = ggtext::element_markdown()
  )

# scatter plot of 5-year forest cover change from 1990-2020
plot2 <-
  forest_areaCHG %>%
  pivot_longer(cols = -c(1:2),
               names_to = 'year',
               values_to = 'chg') %>%
  mutate(year = as.numeric(str_remove(year, 'chg'))) %>%
  left_join(cont, by = 'code') %>%
  ggplot(aes(year, chg)) +
  geom_line(
    aes(group = entity, col = continent),
    size = 1.1,
    alpha = 0.7,
    show.le = F
  ) +
  geom_point(
    aes(col = continent),
    pch = 21,
    size = 2,
    stroke = 1.1,
    fill = 'snow1',
    alpha = 0.7,
    show.legend = F
  ) +
  geom_text(
    aes(label = paste0(entity, '  '), col = continent),
    hjust = 1,
    vjust = 0.3,
    size = 3,
    show.legend = F,
    check_overlap = T,
    alpha = 0.8,
    family = 'Arial'
  ) +
  scale_color_brewer(palette = 'Set1', aesthetics = c('fill', 'color')) +
  scale_x_continuous(limits = c(1993, 2020),
                     breaks = seq(1995, 2020, by = 5)) +
  scale_y_continuous(n.breaks = 8,
                     labels = scales::percent) +
  labs(x = '', y = '5-year change in % forest cover') +
  theme_minimal() +
  theme(
    text = element_text(size = 14, color = 'grey70', face = 'bold'),
    plot.background = element_rect(color = 'snow', fill = 'snow1', size = 7)
  )


# fnal plot
plotf <-
  plot1 + inset_element(plot2, 0.36, 0.5, 1, 1)

# save plot
ggsave(
  plot = plotf,
  height = 10,
  width = 20,
  dpi = 400,
  device = 'png',
  filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')

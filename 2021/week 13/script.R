#' -------------------------------------------------------------------------------
#' UN Votes
#' The data this week comes from [Data_source](https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379)
#' This [Mine Çetinkaya-Rundel wrote about the process](http://www.citizen-statistician.org/2021/03/open-source-contribution-as-a-student-project/) talks about UN Votes over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 13
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)

rollcall_votes <- unvotes %>%
  left_join(roll_calls, by = 'rcid') %>%
  mutate(year = lubridate::year(date))

# plot
color <- RColorBrewer::brewer.pal(3, 'Dark2')

plot <-
  rollcall_votes %>%
  group_by(year) %>%
  mutate(n_year = n()) %>%
  group_by(year, vote) %>%
  summarise(
    prop_vote = n() / n_year,
    std_error = sqrt((prop_vote * (1 - prop_vote)) / n_year),
    ul = prop_vote + (1.96 * std_error),
    ll = prop_vote - (1.96 * std_error)
  ) %>% distinct() %>%
  mutate(y_t = ifelse(year == 2019, prop_vote, NA)) %>%
ggplot(aes(year, prop_vote, col = vote)) +
  geom_line(size = 1, show.legend = F) +
  geom_point(
    pch = 21,
    size = 1.2,
    stroke = 1,
    fill = 'snow1',
    show.legend = F
  ) +
  geom_text(
    aes(y = y_t, label = paste0("  ", str_to_upper(vote))),
    family = 'Calibri',
    fontface = 'bold',
    hjust = 0,
    show.legend = F,
    size=3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(
    guide = guide_axis(check.overlap = T),
    n.breaks = 20,
    limits = c(1942, 2021),
    
  ) +
  scale_color_manual(values = color, aesthetics = 'color') +
  labs(
    y = 'Proportion of UN Votes',
    x = 'Year',
    title = 'UN Votes',
    subtitle = 'Change in Proportion of UN Votes(<span style="color:#7570B3">YES</span>, <span style="color:#1B9E77">ABSTAIN </span>&<span style="color:#D95F02"> NO </span>) over time',
    caption = "**Graphic:** #TidyTuesday week 13<br>**Data:** Harvard's Dataverse - <br>**Originally** from Erik Voeten (2013)<br>**GitHub:** @johnmutiso"
  ) +
  theme_bw() +
  theme(
    plot.subtitle = ggtext::element_markdown(face = 'bold', size = 10, color = 'grey30'),
    plot.title = element_text(size = 22, face = 'bold', color = 'grey60'),
    text = element_text(size = 10, color = 'grey20'),
    plot.caption = ggtext::element_markdown(color = 'grey70', halign = 0,size=7)
  )

# save plot
ggsave(
  plot = plot,
  height = 4,
  width = 8.2,
  dpi = 500,
  device = 'png',
  filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

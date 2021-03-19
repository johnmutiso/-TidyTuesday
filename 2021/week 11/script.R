#' -------------------------------------------------------------------------------
#' Bechdel Test
#' The data this week comes from [FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/bechdel)
#' This [FiveThirtyEight](https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/) talks about Bechdel Test over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 11
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year = 2021)

# data
movies_new <-
  movies %>%
  mutate(
    runtime_n = str_extract(runtime, '\\d+') %>% as.numeric(),
    year = as.numeric(year),
    year_cat = factor(
      case_when(
        year < 1985 ~ 1,
        between(year, 1985, 1989) ~ 2,
        between(year, 1990, 1994) ~ 3,
        between(year, 1995, 1999) ~ 4,
        between(year, 2000, 2004) ~ 5,
        between(year, 2005, 2009) ~ 6,
        between(year, 2010, 2013) ~ 7
      ),
      ordered = T,
      levels = 1:7,
      labels = c(
        '1970-1984',
        '1984-1989',
        '1990-1994',
        '1995-1999',
        '2000-2004',
        '2005-2009',
        '2010-2013'
      )
    )
  )

# subgroup sample sizes
ns <-
  movies_new %>%
  group_by(year_cat, binary) %>%
  count() %>%
  pivot_wider(id_cols = 1,
    names_from = 2,
    values_from = 3) %>%
  mutate(ns = paste0('n:   ', FAIL, '   ', PASS, '       '))


# plot
plot <-
  movies_new %>%
  left_join(ns, by = 'year_cat') %>%
  ggplot(aes(y = budget_2013, x = ns, fill = binary)) +
  geom_boxplot(col = 'grey40', show.legend = F) +
  scale_y_log10(label = scales::dollar_format(scale = 0.000001, accuracy = 0.01)) +
  scale_fill_manual(values = c('#ff6666', '#6897bb')) +
  ggpubr::stat_compare_means(
    hide.ns = F,
    label.y = 4,
    label.x = 1.2,
    label = 'p.signif',
    vjust = 0 ,
    color = 'grey60',
    method = 'wilcox.test',
    fontface = 'bold',
    size = 6
  ) +
  facet_wrap(year_cat ~ ., nrow = 1, scales = 'free_x') +
  labs(
    title = 'Bechdel Test',
    y = '2013-Normalized Budget\nin Millions(Log10)',
    x = '',
    subtitle = "Comparisons of 2013-Normalized Budgets for Movies that <span style='color:#6897bb'>Pass</span> or <span style='color:#ff6666'>Fail</span> the Bechdel Test",
    caption = "Wilcoxon Test: ~ ns: not significant\n\n\nGraphic: #TidyTuesday week 11\nData: FiveThirtyEight\nGitHub: @johnmutiso"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    panel.border = element_rect(color = 'grey70'),
    strip.background = element_rect(fill = 'grey70', color = 'grey70'),
    strip.text = element_text(
      size = 15,
      color = 'snow1',
      face = 'bold',
      family = 'Tahoma'
    ),
    plot.title = element_text(
      size = 30,
      family = 'Arial',
      color = 'grey70',
      face = 'bold'
    ),
    plot.subtitle = ggtext::element_markdown(size = 16, color = 'grey35', face =
        'bold'),
    axis.text.x = element_text(
      size = 12,
      face = 'bold',
      family = 'Arial'
    ),
    plot.caption = element_text(color = 'grey70')
  )

# save plot
ggsave(
  plot = plot,
  height = 7.5,
  width = 12,
  dpi = 500,
  device = 'png',
  filename = paste0('week', week_num, 'plot.png'),
  path = paste0('2021/week ', week_num)
)

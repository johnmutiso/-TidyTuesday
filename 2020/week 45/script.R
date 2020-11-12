#' -------------------------------------------------------------------------------
#' IKEA Furniture
#' The data this week comes from [Kaggle](https://www.kaggle.com/ahmedkallam/ikea-sa-furniture-web-scraping).
#' This [FiveThirtyEight](https://fivethirtyeight.com/features/the-weird-economics-of-ikea/) talks about IKEA Furniture in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 45
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

# plot
plot <-
ikea %>%
    mutate(category = str_replace_all(category, '& ', '&\n')) %>%
    ggplot(aes(y = price, x = other_colors, col = other_colors)) +
    geom_boxplot(size = 1) + 
    geom_jitter(alpha = 0.5, size = 0.3) +
    ggpubr::stat_compare_means(
        hide.ns = F,
        label.y = 4,
        label.x = 1.2,
        label = 'p.signif',
        vjust = 0 ,
        color = 'grey35'
    ) +
    coord_flip() +
    facet_grid(category ~ .) +
    scale_y_log10() +
    scale_color_manual(values = c('#696969', '#f08080')) +
    labs(y = "Price (Log<sub>10</sub>)",
         title = '<span style="color:#ffa500;font:times">IKEA</span><span style="color:grey80"> Furniture',
         subtitle = '<br><span style="color:#dcedc1">Does price <span style="color:#b0e0e6">significantly</span> differ by color when<br>other colors are  <span style="color:#f08080">AVAILABLE</span> for the product or <span style="color:#696969">NOT</span>?</span>',
         caption = 'ns-(Not Significant)   *(<0.05)   **(<=0.01)   ***(<=0.001)   ****(<=0.0001)\n\nCredit: @johnmutiso_\nData: Kaggle Originally from IKEA\nGraphic: #TidyTuesday week 45') +
    theme_minimal(base_family = 'Tahoma', base_size = 14, base_rect_size = 2) +
    theme(
        strip.placement = 'inside',
        strip.text.y = element_text(
            angle = 0,
            hjust = 0,
            size = 12,
            color = 'grey90',
            face = 'bold'
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = 'grey90', size = 14),
        axis.title.x = ggtext::element_markdown(size = 15, color = 'grey60', face = 'bold'),
        strip.background.y = element_rect(fill = 'grey60', colour = 'grey60'),
        strip.text = element_text(),
        legend.position = 'none',
        plot.background = element_rect(fill = '#0e2f44'),
        panel.background = element_rect(color = 'grey90'),
        plot.title = ggtext::element_markdown(size=25, face = 'bold'),
        plot.subtitle = ggtext::element_markdown(size = 16, face = 'bold', family = 'Calibri'),
        plot.caption = element_text(hjust = 0, color = 'grey80', size=10)
    )

# save plot
ggsave(
    plot = plot,
    height = 12,
    width = 6.2,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

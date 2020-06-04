#' -------------------------------------------------------------------------------
#' COCKTAILS
#' The data this week comes from [Kaggle](https://www.kaggle.com/ai-first/cocktail-ingredients, https://www.kaggle.com/jenlooper/mr-boston-cocktail-dataset).
#' This [FiveThirtyEight article](https://fivethirtyeight.com/videos/we-got-drunk-on-margaritas-for-science/) talks about Cocktails in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

#tuesdata <- tidytuesdayR::tt_load(2020, week = 22)

#readr::write_csv(tuesdata$cocktails, path = '2020/week 22/data/cocktails.csv')

cocktails <- readr::read_csv('2020/week 22/data/cocktails.csv')

plot <-
    cocktails %>%
    group_by(drink) %>%
    filter(n() > 6) %>%
    select(drink, category) %>%
    group_by(category) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(id = 1:n(),
           per = (n / sum(n)) * 100) %>%
    ggplot() +
    geom_bar(
        aes(x = 1, y = per),
        color = '#bada55',
        size = 2,
        stat = 'identity',
        position = 'stack',
        show.legend = F
    ) +
    geom_text(
        aes(
            x = 1,
            y = per - 0.1,
            label = paste0(category, ' [', round(per, 2), '%]')
        ),
        position = 'stack',
        col = 'snow',
        size = 5,
        fontface = 'bold',
        vjust = 1
    ) +
    labs(title = 'MORE THAN 6 INGREDIENTS\n[Drink Category (%)]',
         caption = 'Graphic:#TidyTuesday week 22|\nAuthor:@johnmutiso|Data:Kaggle|') +
    theme_void() +
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(
            hjust = 0.5,
            family = 'Courier New',
            face = 'bold',
            vjust = -1,
            size = 16
        ),
        plot.background = element_rect(fill = '#d3ffce'),
        plot.caption = element_text(family = 'Courier New')
    )

# save plot
ggsave(
    plot = plot,
    height = 8.8,
    width = 3.5,
    dpi = 500,
    device = 'png',
    filename = 'week22plot.png',
    path = './2020/week 22/'
)

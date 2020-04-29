#' Broadway Musicals
#' The data this week comes from [Playbill](https://www.playbill.com/grosses).
#' This [Alex Cookson](https://www.alexcookson.com/post/most-successful-broadway-show-of-all-time/)
#' talks about 'What's the most successful Broadway show of all time?' in greater detail.

#' Credit: [John Mutiso](twitter: @johnmutiso_)

# libraries ------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')
# data -----------------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2020, week = 18)
grosses <- tuesdata$grosses

# Shwo with highest weekly gross ---------------------------------------------------
max_gross <-
    grosses %>%
    arrange(desc(weekly_gross)) %>%
    filter(row_number() == 1)

# Plot -----------------------------------------------------------
plot <- 
  ggplot() +
    geom_point(
        data = grosses %>%
            filter(weekly_gross > 0 & pct_capacity > 0) ,
        aes(week_ending, weekly_gross, size = previews, col = pct_capacity),
        pch = 21
    ) +
    geom_vline(xintercept = as_date('2011-12-01'),
               col = 'white',
               lty = 3) +
    geom_hline(yintercept = 2400000,
               col = 'white',
               lty = 3) +
    geom_text(
        aes(x = as_date('2007-06-01'), y = 3300000),
        label = 'No previews for these =>\ntop shows\n(60.56% of them are\n`Hamilton Shows`)',
        col = '#dcedc1',
        family = 'Bradley Hand ITC',
        size = 6.3
    ) +
    geom_text(
        data = max_gross,
        aes(
            x = as_date('2013-12-01'),
            y = weekly_gross,
            label = paste0(show, ' show in\n', theatre)
        ),
        col = '#f3f45d',
        family = 'Gabriola',
        size = 5
    ) +
    geom_curve(
        data = max_gross,
        aes(
            x = as_date('2013-12-01'),
            xend = week_ending - 60,
            y = 3900000,
            yend = weekly_gross
        ),
        arrow = arrow(length = unit(0.1, 'in')),
        col = '#ffffff',
        size = 0.01,
        alpha = 0.7
    ) +
    scale_x_date(date_breaks = '3 year', date_labels = '%Y') +
    scale_y_continuous(labels = scales::dollar) +
    scale_size_area("Number of\nPreviews", max_size = 8) +
    scale_color_viridis_c("Theatre\ncapacity (%)", option = 'plasma') +
    labs(
        title = 'Is Hamilton Possibly the Best BROADWAY Show of All time???',
        caption = 'Github: @johnmutiso\nData: From Playbill via Alex Cookson [twitter: @alexcookson]\nGraphic: 2020-week 18 TidyTuesday'
    ) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = '#101010'),
        panel.grid = element_blank(),
        axis.text = element_text(color = '#f0f8ff', size = 12),
        legend.position = c(0.25, 0.8),
        legend.direction = 'horizontal',
        legend.key.width = unit(1.5, 'cm'),
        legend.box.background = element_rect(fill = '#cbcba9'),
        plot.title = element_text(family = 'Forte', size = 26, color = '#fea636'),
        plot.caption = element_text(color = '#dcedc1', size = 12, family = 'Papyrus')
    )

# Sace the plot ------------------------
ggsave(
    plot = plot,
    path = './2020/week 18/',
    width = 13.5,
    height = 8,
    dpi = 400,
    device = 'png',
    filename = 'week18plot.png'
)



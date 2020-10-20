#' -------------------------------------------------------------------------------
#' Beyoncé and Taylor Swift Lyrics
#' The data this week comes from [Rosie Baillie and Dr. Sara Stoudt.](https://twitter.com/Rosie_Baillie_, https://twitter.com/sastoudt).
#' This [Taylor Swift Lyrics](https://rpubs.com/RosieB/taylorswiftlyricanalysis) talks about Taylor Swift Lyrics in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 40
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)


charts <-
    charts %>%
    mutate(released_date = lubridate::as_date(released, format = '%B %d, %Y'))

# plot
plot <-
    charts %>%
    ggplot(aes(y = as.numeric(chart_position), x = released_date)) +
    geom_point(
        aes(fill = artist),
        pch = 21,
        size = 4,
        alpha = 0.7,
        stroke = 1.5,
        col = 'grey90',
        position = position_jitter(
            width = 100,
            height = 0,
            seed = 12
        ),
        show.legend = F
    ) +
    geom_smooth(aes(col = artist),
                method = 'lm',
                se = F,
                size = 2,
                show.legend = F) +
    geom_text(
        aes(label = chart, x = released_date + 100, col = artist),
        alpha = 0.6,
        family = 'Calibri',
        check_overlap = T,
        fontface = 'bold',
        position = position_jitter(
            width = 100,
            height = 0,
            seed = 12
        ),
        show.legend = F,
        hjust = 0
    ) +
    scale_y_log10() +
    #scale_y_continuous(limits = c(0, 85), breaks = seq(0, 80, by = 10)) +
    scale_x_date(breaks = unique(charts$released_date), date_labels = '%d/%b-\n%Y', guide = guide_axis(check.overlap = T)) +
    scale_color_manual(values = c('#407294','#f7347a'), aesthetics = c('colour', 'fill')) +
    labs(
        x = '\nChart Release\nDate',
        y = 'Highest Chart Position(log10)',
        title = '\n\n\n\nHighest Music Chart Position\n',
        subtitle = '<span style="color:#407294">Beyonce</p> & <span style="color:#f7347a">Taylor Swift</p>',
        caption = "Credit: @johnmutiso\nData:  Rosie Baillie(@Rosie_Baillie_) and Dr. Sara Stoudt.(@sastoudt)\nGraphic: #TidyTuesday Week 40\n\n\n\n"
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = '#dcedc1'),
        legend.position = c(0.8, 0.8),
        panel.border = element_rect(color = 'grey80', fill = NA, size = 2),
        plot.subtitle = ggtext::element_markdown(size = 18),
        plot.title = element_text(size = 20, face = 'bold', family = 'Calibri', color = 'grey60'),
        axis.text.x = element_text(face = 'bold', family = 'Tahoma', size = 7),
        axis.title = element_text(size = 14),
        plot.caption = element_text(color = 'grey50')
    )


# save plot
ggsave(
    plot = plot,
    height = 10,
    width = 7.5,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)


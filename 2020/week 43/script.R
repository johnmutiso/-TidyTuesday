#' -------------------------------------------------------------------------------
#' Great American Beer Festival Data
#' The data this week comes from [Great American Beer Festival](https://www.greatamericanbeerfestival.com/the-competition/winners/).
#' This [2019 GABF Medal Winner Analysis](https://www.brewersassociation.org/insights/gabf-medal-winners-analyzed-2019-edition/)
#'  talks about SUBJECT TITLE in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 43
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
    beer_awards %>%
    mutate(medal = fct_relevel(medal, 'Gold', 'Silver', 'Bronze')) %>%
    group_by(year, medal) %>%
    summarise(n = n()) %>%
    ggplot(aes(year, n)) +
    geom_line(aes(col = medal), size = 1.2, alpha = 0.8) +
    geom_point(
        aes(col = medal),
        size = 2.3,
        show.legend = F,
        stroke = 2,
        pch = 21,
        alpha = 0.8
    ) +
    scale_x_continuous(breaks = seq(1985, 2020, by = 5), expand = c(0, 1.3)) +
    scale_y_continuous(breaks = seq(0, 100, by = 20)) +
    scale_color_manual(values = c('#FFD700', '#C0C0C0', '#cd7f32')) +
    labs(
        title = '<span style="color:grey55">GREAT AMERICAN</p>',
        subtitle = '<span style="font-family: Algerian; color:grey85">BEER FESTIVAL</span><span style="color:grey90;font-size:28px"><sup><sup>®',
        color = 'Beer Awards',
        y = 'Number of Medals',
        caption = "\n\n\nCredit: @johnmutiso\nData: Great American Beer Festival (https://www.greatamericanbeerfestival.com/the-competition/winners/\nGraphic: #TidyTuesday Week 43"
    ) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = '#36454f', color = '#FFD758', size = 4),
        panel.grid = element_line(color = '#536878', size = 1 / 10),
        axis.text = element_text(
            size = 13,
            color = 'grey90',
            family = 'Arial'
        ),
        plot.title = ggtext::element_markdown(
            fill = NULL,
            face = 'bold',
            box.colour = 'white',
            size = 24
        ),
        plot.subtitle = ggtext::element_markdown(size = 28, colour = 'red'),
        axis.title = element_text(color = 'grey45', size =  16, face = 'bold'),
        legend.position = c(0.81, 0.19),
        legend.text = element_text(size = 10, color = 'grey90', face = 'bold'),
        legend.title = element_text(
            size = 18,
            family = 'Stencil',
            face = 'bold',
            color = 'grey75'
        ),
        plot.caption = element_text(color = 'grey90', size = 7)
    )
# save plot
ggsave(
    plot = plot,
    height = 5.5,
    width = 8,
    dpi = 400,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

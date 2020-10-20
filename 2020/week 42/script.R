#' -------------------------------------------------------------------------------
#' Datasaurus Dozen
#' The data this week comes from [Alberto Cairo](http://www.thefunctionalart.com/2016/08/download-datasaurus-never-trust-summary.html)
#'  via [Steph Locke + Lucy McGowan](https://twitter.com/SteffLocke, https://twitter.com/LucyStats).
#' This [Alberto Cairo blogpost](http://www.thefunctionalart.com/2016/08/download-datasaurus-never-trust-summary.html) talks about SUBJECT TITLE in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 42
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(patchwork)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

# Summary Statistics by data

summ <-
    datasaurus %>%
    group_by(dataset) %>%
    summarise(
        Mean_X = mean(x),
        Mean_Y = mean(y),
        SD_X = sd(x),
        SD_Y = sd(y),
        CORR_XY = cor(x, y)
    ) %>%
    mutate_if(.predicate = is.numeric,
              .funs = round,
              digits = 2)

# plot
plot <-
    datasaurus %>%
    ggplot(aes(x, y)) +
    geom_density2d(col = 'grey70') +
    geom_point(size = 1, col = '#ff0000') +
    facet_wrap( ~ dataset, ncol = 4) +
    coord_equal() +
    theme_minimal() +
    theme(
        strip.placement = 'outside',
        strip.text = element_text(
            angle = 0,
            face = 'bold',
            family = 'Comic Sans MS',
            size = 12
        ),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8, face = 'bold'),
        axis.title = element_blank(),
        plot.background = element_rect(color = 'grey70', size = 3)
    )



summ_grob <-
    gridExtra::tableGrob(
        summ,
        theme = gridExtra::ttheme_minimal(
            base_family = 'Comic Sans MS',
            base_size = 13,
            base_colour = 'grey60'
        )
    )

plot_final <-
    plot + summ_grob +
    plot_layout(widths = c(1.5, 1)) +
    plot_annotation(
        title = '\nDatasaurus Dozen',
        subtitle = '{Datasaurus shows us why *visualisation* is important, not just summary statistics}\n',
        theme = theme(
            plot.title = element_text(
                size = 30,
                face = 'bold',
                family = 'Comic Sans MS',
                color = '#065535'
            ),
            plot.subtitle = element_text(
                size = 20,
                family = 'Lucida Sans',
                color = '#f7347a'
            ),
            plot.caption = element_text(color = 'grey50', size = 10)
        ),
        caption = "\n\n\nCredit: @johnmutiso\nData: Alberto Cairo(http://www.thefunctionalart.com/2016/08/download-datasaurus-never-trust-summary.html)\ncourtesy of @SteffLocke + @LucyStats thru' Jesus M. Castagnetto\nGraphic: #TidyTuesday Week 42"
    )




# save plot
ggsave(
    plot = plot_final,
    height = 12,
    width = 14,
    dpi = 400,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

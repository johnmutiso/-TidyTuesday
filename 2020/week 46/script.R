#' -------------------------------------------------------------------------------
#' Historical Phone Usage
#' The data this week comes from [OurWorldInData.org.](https://ourworldindata.org/technology-adoption).
#' This [Hannah Ritchie (2017) - "Technology Adoption"](https://ourworldindata.org/technology-adoption)
#'  talks about Historical Phone Usage in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 46
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

# plot
plot <-
    function(data,
             var,
             ylab,
             subtitle,
             legend_pos = c(0.3, 0.85),
             countries = c('Kenya', 'South Africa', 'United States')) {
        data %>%
            ggplot(aes(year, {
                {
                    var
                }
            }, group = entity)) +
            geom_line(aes(col = continent), alpha = 0.4, size = 1.5) +
            geom_line(
                data = data %>% filter(entity %in% countries),
                aes(year, {
                    {
                        var
                    }
                }, lty = entity),
                size = 2,
                col = 'snow1'
            ) +
            scale_color_brewer(palette = 'Set1') +
            scale_x_continuous(
                limits = c(1990, 2020),
                breaks = seq(1990, 2018, 5),
                expand = c(0, 0)
            ) +
            labs(
                linetype = '',
                color = '',
                y = ylab,
                subtitle = subtitle
            ) +
            theme_minimal() +
            theme(
                plot.background = element_rect(fill = 'grey70'),
                panel.grid = element_line(color = 'grey75'),
                legend.key.width = unit(0.7, 'in'),
                legend.position = legend_pos,
                legend.box = 'horizontal',
                legend.text = element_text(face = 'bold'),
                axis.title.y = element_text(size = 14),
                plot.subtitle = element_text(
                    size = 20,
                    face = 'bold',
                    color = 'snow1',
                    hjust = 0.8,
                    vjust = -10
                )
            )
        
    }

final_plot <- 
plot(
    data = landline,
    var = landline_subs,
    subtitle = 'Landline',
    legend_pos = 'none',
    ylab = 'Fixed telephone subscriptions\n(per 100 people)'
) /
    plot(
        data = mobile,
        var = mobile_subs,
        subtitle = 'Mobile',
        ylab = 'Fixed mobile subscriptions\n(per 100 people)'
    ) + plot_annotation(title = 'Historical Phone Usage',
                        caption = 'Credit: @johnmutiso_\nData: OurWorldInData.org\nGraphic: #TidyTuesday week 46',
                        theme = theme(plot.title = element_text(
                            size = 28,
                            family = 'Stencil',
                            face = 'bold',
                            color = 'grey50'
                        )))
# save plot
ggsave(
    plot = final_plot,
    height = 10,
    width = 8,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

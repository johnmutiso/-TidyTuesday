#' -------------------------------------------------------------------------------
#' Toronto Shelters
#' The data this week comes from [Sharla Gelfand's opendatatoronto R package](https://github.com/sharlagelfand/opendatatoronto).
#' This [Homeless Shelters in Toronto](https://rabble.ca/blogs/bloggers/cathy-crowes-blog/2019/03/twitter-truth-torontos-homeless-emergency)
#'  talks about Toronto Shelters in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 49
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
    shelters %>%
    filter(!occupancy < 0) %>%
    mutate(
        occupancy_date = lubridate::as_date(occupancy_date),
        organization_name = str_wrap(organization_name, 30)
    ) %>%
    ggplot(aes(occupancy_date, occupancy)) +
    geom_line(aes(group = facility_name, col = sector), show.legend = F) +
    scale_x_date(date_labels = "%b/%Y") +
    facet_wrap( ~ organization_name, scales = 'free', drop = T) +
    scale_color_manual(values = c("#d11141", "#00b159", "#00aedb", "#f37735", "#ffc425")) +
    labs(x = 'Date', y = 'Occupancy') +
    theme_bw(base_family = 'Calibri', base_line_size = 0.2) +
    theme(
        strip.text = element_text(size = 13, face = 'bold', color = 'snow1'),
        strip.background = element_rect(fill = '#468499', color = '#468499'),
        axis.title = element_text(size = 12, face = 'bold'),
        plot.background = element_rect(fill = 'grey80')
    )

title <-
    ggplot(data = NULL) +
    ggtext::geom_richtext(
        aes(x = 0, y = 0, label = '<span style="color:#133337;font-size:60px;font-family:Papyrus">Toronto Shelters</span><br/><span style="color:#d11141">Co-ed, <span style="color:#00b159">Families, <span style="color:#00aedb">Men, <span style="color:#f37735">Women, <span style="color:#133337">& <span style="color:#ffc425">Youth'),
        size = 10,
    ) +
    xlim(c(-0.01,0.01)) +
    ylim(c(-0.005, 0.005)) +
    theme_void() 

final_plot <-
    plot + patchwork::inset_element(
        title,
        bottom = 0.016,
        top = 0.19,
        left = 0.52,
        right = 0.99,
        align_to = 'full'
    )

# save plot
ggsave(
    plot = final_plot,
    height = 10,
    width = 18,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

# Data from:  Sharla Gelfand's opendatatoronto R package - originally open.toronto.ca.

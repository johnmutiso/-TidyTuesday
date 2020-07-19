#' -------------------------------------------------------------------------------
#' Astronaut database
#' The data this week comes from [Mariya Stavnichuk and Tatsuya Corlett.](https://data.mendeley.com/datasets/86tsnnbv2w/1).
#' This [Population analysis of space travelers](https://doi.org/10.1016/j.lssr.2020.06.003) talks about Astronauts in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

rm(list = ls())
week_num = 29
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

source('read_TT_data.R')
read_TTdata(week = week_num)


# plot
plot <-
    astronauts %>%
    mutate(
        age_at_selection = year_of_selection - year_of_birth,
        yrs_2_misson_frm_selection = year_of_mission - year_of_selection,
        age_at_mission = year_of_mission - year_of_birth,
        point_s = runif(1277, 40, 77),
        point_s = ifelse(str_detect(nationality, "U\\.S\\.|Russia"), NA, point_s)
        
    ) %>%
    ggplot() +
    geom_point(
        aes(reorder(nationality, desc(age_at_mission), FUN = median), point_s),
        position = position_jitter(width = 1),
        color = 'grey80',
        shape = 4,
        size = 1 / 10
    ) +
    geom_point(
        aes(
            y = age_at_mission,
            x = reorder(nationality, desc(age_at_mission), FUN = median),
            fill = military_civilian,
        ),
        shape = 21,
        size = 4,
        col = 'snow1',
        alpha = 0.7,
        position = position_jitter(width = 0.3)
    ) +
    geom_text(
        aes(x = 17, y = 75,
            label = 'JOHN GLENN (Year: 1961)\n Mission: FRIENDSHIP 7 @77yrs,\n    The first American to orbit\n    the Earth, 5th person and 3rd\n    American in space'),
        hjust = 1,
        size = 2.5,
        family = 'Segoe Print',
        col = '#ffd700'
    ) +
    labs(
        y = 'Age (yrs) going to mission',
        x = '',
        title = 'SPACE MISSIONS',
        caption = "GitHub: @johnmutiso\nData: https://doi.org/10.1016/j.lssr.2020.06.003 via (@geokaramanis)\nGraphic: TidyTuesday week 29"
    ) +
    coord_polar(clip = 'on', start = 80) +
    theme_minimal(base_rect_size = 0) +
    theme(
        axis.text.y = element_text(size = 6),
        plot.background = element_rect(fill = '#666666'),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = '#6897bb'),
        axis.text.x = element_text(color = '#e9f8ff', size = 12, face = 'bold'),
        axis.text.y.left = element_text(size = 15, color = '#d2b48c'),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.1),
        legend.text = element_text(size = 15, color = '#5ac18e'),
        axis.title.y = element_text(size = 14, color = '#5ac18e'),
        plot.caption = element_text(color = '#dcedc1', size = 7),
        plot.title = element_text(size = 30, hjust = 0.5, family = 'Stencil', color = '#5ac18e')
    )

# save plot
ggsave(
    plot = plot,
    height = 10.2,
    width = 9.7,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

#data: https://doi.org/10.1016/j.lssr.2020.06.003 via Georgios Karamanis (@geokaramanis)
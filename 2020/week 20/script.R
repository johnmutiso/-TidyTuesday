#' -------------------------------------------------------------------------------
#' VOLCANO ERUPTIONS
#' The data this week comes from [Volcano Eruptions](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-12/readme.md).
#' This [Smithsonian](https://www.himalayandatabase.com/) talks about Volcano Eruptions in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' ---------------------------------------------------------------------------

# Libraries -------------------------------------------------------------------
library(tidyverse)
library(extrafont)

# Data -----------------------------------------------------------------------------
#tuesdata <- tidytuesdayR::tt_load(2020, week = 20)

# writing the data -----------------------------------------------------------------
#readr::write_csv(x= tuesdata$volcano, './2020/week 20/data/volcano.csv', col_names = T)

# Creating categories for the latest year of eruption -------------------------------
volcano <-
    readr::read_csv('./2020/week 20/data/volcano.csv') %>%
    mutate(
        eruption_year = as.numeric(last_eruption_year),
        erup_year_cat = factor(
            case_when(
                is.na(eruption_year) ~ 1,
                eruption_year <= 0 ~ 2,
                eruption_year > 0 & eruption_year < 1900 ~ 3,
                eruption_year >= 1900 & eruption_year < 2000 ~ 4,
                eruption_year >= 2000 & eruption_year < 2010 ~ 5,
                eruption_year >= 2010 & eruption_year <= 2020 ~ 6
            ),
            labels = c(
                'Unknown',
                'BC',
                'AD 1-1899',
                '1900-1999',
                '2000-2009',
                '2010-2020'
            )
        )
    )

# spatial dataset for the world map -------------------------------------
world_map <- sf::st_as_sf(maps::map('world', fill = T, plot = F))

# plot ----------------------------------------------------------------------------------------
plot <-
    ggplot() +
    geom_sf(
        data = world_map,
        color = '#696966',
        fill = '#fffafa',
        size = 0.1
    ) +
    geom_point(
        data = volcano,
        aes(
            x = longitude,
            y = latitude,
            fill = erup_year_cat,
            color = erup_year_cat,
            # last eruption period
            shape = evidence_category
        ),
        # data certainity level
        alpha = 0.6,
        size = 3,
        position = position_jitter(
            height = 3.5,
            width = 3.5,
            seed = 100
        )
    ) +
    scale_fill_manual(
        'Last Eruption',
        values = c(
            '#726255',
            '#ffc100',
            '#ff9a00',
            '#ff7400',
            '#ff4d00',
            '#ff0000'
        ),
        aesthetics = c('colour', 'fill')
    ) +
    scale_shape_manual("Evidence category", values = 21:25) +
    labs(title = 'VOLCANO ERUPTIONS',
         subtitle = 'Last Eruption',
         caption = 'Github: @johnmutiso\nData: The Smithsonian Institution @SmithsonianGVP\nGraphic: 2020-week 20 TidyTuesday') +
    coord_sf(xlim = c(-180, 180),
             ylim = c(-90, 90),
             expand = FALSE) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = '#00ced1'),
        legend.title = element_text(
            family = 'Courier New',
            color = '#ff4646',
            size = 14,
            face = 'bold'
        ),
        plot.title = element_text(
            hjust = 0.5,
            family = 'Tahoma',
            size = 22,
            face = 'bold',
            color = '#133337'
        ),
        plot.subtitle = element_text(
            hjust = 0.5,
            family = 'Courier New',
            size = 22,
            face = 'bold',
            color = '#b72d5b'
        ),
        axis.title = element_blank(),
        axis.text = element_text(color = '#003333', face = 'bold')
    )

#Save plot ----------------------------------------------------------------------------------
ggsave(
    plot = plot,
    height = 8.1,
    width = 15.8,
    dpi = 500,
    device = 'png',
    filename = 'week20plot.png',
    path = './2020/week 20/'
)

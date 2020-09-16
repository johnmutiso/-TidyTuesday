#' ----------------------------
#' ---------------------------------------------------
#' Global Crop Yields
#' The data this week comes from [Our World in Data](https://ourworldindata.org/crop-yields).
#' This [ARTICLE_SOURCE](LINK_TO_ARTICLE) talks about Global Crop Yields in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 36
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(sf)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)



ken <-
    GADMTools::gadm_sf_loadCountries("KEN", level = 0, basefile = './2020/week 36/data/')

kenya_map <- read_rds('2020/week 36/data/KEN_adm0.sf.rds') %>%
    as_Spatial() %>%
    rmapshaper::ms_simplify(0.005) %>%
    st_as_sf()

kenya_yield <-
    key_crop_yields %>%
    filter(Entity %in% "Kenya") %>%
    select(-c(10, 13)) %>%
    pivot_longer(cols = 4:12,
                 names_to = 'category',
                 values_to = 'yield') %>%
    mutate(category_ = str_extract(category, "\\w+\\s"),
           geometry = kenya_map$geometry) %>%
    st_as_sf()

# plot
plot <-
    kenya_yield %>%
    ggplot() +
    geom_sf(aes(fill = yield)) +
    facet_grid(category_ ~ Year) +
    scale_fill_gradient2(
        "Yield\n(Tonnes/hectare)",
        low = 'white',
        high = 'darkgreen',
        na.value = '#696969'
    ) + guides(fill = guide_colorbar(ticks = T, nbin = 10)) +
    labs(title = 'KENYA: Changes in Agricultural Productivity Over Time',
         subtitle = 'Average Crop Yields (Tonnes per Hectare)\n',
         caption = "\n\nCredit: @johnmutiso\nData: Our World in Data(https://ourworldindata.org/crop-yields)\nGraphic: #TidyTuesday week 36") +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        legend.position = 'bottom',
        strip.text.y = element_text(
            angle = 0,
            size = 16,
            face = 'bold',
            color = 'snow1'
        ),
        strip.text.x = element_text(face = 'bold', size = 12, color = 'snow1'),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.switch.pad.grid = unit(0.01, 'in'),
        plot.background = element_rect(fill = '#696969'),
        legend.key.width = unit(4, 'cm'),
        legend.title = element_text(size = 16, color = '#40e0d0'),
        legend.text = element_text(size = 14, color = 'snow1'),
        plot.title = element_text(
            size = 30,
            face = 'bold',
            family = 'Arial',
            color = '#40e0d0'
        ),
        plot.subtitle = element_text(
            size = 25,
            color = '#dcedc1',
            family = 'Calibri'
        ),
        plot.caption = element_text(color = 'snow1', size = 14)
    )

# save plot
ggsave(
    plot = plot,
    height = 8,
    width = 25,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

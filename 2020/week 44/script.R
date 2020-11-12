#' -------------------------------------------------------------------------------
#' Canadian Wind Turbines
#' The data this week comes from [Government of Canada.](https://open.canada.ca/data/en/dataset/79fdad93-9025-49ad-ba16-c26d718cc070).
#'   via Will Noel(@OneWindyBoy), Tim Weis(@TimWeisAB) and Andrew Leach(@andrew_leach)
#' This [Canada's National Observer Article](https://www.nationalobserver.com/2020/10/23/news/wind-turbine-database-canada)
#'   talks about [Canadian Wind Turbines] in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 44
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(ggspatial)
library("rnaturalearth")
library("rnaturalearthdata")
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

world <- ne_countries(scale = "medium", returnclass = "sf")

regions <-
    wind %>%
    group_by(province_territory) %>%
    summarise(long_med = median(longitude),
              lat_med = median(latitude)) %>%
    ungroup() %>%
    select(province_territory, long_med, lat_med)

# plot
plot <-
wind %>%
    ggplot() +
    geom_sf(data = world, aes(fill = iso_a3 == 'CAN'), show.legend = F) +
    geom_jitter(aes(longitude, latitude, col = turbine_rated_capacity_k_w, size = rotor_diameter_m), alpha = 0.6) +
    geom_text(
        data = regions,
        aes(long_med, lat_med, label = province_territory),
        alpha = 0.6,
        fontface = 'bold',
        size = 6,
        check_overlap = T
    ) +
    scale_fill_manual(values = c('grey90', 'snow1')) +
    scale_color_viridis_c() +
    coord_sf(ylim = c(35,75), xlim = c(-150, -40)) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    labs(color = "Turbine rated\nCapacity (Kw)",
         size = "Rotor Diameter\n(Metres)",
         title = "Canadian Wind Turbines\n",
         caption = "\n\n\nCredit: @johnmutiso_\nData: Government of Canada via @OneWindyBoy, @TimWeisAB & @andrew_leach\nGraphic: #TidyTuesday Week 44") +
    theme_minimal() +
    theme(plot.background = element_rect(fill='#9DE7BE', size = 4, color = '#8A5257'),
          panel.grid = element_line(color = 'grey80'),
          axis.title = element_blank(),
          axis.text = element_text(color = 'grey20', size = 12, face = 'bold', family = 'Arial Bold'),
          legend.text = element_text(size = 14, color = 'grey30', face = 'bold'),
          legend.title = element_text(size = 16, color = 'grey50', face = 'bold'), 
          plot.title = element_text(size = 40, family = 'Forte', face = 'bold', color = 'grey40'),
          plot.caption = element_text(color = 'grey50', size = 10))



# save plot
ggsave(
    plot = plot,
    height = 7.25,
    width = 10,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

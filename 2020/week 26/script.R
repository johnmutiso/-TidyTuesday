#' -------------------------------------------------------------------------------
#' CARIBOU LOCATIONS
#' The data this week comes from [Movebank prepated by Alex Cookson](https://www.movebank.org/cms/movebank-content/about-movebank).
#' This [B.C. Ministry of Environment](https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/wildlife-wildlife-habitat/caribou/science_update_final_from_web_jan_2014.pdf)
#'   talks about Caribou locations in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(sf)
loadfonts(device = 'win')

#tuesdata <- tidytuesdayR::tt_load(2020, week = 26)

#readr::write_csv(tuesdata$individuals, path = '2020/week 26/data/individuals.csv')

locations <- readr::read_csv('2020/week 26/data/locations.csv') %>%
    arrange(animal_id, season, timestamp)
individuals <- readr::read_csv('2020/week 26/data/individuals.csv')

animal_id <- locations %>% 
    distinct(animal_id) %>% 
    arrange(animal_id) %>%
    mutate(group = case_when(row_number() < 131 ~ 1,
                             TRUE ~ 2)) %>% 
    group_by(group) %>% 
    mutate(id = 1:n())

study_site1 <- locations %>%
    select(study_site,season,longitude, latitude) %>% 
    distinct(study_site, .keep_all = T)

study_site2 = study_site1 
study_site2$season='Winter'
study_site <- bind_rows(study_site1,study_site2)

set.seed(635653)
colors <- randomcoloR::distinctColorPalette(260)

# plot
ggplot(animal_id) +
    geom_text(aes(factor(group), id, label = animal_id, col = animal_id), 
              size=2, show.legend = F)+
    theme_bw()+
    scale_color_manual(values = colors)

plot <-
    ggplot() +
    #geom_sf(data=map_x)+
    geom_path(
        data = locations,
        aes(longitude, latitude, group = animal_id, col = animal_id)
        ,show.legend = F
    ) +
    scale_color_manual(values = colors) +
    geom_text(
        data = study_site,
        aes(longitude, latitude, label = study_site)
    ) +
    facet_grid(. ~ season) +
    coord_map()+
    labs(title = 'Caribou Location Tracking',
         subtitle = 'Different Study Sites',
         caption = 'GitHub: @johnmutiso\nData:Seip & Price (2019) via Movebank\nGraphic: TidyTuesday Week 26') +
    theme_bw()+
    theme(strip.text = element_text(size = 16),
          plot.title=element_text(hjust=0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5))

# save plot
ggsave(
    plot = plot,
    height = 7.5,
    width = 7,
    dpi = 500,
    device = 'png',
    filename = 'week26plot.png',
    path = './2020/week 26/'
)

# data citation
#Seip DR, Price E (2019) Data from: Science update for the South Peace Northern Caribou (Rangifer tarandus caribou pop. 15) in British Columbia. Movebank Data Repository. https://doi.org/10.5441/001/1.p5bn656k

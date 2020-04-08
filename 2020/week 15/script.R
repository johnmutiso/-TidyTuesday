# Header ------------------------------------------------------------------
#' Name: John Mutua
#' Github: @johnmutiso
#' Twitter: @johnmutiso_
#' Graphic: TidyTuesday week 15
#' 

# Libraries
library(tidyverse)
library(ggrepel)
library(extrafont)
library(ggimage)

loadfonts(device = 'postscript', quiet = T)
# Read in data -------------------------------------------------------------
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

# plot ---------------------------------------------------------------------
plot <- tdf_winners %>% 
    filter(!is.na(time_margin)) %>%
    mutate(nationality = str_squish(nationality)) %>% 
    mutate(ccode = case_when(nationality %in% "France" ~ 'FRA',
                             nationality %in% c("Luxembourg") ~ 'LUX',
                             nationality %in% c("Belgium") ~ 'BEL',
                             nationality %in% c("Italy") ~ 'ITA',
                             nationality %in% c("Switzerland") ~ 'CHE',
                             nationality %in% c("Spain") ~ 'ESP',
                             nationality %in% c("Netherlands") ~ 'NLD',
                             nationality %in% c("United States") ~ 'USA',
                             nationality %in% c("Ireland") ~ 'IRL',
                             nationality %in% c("Denmark") ~ 'DNK',
                             nationality %in% c("Germany") ~ 'DEU',
                             nationality %in% c("Australia") ~ 'AUS',
                             nationality %in% c("Great Britain") ~ 'GBR',
                             nationality %in% c("Colombia") ~ 'COL')) %>% 
    separate(winner_name, into = c('first_name',NA),sep = " ") %>%
    mutate(name_ccode = str_c(first_name, '[',ccode, ']')) %>%
   ggplot(aes(edition, time_overall)) +
    geom_point(aes(size = time_margin*60, fill = distance),
               position = position_dodge(width=0.3), alpha = 0.7, pch = 21, col = '#000080') +
    geom_text_repel(aes(label=name_ccode), size = 3.5, segment.size = 1, direction = 'both',
                    position = position_dodge(width = 0.5), segment.alpha=0.3, segment.color = '#ff2800',
                    color = '#ff5d00', family = 'serif', fontface = 'bold', 
                    arrow = arrow(type = 'open', ends = 'first', length = unit(0.1, 'in'))) +
    scale_size_area("Time to\nRunner-up\n(minutes)", max_size = 20, breaks = c(1,5,30,60,120)) +
    scale_fill_viridis_c(name = 'Distance\nCovered(KM)')+
    labs(x = "Tour Edition",
         y = "Time (Hours) taken to Win the Race ",
         title = "Tour de France Winners",
         subtitle = "How long Did it take to Win? How Far was the Runner-up?",
         caption = "Github: @johnmutiso\nData:  Alastair Rushworth's Data Package tdf & Kaggle \nGraphic: 2020-week 15 TidyTuesday")+
    theme_minimal() +
    theme(plot.background = element_rect(fill = '#daf7f8'), 
          axis.title = element_text(color = '#00007f', family = 'mono', size = 18, face = 'bold'),
          legend.text = element_text(color = '#00007f', family = 'mono', size = 14),
          axis.text = element_text(size = 20, face = 'bold'),
          plot.title = element_text(size = 35, family = 'Papyrus', face = 'bold'), 
          plot.subtitle = element_text(size = 25, family = 'mono', face = 'bold'))

# Saving the plot ------------------------------------------------------------------------------
ggsave(filename = 'week 15 plot.jpeg', plot = plot, device = 'jpeg', path = '2020/week 15/',dpi = 400,
       width = 22, height = 10)

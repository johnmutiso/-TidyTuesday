#' --------------------------------------------------------------------------------------
#' [ANIMAL CROSSING]
#' The data this week comes from [Villager DB](https://github.com/jefflomacy/villagerdb). 
#' This [Polygon](https://www.polygon.com/2020/4/2/21201065/animal-crossing-new-horizons-calm-mindfulness-coronavirus-quarantine) 
#' talks about [Why Animal Crossing calms you down, explained] in greater detail.
#' Credit: [JOHN MUTISO](twitter: @johnmutiso_)
#' ----------------------------------------------------------------------------------------

#libraries ----------------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# Read data -----------------------------------------------------------------------------
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

villagers_1 <- 
    villagers %>% 
    mutate(lazy = case_when(personality %in% 'lazy' ~ 'Lazy',
                            TRUE ~ 'Not Lazy'),
           species = str_to_title(species)) %>% 
    group_by(species, lazy) %>% 
    summarise(count = n()) %>% 
    pivot_wider(id_cols = 1,
                names_from = lazy,
                values_from = count) %>% 
    mutate(Lazy = replace_na(Lazy,0),
           lazy_ = (Lazy/(Lazy+`Not Lazy`))*100,
           not_lazy = (`Not Lazy`/(Lazy+`Not Lazy`))*100) %>% 
    pivot_longer(cols = 4:5,
                 names_to = 'lazy',
                 values_to = 'per') %>% 
    select(-c(2:3)) %>% 
    mutate(per_1 = paste0(round(per,1),'%'),
           lazy = factor(lazy, levels = c('not_lazy','lazy_'),
                         labels = c('Not Lazy','Lazy')),
           lazy1 = ifelse(lazy %in% 'Lazy', per, 0))
    

plot <- 
villagers_1 %>% 
    ggplot(aes(y = reorder(species,desc(lazy1)), x=per)) +
    geom_bar(aes(fill = lazy), stat = 'identity') +
    geom_text(aes(label = ifelse(per > 0 & lazy == 'Lazy', per_1, '')),
              position = position_stack(vjust = 0.5), family = 'Courier New',
              fontface = 'bold', col = '#eeeeee') +
    scale_fill_manual('', values = c('#92bcc8','#fb7c83')) +
    scale_x_continuous(expand = c(0,0)) +
    labs(title = 'ANIMAL CROSSING - NEW HORIZONS',
         subtitle = 'The Lazy Personality',
         caption = 'Github: @johnmutiso\nData: VillagerDB & Metacritic\nGraphic: 2020-week 19 TidyTuesday') +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_text(hjust = 1, color = '#f5f5f5', size = 12,
                                     family = 'Courier New', face = 'bold'),
          plot.background = element_rect(fill = '#00847d'),
          legend.position = 'bottom',
          legend.text = element_text(color = '#f5f5f5', size = 14,
                                     family = 'Tahoma'),
          plot.title = element_text(color = '#e2ddd9', family = 'Bodoni MT', face = 'bold', size = 18, hjust = 0.5),
          plot.subtitle = element_text(color = '#ffff66', size = 16, family = 'Lucida Calligraphy', hjust = 0.5),
          plot.caption = element_text(color = '#c0d6e4', family = 'Courier New'))

# save the plot ------------------------------------------------------------------------------------
ggsave(plot = plot,
       height = 7.5,
       width = 6.5,
       dpi = 400,
       device = 'png',
       filename = 'week19plot.png',
       path = './2020/week 19/')

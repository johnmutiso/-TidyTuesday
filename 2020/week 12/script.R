
# Hearder ----------------------------------------------------------------
#' Name: John Mutiso
#' twitter: @johnmutiso_
#' github: @johnmutiso
#' 

#Libraries
library(tidyverse)

# Reading data

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


# number of texts per episode
office_ratings %>% 
    mutate_at(.vars = c(1,2), .funs = as.factor) %>% 
    ggplot(aes(x = episode, y = season, alpha = imdb_rating), col = 'white') +
    geom_tile() +
    geom_text(data = office_ratings %>% filter(imdb_rating>9),
              aes(episode, season-1, label = title)) +
    theme_classic() + 
    theme(plot.background = element_rect(fill = '#f6f6f6'), 
          legend.title = element_blank(), 
          legend.background = element_rect(fill = '#f6f6f6'),
          axis.ticks = element_blank(), axis.text = element_text(size = 14))

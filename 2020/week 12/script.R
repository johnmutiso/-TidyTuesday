
# Hearder ----------------------------------------------------------------
#' Name: John Mutiso
#' twitter: @johnmutiso_
#' github: @johnmutiso
#' 

#Libraries
library(tidyverse)

# Reading data

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


# Episodes with a rating greater than 9
office_9 <- office_ratings %>% filter(imdb_rating>9) %>% 
    mutate(y = row_number()/1.5) #y variable to tidy the text ad segments
    


# number of texts per episode
plot <- office_ratings %>% 
    mutate_at(.vars = c(1,2), .funs = as.factor) %>% 
    ggplot(aes(x = episode, y = season, alpha = imdb_rating), col = 'white') +
    geom_tile() +
    geom_segment(data = office_9, aes(x = episode, y = season, xend = episode+1.5, yend = y), 
                 show.legend = F, arrow = arrow(ends = 'first', type = 'open', length = unit(0.1, 'inches')), 
                 col = '#78edb2', size = 1,)+
    geom_label(data = office_9, aes(episode+2.4, y, label = paste0(title, '[', imdb_rating, ']'), fontface = 'bold'), 
              show.legend = F, col = "#003366") +
    labs(x = "Episodes #",
         y = "Seasons #",
         title = "THE OFFICE SERIES IMDb RATINGS", 
         subtitle = "Highlighted {Ratings Above 9/10}", 
         caption = "Github: @johnmutiso\ndata: schrute R Package & data.world\n2020-week 12 TidyTuesday") + 
    theme_classic() + 
    theme(plot.background = element_rect(fill = '#f6f6f6'), 
          legend.title = element_blank(), 
          legend.background = element_rect(fill = '#f6f6f6'),
          axis.ticks = element_blank(), 
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 16), 
          legend.position = 'top',
          plot.caption = element_text(colour = '#003366', face = 'italic', size = 12),
          plot.title  = element_text(colour = '#003366', face = 'bold', size = 24),
          plot.subtitle = element_text(colour = '#5b8dc5', face = 'bold.italic', size = 18))

# Saving the plot
ggsave(filename = 'week 12 plot.jpeg', plot = plot, device = 'jpeg', path = '2020/week 12/',dpi = 400,
       width = 14, height = 7.2)

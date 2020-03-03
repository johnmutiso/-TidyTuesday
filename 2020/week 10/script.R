
# Header ------------------------------------------------------------------
#'Author: John Mutiso
#'Twitter: @johnmutiso_
#'-------------------------------------------------------------------------

library(tidyverse)
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')


# Filtering who've hit 700th NHL goal
top_players <- game_goals %>% 
    group_by(player) %>% 
    mutate(cum_goals = cumsum(goals), cumnum_games = 1:n()) %>% 
    ungroup() %>% 
    filter(player %in% top_250$player[1:8],
           goals != 0)

hit_700 <- top_players %>% 
    group_by(player) %>% 
    filter(between(cum_goals, 700, 702)) %>% 
    select(player, cum_goals, age, location, cumnum_games) %>% 
    distinct(player, .keep_all = T) 

cols <- RColorBrewer::brewer.pal(5, 'Dark2')

# A time series plot for their cumulative number of goals - from the 90's
plot <- ggplot() +
    scale_x_continuous(breaks = seq(0,1800,200), minor_breaks = NULL, expand = expand_scale()) +
    scale_y_continuous(breaks = seq(0,900,100), minor_breaks = NULL, expand = expand_scale()) +
    geom_hline(yintercept = 700, size = 0.8, col = 'gray80', lty = 4) +
    geom_vline(xintercept = c(hit_700$cumnum_games), lty = 3, size = 0.8, col = cols) +
    geom_line(data = top_players,aes(cumnum_games, cum_goals, col = player), size = 0.85) +
    geom_text(data = hit_700, aes(x = cumnum_games, y = c(330,220,530,430,150), 
                                   label = paste(player, '\n', 'Age=', 
                                                 substr(age,1,2), 'yrs', '&', 
                                                 substr(age,4,6), 'days', " "), col = player), 
              size = 3, position = position_dodge(0.4), fontface = 'bold') +
    geom_text(data = hit_700, aes(x = cumnum_games + 40, y = c(20,50,15,20,15), 
                                  label = cumnum_games, col = player), size = 3, fontface = 'bold') +
    geom_rect(aes(xmin = 960, xmax = 1350, ymin = 280, ymax = 370),
              fill = "transparent", color = "red", size = 1) +
    scale_color_manual(values = cols) +
    labs(x = "Number of Games", y = "Cumulative Number of Goals",
         title = "THE GREAT NHL PLAYERS - FROM THE 90's",
         subtitle = bquote("The" ~ 700^th ~ "NHL Championship Goooal"),
         caption = "Github: @johnmutiso\ndata: HockeyReference.com\n2020-week 10 TidyTuesday") +
    theme_minimal() +
    theme(panel.grid = element_line(size = 0.2, colour = 'grey25'),
          plot.background = element_rect(fill = 'gray15'), 
          axis.text = element_text(colour = 'grey80'), 
          axis.title = element_text(size = 12, colour = 'grey85', face = 'bold.italic'), 
          legend.position = 'none', 
          plot.caption = element_text(colour = 'grey50', face = 'italic', size = 8),
          plot.title  = element_text(colour = 'grey90', face = 'bold', size = 13),
          plot.subtitle = element_text(colour = 'grey70', face = 'bold.italic', size = 10))


ggsave(filename = 'week 10 plot.jpeg', plot = plot, device = 'jpeg', path = '2020/week 10/',dpi = 500,
       width = 10, height = 5.8)

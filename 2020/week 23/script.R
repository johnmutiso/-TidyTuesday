#' -------------------------------------------------------------------------------
#' --Marble Racing--
#' The data this week comes from Jelle's Marble Runs Courtesy of [Randy Olson](https://www.youtube.com/channel/UCYJdpnjuSWVOLgGT9fIzL0g).
#' This [Randy Olson article](http://www.randalolson.com/2020/05/24/a-data-driven-look-at-marble-racing/) talks about Mabble racing in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------


# libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(gridExtra)
library(extrafont)

loadfonts(device = 'win')

# data ----------------------------------------------------------------------------
# tuesdata <- tidytuesdayR::tt_load(2020, week = 23)
# marbles <- tuesdata$marbles

# readr::write_csv(marbles, '2020/week 23/data/marbles.csv')

marbles <- readr::read_csv('2020/week 23/data/marbles.csv') %>% 
    mutate(date = as_date(date, format = '%d-%b-%y'),
           site_date = paste0(site, '\n', day(date), '-', month(date, label = T), '-', isoyear(date))) %>% 
    drop_na(points)
    

glimpse(marbles)

# Points variation per marble
marble_sd <- 
    marbles %>%
    drop_na(points) %>%
    group_by(marble_name) %>%
    summarise(mean = mean(points),
              sd = round(sd(points), 2)) %>%
    arrange(sd) %>%
    filter(!mean == 0) %>% 
    mutate(marble_name = str_pad(marble_name, 'left', width = 0))

names(marble_sd) <- c('Marble\nName', 'Average\nPoints', 'Standard\nDeviation')

# Grob table
marble_sd_tab <- tableGrob(marble_sd, rows = NULL, theme = ttheme_gtstripes(base_size = 12, base_family = 'Verdana'))

set.seed(4242)
colors <- randomcoloR::distinctColorPalette(32)

plot <- 
marbles %>%
    ggplot(aes(reorder(site_date, date, FUN = min), points)) +
    geom_line(
        aes(group = marble_name, col = marble_name),
        size = 1.1,
        alpha = 0.8,
        show.legend = F
    ) +
    geom_point(aes(size = avg_time_lap, fill = time_s),
               pch = 21) +
    geom_text_repel(aes(site_date, points, label = marble_name), 
                    fontface = 'bold',
                    col = '#468499') +
    labs(x = 'Race Site\nDate of Race',
         y = 'Points Gained',
         title = 'MARBLE RACING: POINTS GAINED ACROSS DIFFERENT RACES',
         subtitle = '**Line colors only differentiate marbles**',
         caption = 'Github: @johnmutiso\nData: Jelle`s Marble Runs via Randy Olson\nGraphic: 2020-week 23 TidyTuesday') +
    scale_color_manual(values = colors) +
    scale_fill_viridis_c('Race time\nin seconds') +
    scale_size('Average time\nper lap (seconds)') +
    theme_bw() +
    theme(plot.background = element_rect(colour = '#065535', size = 5),
          axis.title = element_text(size = 14, color = '#407294'),
          axis.text = element_text(size = 13, face = 'bold', family = 'Tahoma', color = '#003366'),
          plot.subtitle = element_text(size = 14, color = '#800000', hjust = 0.5),
          plot.title = element_text(size = 18, hjust = 0.5, color = '#407294', family = 'Imprint MT Shadow', face = 'bold'))

# final plot
final_plot <- grid.arrange(plot, marble_sd_tab, ncol = 2, widths = c(4.5,1))

# save the plot
ggsave(
    plot = final_plot,
    height = 9,
    width = 20,
    dpi = 500,
    device = 'png',
    filename = 'week23plot.png',
    path = './2020/week 23/'
)


# Week 35

#Loading libraries
library(tidyverse)
library(ggthemes)

# reading The Simpsons data
simpsons <-
   readr::read_csv(
      "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv"
   ) %>%           
   filter(!str_detect(season, 'Movie')) %>% # removing observations with movie other than season number
   mutate(season = as.numeric(season)) # converting season variable to numeric


# Number of apprearances per guest star
Appearances <-
   simpsons %>%
   group_by(guest_star) %>%
   summarise(
      appearances = n(),
      last_first_appearance = max(season, na.rm = T) - min(season, na.rm = T)
   )


# Joining original data with number of appearances dataset
simpsons <-
   left_join(simpsons, Appearances, by = 'guest_star')

# Creating a vector of gradient fill colors
fill_col <- RColorBrewer::brewer.pal(n = 6, 'YlOrRd')

# Dataset for curves in the plot
curve_data <-
   data.frame(
      x = c(35, 35, 35, 14),
      xend = c(37, 31.4, 37, 20),
      y = c(55, 55, 55, 125.5),
      yend = c(30, 13, 50, 155)
   )

# The barplot
simpsons_barplot <- # plot name
   simpsons %>%
   filter(appearances > 2 & last_first_appearance > 2) %>% # filtering guest stars with more than 2 appearances
   ggplot() +
   geom_bar(aes(reorder(guest_star, appearances), fill = last_first_appearance)) +
   geom_bar(
      aes(reorder(guest_star, appearances), col = role),
      show.legend = F,
      alpha = 0,
      size = 1
   ) +
   scale_y_continuous(expand = c(0, 0), breaks = seq(0, 150, 25)) + # scalling the y axis # expand removes the space between axis text and the base of the bars
   scale_fill_gradientn(colours = fill_col) + # scaling the colors
   coord_flip() +
   annotate(
      'text',
      y = c(70, 125.5),
      x = c(35, 12),
      label = c(
         'The segments show \n different role plays\nfor each guest star',
         '# episodes between\nthe first appearance and most\nrecently appeared'
      ),
      col = 'snow',
      size = 6,
      family = 'palatino'
   ) + # annotating the plot
   geom_curve(
      data = curve_data,
      aes(
         x = x,
         y = y,
         yend = yend,
         xend = xend
      ),
      curvature = -0.5,
      col = 'snow',
      arrow = arrow(length = unit(0.03, "npc"))
   ) + # plotting curves to link the text annotated
   labs(
      y = '# of Appearances',
      x = 'More than 2 appearances',
      title = "GUEST STAR APPEARANCES IN THE SIMPSONS",
      subtitle = "Guest stars appearing more than twice and Diversity in role play",
      caption = 'Data Source: Wikipedia by way of: Andrew Collier\nTwitter: @johnmutiso_  GitHub: github.com/johnmutiso'
   ) + # figure title and captions
   theme_pander(
      bc = 'grey30',
      base_size = 16,
      base_family = 'Bookman',
      fc = 'yellow'
   ) + # changing from default theme
   theme(
      axis.text = element_text(
         face = 'bold.italic',
         family = 'mono',
         size = 13,
         colour = 'whitesmoke',
      ),
      legend.background  = element_rect(fill = 'grey30'),
      axis.ticks.y = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(
         face = 'bold',
         family = 'mono',
         size = 12,
         colour = 'whitesmoke'
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.caption = element_text(colour = 'snow2',
                                  size = 12),
      plot.subtitle = element_text(
         colour = 'wheat',
         face = 'bold',
         family = 'courier',
         hjust = -0.4
      ),
      plot.title = element_text(hjust = -0.54)
   ) # editing the plot theme

ggsave(
   plot = simpsons_barplot,
   filename = paste(Sys.Date(), 'Simpsons_Barplot.png' , sep = '_'),
   path = "Week_35/",
   dpi = 500,
   width = 15,
   height = 8,
   units = 'in',
   device = 'png'
) # Saving the plot to png

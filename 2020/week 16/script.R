#'title: TidyTuesday Week 16 - Best Rap Artists-----------------------------------------------------
#'This weeks data comes from: [BBC Music] (http://www.bbc.com/culture/story/20191007-the-greatest-hip-hop-songs-of-all-time-who-voted)
#'This article by [Simon Jockers at Datawrapper](https://blog.datawrapper.de/best-hip-hop-songs-of-all-time-visualized/)
#'talks about [The best hip-hop songs of all time, visualized] in greater detail 
#'Credit:[John Mutiso](twitter: @johnmutiso)

# libraries --------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(patchwork)

# Fonts ------------------------------
windowsFonts(Harrington = windowsFont('Harrington'),
             Papyrus = windowsFont('Papyrus'),
             Gabriola = windowsFont('Gabriola'))

# reading data ----------------------------------------------------------------------------------------------------
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# plots ---------------------------------------------------------------------------------------------------------
text1 <- "MC Lyte - first Hiphop Female artist\n to be nominated for a Grammy\nunder Best Rap Single(Song - 'Rucffneck')"
text2 <- "Queen Latifah - released hiphop\n hit 'UNITY' addressing\ndomestic violence, street harassment ..."
text3 <- "Missy Elliot - Wins her\nFirst Grammy Awards\n[song: Get Ur Freak On]"
text4 <- "Cardi B - Wins several categories\n in BET Awards - Her Single\n 'Bodak Yellow' doesn't win\n Hiphop video [Dj Khaled wins]"
text5 <- "Lauryn Hill's Album\n[The Miseducation of Lauryn Hill#5]\n top-TEN highest first-week\n home market sales"

# Barplot ~Year by ~Average votes -------------------------------------------------------------------------------------------------
plot1 <- 
rankings %>% 
    group_by(year, gender) %>% 
    summarise(mean_points = mean(points)) %>% 
    ggplot() +
    geom_bar(aes(factor(year), mean_points, fill = gender), stat = 'identity') +
    scale_y_continuous(limits = c(0,60), expand = c(0,0)) +
    scale_fill_brewer(palette = 'Paired', labels = c('Female','Male','Mixed')) +
    geom_text(aes(14,55), label = text1, family = 'Gabriola', hjust = -0.05, vjust = 1, size = 5, color = "#dddddd") +
    geom_text(aes(14, 45), label = text2, family = 'Gabriola', hjust = -0.05, vjust = 1, size = 5, color = "#ffb554") +
    geom_text(aes(23.6, 30), label = text3, family = 'Gabriola', hjust = -0.05, vjust = 1, size = 5, color = "#fff68f") +
    geom_text(aes(38, 26), label = text4, family = 'Gabriola', vjust = 1, size = 5, color = "#00ffff") +
    geom_text(aes(19, 34), label = text5, family = 'Gabriola', vjust = 1, size = 5, color = "#00ff00") +
    geom_segment(aes(x=14,xend=14,y=55,yend=30), arrow = arrow(length = unit(0.1,'in')), size = 1.5, col = '#bada55') + #arrow text1&2
    geom_segment(aes(x=26.8,xend=23.5,y=31,yend=31), arrow = arrow(length = unit(0.1,'in')), size = 1.5, col = '#bada55') + #text3
    geom_segment(aes(x=38,xend=38,y=16,yend=13), arrow = arrow(length = unit(0.1,'in')), size = 1.5, col = '#bada55') + #text4
    geom_segment(aes(x=19,xend=19,y=25.4,yend=25), arrow = arrow(length = unit(0.1,'in')), size = 1.5, col = '#bada55') + #text5
    labs(y = "Points Awarded [Mean]",
         title = "HIPHOP HISTORY - HITS OF ALL TIME",
         subtitle = "Key Focus on Best Female Artists {highlights annotated}",
         caption = "Github: @johnmutiso\nData: BBC Music via Simon Jockers at Datawrapper[@sjockers]\nGraphic: 2020-week 16 TidyTuesday") +
    theme_minimal() +
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "#c2e8ea",
                                      family = 'Harrington', face = 'bold', size = 16),
          axis.ticks = element_blank(),
          axis.title.y = element_text(color = "#c2e8ea"),
          axis.text.y = element_text(color = "#c2e8ea"),
          axis.title.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(fill = '#0e2f44'),
          text = element_text(color = "#c2e8ea", size = 16), 
          panel.grid = element_blank(), 
          legend.position = c(0.13,0.8), 
          legend.background = element_rect(fill = "#065535"), 
          legend.title = element_blank(),
          legend.text = element_text(size = 14, face = 'bold', family = 'Harrington',color = '#c2e8ea'),
          plot.title=element_text(size=22, face='bold', color = '#ffff00', family = 'Papyrus'),
          plot.subtitle=element_text(size=20, face='bold', color = 'snow1', family = 'Harrington'),
          plot.caption = element_text(color='#e6e6fa', size = 12, family = 'Papyrus'))

# Music votes -----------------------------------------------------------------------------------
# Data for Music with more than 10 total votes -------------------------------------------
highest_ranked <- 
    rankings %>% 
    filter(n > 10) 

# Plot for the votes by year for each music -------------------------------------------
plot2 <- 
ggplot() +
    geom_tile(data = rankings,
              aes(x=factor(year), y=factor(n1), fill = points), col = '#e6e6fa') +
    geom_label_repel(data = highest_ranked, 
                     aes(factor(year), factor(n1), label = paste(artist,title, sep = "\n~")), fill = NA,
                     segment.alpha = 0.6, segment.size = 1, segment.color = 'snow1', nudge_y = -0.2,nudge_x=-0.3, col = '#c2e8ea', 
                     family = 'Gabriola', size = 5, fontface = 'bold',hjust = -0.2, vjust = 0.2, arrow = arrow(length = unit(0.1, 'in'))) + 
    scale_fill_viridis_c("Total Points\nAwarded") +
    labs(y = "Number of Times Voted as #1",
         title = "HipHop Hits with More than 10 Total Votes Annotated") +
    theme_minimal() +
    theme(
        axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "#c2e8ea",
                                    family = 'Harrington', face = 'bold', size = 12),
        plot.background = element_rect(fill = '#065535', linetype = 6, size = 1, color = 'snow1'),
        axis.title.y = element_text(color = "#c2e8ea", size = 14),
        axis.text.y = element_text(color = "#c2e8ea", size = 12),
        axis.title.x = element_blank(),
        legend.position = c(0.9,0.7),
        legend.title = element_text(color = "#c2e8ea", size = 12), 
        panel.grid.major = element_line(color = '#696969', size = 0.2), 
        legend.text = element_text(size = 12, face = 'bold', family = 'Harrington',color = '#c2e8ea'),
        plot.title=element_text(size=18, face='bold', color = '#ffff00', family = 'Harrington')
    )

# Combine the plot -------------------------------------------------------------------------------------------
inset_plot <- ggplotGrob(plot2)
plot <- plot1 + annotation_custom(grob = inset_plot, xmin = 21, xmax = 40.5, ymin = 33, ymax = 66)                  
plot

# save the plot -----------------------------------------------------------------------------------------
ggsave(filename = "week16plot.jpeg", device = 'jpeg', 
       path = '2020/week 16/',plot = plot, dpi = 400, height = 9, width = 19)

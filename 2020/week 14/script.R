
#' Header -------TidyTuesday 2020 week 14--------------------
#' Name: John Mutiso
#' twitter: @johnmutiso_
#' Github: @johnmutiso
#' 

#libraries -------------------------------------------------
library(tidyverse)
library(patchwork)
#Loading Data ----------------------------------------------
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')



#brewing materials plot ------------------------------------
mydata <- brewing_materials %>% 
    mutate_at(.vars=c(1:5), .funs = as.factor) %>% 
    group_by(year,month, material_type) %>% 
    summarise(month_current = mean(month_current),
              month_prior_year = mean(month_prior_year)) %>% 
    pivot_longer(cols = c(month_current,month_prior_year), names_to = 'period', values_to = 'no_barrels') %>% 
    mutate(period = factor(period, levels = c('month_prior_year','month_current'), labels = c('Same Month Prior Year','Current Month')))

# Plot--------------------------------------------
no_barrels_plot <- ggplot(mydata %>% filter(!material_type == "Total Used"))+
    geom_tile(aes(year,month, fill = log10(no_barrels)),col = 'white', lty = 4, size=0.5) +
    facet_grid(period~material_type, ) +
    scale_fill_viridis_c("No. of Barrels\nin Millions", labels = c("0.1","1","10","100","1000"), breaks = c(5,6,7,8,9))+
    scale_y_discrete(labels = lubridate::month(1:12, abbr = T, label = T)) +
    labs(title = "Number of Brewing Material Barrels Used [Millions]",
         subtitle = "Grouped by Type and Compared with Prior Year") +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5, size = 10, face = 'bold'), 
          axis.text.y = element_text(size = 10, face = 'bold'),
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          legend.text = element_text(face='bold'), legend.title = element_text(face='bold', size=10),
          strip.background = element_rect(fill='#2a4d69'), 
          strip.text = element_text(color='#fdf6f3', size = 12, face='bold'),
          plot.title = element_text(size=14, face = 'bold', color = '#2a4d69'), 
          plot.subtitle = element_text(size=12, face = 'bold.italic', color = '#177245'))


#Total bear produced -----------------------------------------
total_brewer <- brewer_size %>% 
    filter(brewer_size == 'Total') %>% 
    mutate(year=factor(year))

total_produced_plot <- ggplot(total_brewer,aes(year, log10(total_barrels))) +
    geom_line(aes(group=brewer_size),  col = '#e9473f', size = 2) +
    labs(title = "Produced Brew",
         x = "",
         y = "Number of Barrels(Millions)") +
    scale_y_continuous(labels = c(180, 185, 190, 195), breaks = c(8.26,8.27,8.28,8.29))+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90, vjust = 0.5, size = 10, face = 'bold'), 
          axis.text.y = element_text(size = 10, face = 'bold'),
          axis.title.y = element_text(face='bold.italic'),
          plot.title = element_text(size=12, face = 'bold', color = '#2a4d69'))
    

total_shipped_plot <- ggplot(total_brewer, aes(year, log10(total_shipped))) +
    geom_line(aes(group=brewer_size), col = '#e9473f', size = 2)+
    labs(title = "Shipped Brew",
         x = "",
         y = "Number of Barrels(Millions)") +
    scale_y_continuous(labels = c(4.0,4.5,5.0,5.6,6.3,7.1), breaks = c(6.6,6.65,6.7,6.75,6.8,6.85))+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5, size = 10, face = 'bold'), 
          axis.text.y = element_text(size = 10, face = 'bold'),
          axis.title.y = element_text(face='bold.italic'),
          plot.title = element_text(size=12, face = 'bold', color = '#2a4d69'))

#Total  no barrels used -------------------------------------
total_no_barrels_plot <- ggplot(mydata %>% filter(material_type == "Total Used"))+
    geom_tile(aes(year,month, fill = log10(no_barrels)),col = 'white', lty = 4, size=0.5, show.legend = F) +
    facet_grid(~period) +
    scale_fill_viridis_c(begin = 0.5, end = 1)+
    scale_y_discrete(labels = lubridate::month(1:12, abbr = T, label = T)) +
    labs(title = "Total Brewing Materials Used",
         subtitle = "Compared with Prior Year") +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5, size = 10, face = 'bold'), 
          axis.text.y = element_text(size = 10, face = 'bold'),
          axis.text = element_text(color = '#2a4d69'),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),          
          legend.text = element_text(face='bold'), legend.title = element_text(face='bold', size=10),
          strip.background = element_rect(fill='#2a4d69'), 
          strip.text = element_text(color='#fdf6f3', size = 12, face='bold'),
          plot.title = element_text(size=12, face = 'bold', color = '#2a4d69'),
          plot.subtitle = element_text(size=12, face = 'bold.italic', color = '#177245'))


#final plot ------------------------------------------------------
plot1 <- (total_no_barrels_plot/
    (total_produced_plot|total_shipped_plot)|no_barrels_plot)+
    plot_layout(ncol = 2, widths=c(1,2.3)) +
    plot_annotation(title="Beer Production Statistics",
                    caption = "Github: @johnmutiso\nData: Alcohol and Tobacco Tax and Trade Bureau (TTB)|via @BrewersStats\nGraphic: 2020-week 14 TidyTuesday",
    theme=theme(plot.title=element_text(size=30, face='bold', color = '#600707'),
          plot.caption = element_text(color='#177245', family='mono', size = 10)))

ggsave(filename = "week14plot.jpeg", device = 'jpeg', 
       path = '2020/week 14/',plot = plot1, dpi = 400, height = 10, width = 22)

#' Author: John Mutiso
#' Twitter:@johnmutiso_
#' Github: @johnmutiso
#' packages used: tidyverse

#libraries
library(tidyverse)
#loading data

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')


#summary
summary(tbi_age)
summary(tbi_year)
summary(tbi_military)

#plots
tbi_age %>% 
    filter(age_group %in% ('Total')) %>%
    group_by(injury_mechanism) %>% 
    summarise(sum = sum(number_est)) %>% 
    arrange(sum) %>%  c()-> ordered_injr #to order by cause of injury

tbi_age %>% 
    filter(age_group %in% ('Total')) %>%
    group_by(type) %>% 
    summarise(sum = sum(number_est)) %>% 
    arrange(desc(sum)) %>%  c()-> ordered_type #to order by cause of injury


plot <- tbi_age %>%
    mutate_at(.vars = c(1,2), .funs = as.factor) %>% 
    mutate(age_group = factor(age_group, 
                              levels = c('0-4','0-17','5-14' ,'15-24','25-34','35-44','45-54', '55-64','65-74','75+','Total'), 
                              ordered = T),
           injury_mechanism = factor(injury_mechanism, levels = ordered_injr[[1]]),
           type = factor(type, levels = ordered_type[[1]])) %>% 
    filter(!age_group %in% c('Total', '0-17')) %>% 
    ggplot() +
    geom_point(aes(age_group, injury_mechanism, size = number_est, fill = rate_est), pch = 21, col = 'white') +
    scale_size_area(name = 'Estimated TBI \nObserved Cases', max_size = 15, breaks = c(500, 5000, 20000, 50000, 100000),
                    labels = c('500', '5,000', '20,000', '50,000', '100,000')) +
    scale_fill_viridis_c(name = 'Estimated TBI rate\nper 100,000') +
    labs(title = "TRAUMATIC BRAIN INJURY (TBI) IN US (2014)", 
         subtitle = "Emergency Department Visits, Hospitalizations & Deaths", 
         caption = "Github: @johnmutiso\ndata: CDC & Veterans Brain Injury Center\nGraphic: 2020-week 13 TidyTuesday") +
    theme_minimal() +
    facet_grid(~type) +
    theme(plot.background = element_rect(linetype = 8, fill = '#07070d'),
          axis.text  = element_text(size = 13, color ='#ddd6da', face = 'bold', family = 'sans'),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.title = element_blank(),
          legend.title = element_text(color = '#f5fffa', face = 'bold', size = 10),
          panel.grid.major = element_line(size = 0.01, color = '#56364a', linetype = 4),
          legend.text = element_text(color = '#ddd6da', face = 'bold', size = 8), 
          strip.text = element_text(size = 14, color = '#fff4df', face='bold'), 
          strip.background = element_rect(fill = '#826a51'),
          plot.caption = element_text(colour = '#ddd6da', face = 'italic', size = 12),
          plot.title  = element_text(colour = '#f0e68c', face = 'bold', size = 18),
          plot.subtitle = element_text(colour = '#bbaeb6', face = 'bold.italic', size = 14))

# Saving the plot
ggsave(filename = 'week 13 plot.jpeg', plot = plot, device = 'jpeg', path = '2020/week 13/',dpi = 350,
       width = 20, height = 6)

#' -------------------------------------------------------------------------------
#' --Slave TRADE--
#' The data this week comes from US Census's Archives(https://www.census.gov/content/dam/Census/library/working-papers/2002/demo/POP-twps0056.pdf),
#'                                  Slave Voyages(https://slavevoyages.org/), & Black Past(https://www.blackpast.org/african-american-history-timeline/)
#' This [BlackPast.org and Vox article](https://www.blackpast.org/african-american-history/juneteenth-birth-african-american-holiday-2/) 
#'      talks about American Slavery and Juneteenth in greater detail.
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
# blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
# census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
# slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
# african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

# readr::write_csv(african_names, '2020/week 25/data/african_names.csv')

slave_routes <- readr::read_csv('2020/week 25/data/african_names.csv')

plot <- 
slave_routes %>% 
    mutate(gender_=case_when(str_detect(gender, '^Boy|^Man')~'Male',
                             str_detect(gender, '^Girl|^Woman')~'Female',
                             TRUE ~ 'Unknown')) %>%  
    select(year_arrival, gender_, age) %>%
    group_by(gender_) %>% 
    arrange(year_arrival) %>% 
    mutate(cum_num = 1:n()) %>%
    ggplot(aes(x=year_arrival)) +
    geom_jitter(aes(y=age, color=gender_)) +
    geom_line(aes(y=cum_num/700, group = gender_), size = 2, col = '#fcc044')+
    scale_x_continuous(limits = c(1810,1850))+
    scale_y_continuous(sec.axis = sec_axis(trans = ~.*700, name = "Cumulative number of Slaves"))+
    scale_color_manual(values = c('#dd5449','#0d4241', '#bababa'))+
    geom_text(aes(x=1845, y = 68, label = 'Male'), fontface='bold', size = 8, col = '#c30101') +
    geom_text(aes(x=1845, y = 38, label = 'Female'), fontface = 'bold', size = 8, col='#c30101') +
    labs(title = "SLAVE TRADE",x = "Arrival Year", y = "Age (Years)", 
         caption = "Github: @johnmutiso\nData: US Census, Slave Voyages & Black Past\nGraphic: TidyTuesday week 25") +
    theme_minimal()+
    theme(plot.background = element_rect(fill = '#c0d6e4'),
          axis.text = element_text(size = 13, family = 'Tahoma'),
          axis.title = element_text(size = 14, family = 'Tahoma', face = 'bold'),
          legend.title = element_blank(),
          legend.key.height =  unit(1.2,'cm'),
          legend.text = element_text(size=13, face = 'bold'),
          legend.position = c(0.9,0.7),
          plot.title = element_text(hjust = 0.5, size = 24, family = 'Calibri', face='bold'))

# save the plot
ggsave(
    plot = plot,
    height = 9,
    width = 13,
    device = 'png',
    filename = 'week25plot.png',
    path = './2020/week 25/'
)


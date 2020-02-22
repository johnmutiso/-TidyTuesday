# Get the Data

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version ? from GitHub

# Either ISO-8601 date or year/week works!

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-02-18')
tuesdata <- tidytuesdayR::tt_load(2020, week = 8)


food_consumption <- tuesdata$food_consumption
country_continent <- readr::read_csv("https://raw.githubusercontent.com/dbouquin/IS_608/master/NanosatDB_munging/Countries-Continents.csv") %>% 
    rename(country=Country, continent=Continent)

library(tidyverse)
library(ggthemes)

#merging and creating continent and food_class variables
mydata <- left_join(food_consumption,country_continent, by="country") %>% 
    mutate(continent=replace(continent, country %in% c("Russia","Czech Republic"), "Europe"),
           continent=replace(continent, country=="USA", "South America"),
           continent=replace(continent, country=="Bermuda", "North America"),
           continent=replace(continent, country %in% c("Myanmar","Taiwan. ROC", "South Korea","Hong Kong SAR. China"), "Asia"),
           continent=replace(continent, country %in% c("French Polynesia","New Caledonia"), "Oceania"),
           food_class=ifelse(food_category %in% c("Pork","Poultry","Beef","Lamb & Goat","Fish","Eggs","Milk - inc. cheese"), "Animal Products", "Non-Animal Products"))%>%
    group_by(continent, food_class)%>%
    summarise(median_consumption=median(consumption),
              median_co2_emmission=median(co2_emmission))

myplot <- ggplot(data = mydata)+
    geom_point(aes(median_consumption, median_co2_emmission, col=food_class, shape=continent), size=6)+
    scale_shape()+
    theme_minimal()+
    labs(x="CO2 Consumption/person/year [Median]", y="CO2 Emission/person/year [Median]", 
         title="Relationship between CO2 Consumption and Emission",
         subtitle = "Median Values Calculated by Continent and Food Class",
         caption = "Github: @johnmutiso")+
    scale_color_manual(values = c("red","darkgreen"))+
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.29),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.text = element_text(face = "bold", colour = "grey30"),
          axis.ticks = element_blank(), panel.background = element_rect(fill = "grey88"), 
          panel.grid = element_blank())

ggsave(filename = "week 8.png", device = "jpeg", plot = myplot, width = 6.3, height = 6)

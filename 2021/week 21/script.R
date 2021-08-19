#' -------------------------------------------------------------------------------
#' Ask a Manager Survey
#' The data this week comes from [Ask a Manager Survey](https://docs.google.com/spreadsheets/d/1IPS5dBSGtwYVbjsfbaMCYIWnOuRmJcbequohNxCyGVw/edit?resourcekey#gid=1625408792)  via [Kaija Gahm] (https://github.com/rfordatascience/tidytuesday/issues/340)
#' This [some findings from 24,000 people's salaries](https://www.askamanager.org/2021/05/some-findings-from-24000-peoples-salaries.html) talks about Ask a Manager Survey over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 21
data_source <- "Ask a Manager Survey via Kaija Gahm"
#libraries -----------------------------------------------------------------------------
library(pacman)
p_load(tidyverse, extrafont, patchwork, stringr, lubridate)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2021, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

survey_new <- 
  survey %>% 
  mutate(timestamp1 = as_date(timestamp, format="%m/%d/%Y"))

survey_new %>% 
  group_by(timestamp1, industry) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
  
# plot
plot <- 
  ggplot() +
  labs(fill = '', x = "", y = "",
       caption = paste0("**Graphic:** #TidyTuesday week",week_num, "<br> **Data:** ",data_source, "<br> **GitHub:** @johnmutiso"))  +
  theme(plot.caption=ggtext::element_markdown())

# save plot
ggsave(
    plot = plot,
    height = 8.8,
    width = 3.5,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')
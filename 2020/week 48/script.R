#' -------------------------------------------------------------------------------
#' Washington Hiking
#' The data this week comes from [Washington Trails Association](https://www.wta.org/go-outside/hikes?b_start:int=1).
#' This [ARTICLE_SOURCE](LINK_TO_ARTICLE) talks about Washington Hiking in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 48
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# # download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)
# write_rds(tuesdata$hike_data, "2020/week 48/data/hike_data.rds")

#source('read_TT_data.R')
#read_TTdata(week = week_num)

hike_data <- read_rds("2020/week 48/data/hike_data.rds")

# manage the data
hikedata_new <-
    hike_data %>%
    mutate(
        length_miles = str_extract(length, "\\d+\\.\\d+|\\d+") %>% as.numeric,
        rating = as.numeric(rating),
        gain = as.numeric(gain),
        highpoint = as.numeric(highpoint)
    ) %>%
    # filter(!str_detect(length, "miles")) #check if there other length measurement other than miles
    filter(features != 'character(0)')

# transform data to long with, an observation per feature

hikedata_trans <- data.frame()

n_obs <- dim(hikedata_new)[1]

for (i in 1:n_obs) {
    n_ft <- hikedata_new$features[[i]] %>% length() # n features per
    
    for (j in 1:n_ft) {
        hikedata_trans <-
            hikedata_new %>%
            select(-features) %>%
            slice(i) %>%
            mutate(features = hikedata_new$features[[i]][j]) %>%
            bind_rows(hikedata_trans)
    }
    
    
}

# Median Ratings by Features
ratings_data <- 
    hikedata_trans %>% 
    group_by(features) %>% 
    summarise(median_rating = round(median(rating))) %>% 
    ungroup() %>% 
    mutate(r_stars = strrep("*", median_rating)) %>% 
    select(features, r_stars)

hikedata_trans <- 
    hikedata_trans %>% 
    left_join(ratings_data, by = "features")


# plot
plot <-
    hikedata_trans %>%
    mutate(features = fct_reorder(features, gain, .fun = 'median')) %>%
    ggplot(aes(gain)) +
    geom_boxplot(aes(y = features), fill = 'snow1', col = 'grey40') +
    geom_text(aes(label = str_squish(r_stars), x = 5, y = features), col = 'yellow', hjust = 0, size = 9, nudge_y = -0.18) +
    stat_summary(
        fun = median,
        geom = 'point',
        aes(
            x = highpoint,
            y = features,
            shape = "Highest Point (Median)"
        ),
        col = 'steelblue4',
        #shape = 8,
        size = 4
    ) + # average highest point above sea - level
    scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10 ^ x),
        labels = scales::trans_format("log10", scales::math_format(10^.x)), expand = c(0.007,0.007)
    ) +
    annotation_logticks(sides = 'b', outside = T) +
    coord_cartesian(clip = 'off') +
    labs(
        caption = "\n\nGraphic: #TidyTuesday week 48\nGitHub: @johnmutiso\nData:  Washington Trails Association courtesy of the TidyX crew, Ellis Hughes and Patrick Ward",
        title = "Hiking in Washington",
        subtitle = "",
        x = "Gain in Elevation (ft above sea level)",
        y = "Features"
    ) +
    scale_shape_manual("", values = c("Highest Point (Median)" = 8)) +
    theme_minimal() +
    theme(
        axis.text = element_text(
            size = 13,
            face = 'bold',
            family = 'Calibri'
        ),
        panel.grid.major.y = element_line(
            color = 'grey70',
            size = 11,
            arrow = arrow(
                ends = 'first',
                type = 'open',
                length = unit(0.5, 'cm')
            )
        ),
        legend.position = 'top',
        axis.title = element_text(size = 16, color = 'steelblue'),
        plot.title = element_text(
            size = 24,
            face = 'bold',
            family = 'Tahoma'
        ),
        plot.caption = element_text(color = 'grey60')
    )


# save plot
ggsave(
    plot = plot,
    height = 8.8,
    width = 9.5,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

# Courtesy of  TidyX crew(https://github.com/thebioengineer/TidyX/tree/master/TidyTuesday_Explained/035-Rectangles),
# Ellis Hughes(https://twitter.com/Ellis_hughes) and Patrick Ward (https://twitter.com/OSPpatrick)

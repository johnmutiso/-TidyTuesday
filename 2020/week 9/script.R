
# Header ------------------------------------------------------------------
#'Author: John Mutiso
#'Twitter: @johnmutiso_
#'Github: @johnmutiso

# Reading Data ------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(patchwork)
tuesdata <- tidytuesdayR::tt_load(2020, week = 9)


measles <- tuesdata$measles %>% 
    filter(mmr > 0 & type != "NA") %>% 
    group_by(state,county,type) %>% 
    summarise(ave_mmr = mean(mmr)) %>% 
    group_by(state) %>% 
    arrange(ave_mmr) %>% 
    mutate(seq = 1:n(),
           mmr_cat = case_when(ave_mmr < 50 ~ 1,
                               between(ave_mmr, 50, 75) ~ 2,
                               between(ave_mmr, 75, 85) ~ 3,
                               between(ave_mmr, 85, 90) ~ 4,
                               between(ave_mmr, 90, 95) ~ 5,
                               ave_mmr > 95 ~ 6),
           mmr_cat = factor(mmr_cat, levels = c(1:6),
                            labels = c("< 50", "50 - 75", "75 - 85", "85 - 90", "90 - 95", "> 95"))) %>% 
    ungroup() %>% 
    mutate(state = factor(state),
           county = factor(county),
           seq = factor(seq))

#tile plot


    
measles_mmr <- function(State="New York", legend.pos) {
     measles %>% 
        filter(state == State,
               type %in% c("Public", "Private")) %>% 
        ggplot() +
        geom_bin2d(aes(county, type, fill = mmr_cat), col = "white", ) +
        scale_fill_brewer(palette  = 1) + 
        facet_wrap(.~state, ncol = 4, 
                   scales = "free", 
                   strip.position = "right") +
        theme_fivethirtyeight() +
        theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.3, 
                                         face = 'bold', family = 'monospace', 
                                         color = "bisque4", size = 8),
              axis.text.y = element_text(size = 9, face = 'bold',
                                         family = 'monospace', vjust = 0.5),
              panel.grid.major = element_line(size = 15), 
              panel.background = element_rect(fill = 'white'),
              strip.text = element_text(face = 'bold', 
                                        color = "gray30", size = 11), 
              axis.ticks = element_blank(), axis.line = element_blank(), 
              legend.title = element_blank(), axis.title = element_blank(),
              legend.position = legend.pos, legend.direction = 'horizontal') 
}


# Combining the plots using 'patchwork' features
final_plot <- measles_mmr('New York','none')/measles_mmr('Ohio', 'bottom') +
    plot_annotation(title = "Average Measles Vaccination Rate per County (Public vs Private Schools)", subtitle = "New York and Ohio States", 
                    caption = "The data was completed by staff of The Wall Street Journal: Dylan Moriarty, Taylor Umlauf, & Brianna Abbot \nGithub: @johnmutiso")

ggsave(filename = "week9plot.jpeg", device = 'jpeg', path = '2020/week 9/',plot = final_plot, dpi = 500, height = 5.5, width = 10)
# INPUT THE SUBJECT TITLE OF THE DATASET

#' The data this week[17] comes from [Privacy Affairs](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md). 

#' This [Roel Hogervorst Article](https://blog.rmhogervorst.nl/blog/2020/04/08/scraping-gdpr-fines/) talks about [Scraping Gdpr Fines] in greater detail.

#' Credit: [John Mutua](Twiter: @johnmutiso_)


# Library ------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(extrafont)
#fonts
windowsFonts(Jokerman = windowsFont('Jokerman'))
windowsFonts(Gabriola = windowsFont('Gabriola'))
windowsFonts(Papyrus = windowsFont('Papyrus'))
# data ---------------------------------------------------------------
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

# Article Violated
gdpr_violations_1 <- 
    gdpr_violations %>% 
    mutate(article = str_extract_all(article_violated, "\\d{1,}[^(\\d{1,})]")) %>% #extract articles violated
    unnest(cols = c(article)) %>% 
    mutate(article = str_squish(article),
           article_n = as.numeric(article)) %>% 
    arrange(name, article_n)

# Fine by country
data_x <- 
    gdpr_violations_1 %>% 
    group_by(name) %>% 
    summarise(mean_country = mean(price))
# Article violated plot by country
plot <- 
gdpr_violations_1 %>% 
    group_by(name, article, article_n) %>% 
    summarise(mean_price = mean(price), n = n()) %>% 
    left_join(data_x, by = 'name') %>% 
    ggplot() +
    geom_point(aes(reorder(article, article_n), reorder(name, mean_country), fill = log10(mean_price), size = n), 
               shape = 21, alpha = 0.7, col = '#ffffff') +
    scale_size_area("Number of\nArticles\nViolated", max_size = 16, breaks = c(1,5,10,20,30)) +
    scale_fill_gradientn("Fine Price\n(million ???)", colours = c('#ffffff', '#FF0000'),
                         labels = c(0.001,0.01,0.1,1,10,100)) +
    scale_color_gradientn(colours = c('#ffffff', '#FF0000')) +
    labs(title = "<span style='color:#dddddd'> General Data Protection Regulation (GDPR) VIOLATIONS </span>", 
         x = "Violated Article #",
         caption = "Github: @johnmutiso\nData: From Roel Hogervorst via Bob Rudis[twitter: @hrbrmstr]\nGraphic: 2020-week 17 TidyTuesday") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = '#133337'), 
          axis.text = element_text(color = '#ddfff1', family = 'Gabriola', size = 16), 
          text = element_text(color = '#ddfff1', family = 'Gabriola', size = 16), 
          panel.grid = element_line(size = 0.002, color = '#065535'), 
          plot.title = element_markdown(size = 18, family = 'Jokerman'), 
          axis.title.y = element_blank(), 
          plot.caption = element_text(family = 'Papyrus', size = 10, colour = '#00ff7f'),
          legend.text = element_text(size = 18))
  
# Save the plot
ggsave(plot = plot,
       path = './2020/week 17/',
       width = 11,
       height = 7,
       dpi = 400, 
       device = 'png', 
       filename = 'week17plot.png')

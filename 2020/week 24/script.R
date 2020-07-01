#' -------------------------------------------------------------------------------
#' --African American Achievements--
#' The data this week comes from Wikipedia Courtesy(https://en.wikipedia.org/wiki/List_of_African-American_inventors_and_scientists &
#'   https://en.wikipedia.org/wiki/List_of_African-American_firsts) emphasis of [#BlackLivesMatter].
#' The article for this week is the obituary for David Blackwell - Fought racism; became world famous statistician
#'   (https://www.stltoday.com/news/local/obituaries/david-blackwell-fought-racism-became-world-famous-statistician/article_8ea41058-5f35-5afa-9c3a-007200c5c179.html).
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

# # libraries ---------------------------------------------------------------------

library(tidyverse)
library(magick)
library(extrafont)
loadfonts(device = 'win')

# data ----------------------------------------------------------------------------

# tuesdata <- tidytuesdayR::tt_load(2020, week = 24)
# science <- tuesdata$science
# readr::write_csv(science, '2020/week 24/data/science.csv')

Wilkins_data <-
    readr::read_csv('2020/week 24/data/science.csv') %>%
    filter(str_detect(name, 'Wilkins'))

Wilkins_ <-
    'https://upload.wikimedia.org/wikipedia/commons/d/d9/J._Ernest_Wilkins%2C_Jr._9.jpg'

Wilkins <- image_read(Wilkins_)

plot <-
    ggplot(NULL) +
    geom_point(aes(x = 1:10, y = 1:10), col = '#133337') +
    annotation_raster(
        Wilkins,
        xmin = 0,
        xmax = 4,
        ymin = 0,
        ymax = 10,
        interpolate = T
    ) +
    geom_text(
        aes(
            x = 4.5,
            y = 9.5,
            label = Wilkins_data[[1]]
        ),
        family = 'Bauhaus 93',
        size = 12,
        col = '#bada55',
        hjust = 0
    ) +
    geom_text(
        aes(
            x = 4.5,
            y = 8,
            label = paste0('RACE:\n        ', 'Black-American')
        ),
        family = 'Arial Rounded MT Bold',
        size = 8,
        col = '#5ac18e',
        hjust = 0
    ) +
    geom_text(
        aes(
            x = 4.5,
            y = 6.4,
            label = paste0(
                "CAREER:\n       ",
                Wilkins_data[[4]] %>%
                    str_to_title() %>% str_replace_all(';', ';\n      ')
            )
        ),
        family = 'OCR A Extended',
        size = 6,
        hjust = 0,
        color = 'snow1'
    ) +
    geom_text(
        aes(
            x = 4.5,
            y = 4,
            label = paste0(
                "ACHIEVEMENTS:\n         ",
                Wilkins_data[[5]] %>%
                    str_to_title() %>% str_replace_all(';', ';\n        ')
            )
        ),
        family = 'OCR A Extended',
        size = 6,
        hjust = 0,
        color = 'snow1'
    ) +
    geom_text(
        aes(
            x = 4.5,
            y = 2,
            label = paste0('     ', Wilkins_data[[2]],' - : - : - : - ',Wilkins_data[[3]])
        ),
        family = 'Matura MT Script Capitals',
        size = 12,
        col = '#407294',
        hjust = 0
    ) +
    geom_text(
        aes(
            x = 4.5,
            y = 1.2,
            label = '<<<<<      #BlackLivesMatter      >>>>>'
        ),
        family = 'Harrington',
        size = 12,
        col = '#daa520',
        hjust = 0
    )+
    labs(caption = 'Github: @johnmutiso\nData: @Wikipedia\nPicture: @Wikipedia\nGraphic: 2020-week 24 TidyTuesday')+
    theme_void() +
    theme(plot.background = element_rect(fill = '#133337'),
          plot.caption = element_text(color='#cbbeb5',family='Tahoma'))

# save plot
ggsave(
    plot = plot,
    height = 8,
    width = 14.5,
    device = 'png',
    filename = 'week24plot.png',
    path = './2020/week 24/'
)


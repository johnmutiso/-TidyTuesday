#' -------------------------------------------------------------------------------
#' Women of 2020
#' The data this week comes from [BBC](https://www.bbc.com/news/world-55042935) via [Joshua Feldman](@joshuafeidman).
#' This [BBC 100 Women 2020: Who is on the list this year?](https://www.bbc.com/news/world-55042935) 
#'   talks about Women of 2020 in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 50
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num)

img_link <- c()
# download the png files
for (i in 1:nrow(women)) {
    png_file <- paste0('2020/week 50/png_files/',
                       str_remove_all(women$name[[i]], ' '),
                       '.png')
    
    if (file.exists(png_file)==0)
        download.file(url = women$img[[i]],
                      destfile = png_file,
                      quiet = T, method='auto', mode='wb')
    img_link[i] = png_file
}


# Data
women <-
    women %>%
    mutate(rows = rep(seq(1, 96, by=12), 13)[1:100],
           cols = sort(rep(seq(1,39, by=3), 8))[1:100], 
           id = row_number(),
           img_local=img_link)


# plot
plot <-
    women %>%
    ggplot(aes(rows, cols)) +
    ggimage::geom_image(aes(image=img_local), size = 1/33) +
    geom_point(
        size = 20,
        stroke = 9,
        pch = 21,
        color = 'snow1'
    ) +
    geom_point(
        size = 20,
        stroke = 1.5,
        pch = 21,
        color = '#ffc100'
    ) +
    geom_point(
        size = 30,
        stroke = 1.5,
        pch = 21,
        color = '#ff0000'
    ) +
    ggtext::geom_textbox(
        aes(
            label = paste0(
                '<span style="font-family: Tahoma;font-size:11px;margin:0 0 30px 0">',
                "**",
                id, ": *", name, "* (", str_squish(country),
                "**)<br />",
                '<span style="font-family: Arial;font-size:9.5px;color:darkred">**',
                str_squish(role),
                '** </span> <p style="font-size:7px">',
                description, '</p>'
            )
        ),
        hjust = 0,
        nudge_x = 1.4,
        nudge_y = 0.1,
        vjust = 0.4,
        width=unit(1.84, 'inch')
    ) +
    labs(title="Women of 2020",
         caption = 'Credit: @johnmutiso_\nData: BBC via Joshua  Feldman(@joshuafeidman)\nGraphic: #TidyTuesday week 50') +
    coord_cartesian() +
    theme_void() +
    xlim(c(1, 93)) + scale_y_reverse() +
    theme(plot.background = element_rect(fill = '#576675'),
          plot.title=element_text(color='grey70', family='Lucida Handwriting', size=24))

# save plot
ggsave(
    plot = plot,
    height = 18,
    width = 22,#dpi = 290,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

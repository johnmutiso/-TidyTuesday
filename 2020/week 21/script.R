#' -------------------------------------------------------------------------------
#' BEACH VOLLEYBALL
#' The data this week comes from [Adam Vagnar](https://github.com/rfordatascience/tidytuesday/issues/62).
#' This [BigTime Stats article](https://bigtimestats.blog/2019/09/15/avp-stat-trends/) talks about Beach Volleyball in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(ggtext)
loadfonts(device = 'win')

# Load data ----------------------------------------------------------------------------
# vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

# save the data -------------------------------------------------------------------------
# readr::write_csv(vb_matches, path = './2020/week 21/data/vb_matches.csv', col_names = T)

# data ----------------------------------------------------------------------------------
vb_matches <- readr::read_csv('./2020/week 21/data/vb_matches.csv', progress = T)

vb_matches_2 <-
    vb_matches %>% 
    select(1:33) %>% 
    # select only completecases
    drop_na()

# Plot --------------------------------------------------------------------------------------------
plot <- 
vb_matches_2 %>%
    pivot_longer(cols = ends_with('age'),
                 names_to = 'W_L_player',
                 values_to = 'age') %>%
    mutate(player_cat =
               factor(
                   case_when(
                       W_L_player %in% 'w_p1_age' ~ 1,
                       W_L_player %in% 'w_p2_age' ~ 2,
                       W_L_player %in% 'l_p1_age' ~ 3,
                       W_L_player %in% 'l_p2_age' ~ 4
                   ),
                   levels = 1:4,
                   labels = c('Winner 1', 'Winner 2', 'Losser 1', 'Looser 2')
               ),
           gender = factor(gender, levels = c('M','W'), labels = c('Men Teams', 'Women Teams'))) %>%
    ggplot(aes(x = factor(year))) +
    facet_wrap(gender ~ ., nrow = 2) +
    geom_boxplot(aes(y = age, fill = player_cat, color = player_cat), 
                 alpha = 0.6, outlier.shape = 21) +
    geom_hline(yintercept = c(25.12, 28.36, 31.88),
               lty = c(4, 1, 4, 4, 1, 4),
               size = 0.8, color = '#003366') +
    scale_fill_manual(
        'Players',
        values = c('#36802d', '#77ab59', '#ff0000', '#ff5252'),
        aesthetics = c('color', 'fill')
    ) +
    labs(
        title = 'BEACH VOLLEYBALL',
        subtitle = "Age distribution for 
        <span style='color:#36802d;'> Winner Player 1</span>, 
        <span style='color:#77ab59;'>Winner Player 2</span> and 
        <span style='color:#ff0000;'>Looser Player 1</span>,
        <span style='color:#ff5252;'>Looser Player 2</span>\n",
        y = 'Age (Years)',
        caption = 'Github: @johnmutiso\nData: FIVB tournaments & AVP via Adam Vagnar @BigTimeStats\nGraphic: 2020-week 21 TidyTuesday',
        tag = '*The Horizontal lines represent the overall age(yrs) distribution; Median = 28.36, 1st Quartile = 25.12, 3rd Quartile = 31.88'
    ) +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 14),
          legend.position = 'none',
          plot.tag.position = c(0.5,0.45),
          plot.tag = element_text(color = '#003366', family = 'Arial', size = 12, face = 'bold'),
          axis.text = element_text(color = '#407294', size = 13, face = 'bold', family = 'Courier New'),
          plot.background = element_rect(fill = '#f5f5dc', linetype = 1, color = '#407294', size = 1), 
          strip.text = element_text(family = 'Cooper Black', size =  14, color = '#003366'),
          plot.title = element_text(size = 20, color = '#003366', family = 'Bauhaus 93', hjust = 0.5),
          plot.subtitle = element_markdown(size = 14, family = 'Arial', face = 'bold', hjust = 0.5),
          plot.caption = element_text(color = '#407294', face = 'bold'))

# save the plot --
ggsave(
    plot = plot,
    height = 10,
    width = 12,
    dpi = 500,
    device = 'png',
    filename = 'week21plot.png',
    path = './2020/week 21/'
)

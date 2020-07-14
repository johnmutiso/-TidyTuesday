#' -------------------------------------------------------------------------------
#' Uncanny X-men
#' The data this week comes from [ Claremont Run Project and Malcom Barret](http://www.claremontrun.com/, https://twitter.com/malco_barrett).
#' This [Wikipedia - Uncanny X-Men](https://en.wikipedia.org/wiki/Uncanny_X-Men) talks about Uncanny X-Men in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_, Github: @johnmutiso)
#' Data: @ClaremontRun via Malcom Barret
#' --------------------------------------------------------------------------------

#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

week_num = 27

# Read data
source('read_TT_data.R')
read_TTdata(week = week_num)


character_ <- 
character %>% 
    group_by(series, pass_bechdel) %>% 
    summarise(pass = n()) %>% 
    group_by(series) %>% 
    mutate(pass_perc =paste0(round(pass/sum(pass)*100, 1),'%')) %>% # Bechdel test pass %
    mutate(pass_perc = ifelse(pass_perc == '100%','0%',pass_perc)) %>% 
    filter(pass_bechdel == 'yes'|pass_perc == '0%') %>% 
    select(series, pass_perc) %>% 
    left_join(.,character, by = 'series') %>% 
    mutate(series = paste0(series, ' (',pass_perc, ')')) %>% 
    filter(!is.na(pass_bechdel))

#plot
plot <-
    character_ %>%
    ggplot(aes(issue,factor(1))) +
    geom_jitter(aes(col = pass_bechdel, shape = series), size = 5, width = 0, height = 0.5)+
    scale_shape_manual("Series (Bechdel Pass %)", values = seq(1,14,2)) +
    scale_x_continuous(breaks = seq(0,300, 20), expand = c(0,0), limits = c(0,300)) + 
    scale_color_manual('Passed Bechdel', values = c('#f7347a', '#ffff00')) +
    labs(title = 'Uncanny X-men - Bechdel Test', 
         subtitle = '**Bechdel test - Measure of the representation of women in fiction**\nAsks whether a work features at least two WOMEN who TALK to EACH OTHER about something OTHER THAN a MAN (#wikipedia)',
         caption = 'Graphic: TidyTuesday week 27\nCredit: @johnmutiso\nData:@ClaremontRun via Malcom Barret') +
    theme_minimal() +
    theme(plot.background = element_rect(fill = '#133337'),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 15, color = 'white'),
          axis.title.y = element_blank(), 
          axis.title.x = element_text(size = 17, color = 'snow1'),
          panel.grid.major.y = element_blank(), 
          legend.background = element_rect(fill = '#afeeee'), 
          legend.title = element_text(face='bold'),
          plot.title = element_text(size=22, face = 'bold', color = 'snow1', family = 'Tahoma'),
          plot.subtitle = element_text(color = '#ffff00', family = 'Segoe Print'),
          plot.caption = element_text(color = 'snow1', family = 'Tahoma', hjust=0))

# save plot
ggsave(
    plot = plot,
    height = 5,
    width = 15,
    dpi = 500,
    device = 'png',
    filename = paste0('week', week_num, 'plot.png'),
    path = paste0('2020/week ', week_num)
)

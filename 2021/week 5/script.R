#' -------------------------------------------------------------------------------
#' Plastic Pollution
#' The data this week comes from [ Break Free from Plastic courtesy of Sarah Sauve](https://www.breakfreefromplastic.org/). 
#' This [ARTICLE_SOURCE](https://github.com/sarahsauve/TidyTuesdays/blob/master/BFFPDashboard/BlogPost.md) talks about Plastic Pollution in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 5
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

plastics_new <- 
    plastics %>%
    filter(!str_detect(parent_company, 'Grand')) %>%
    group_by(country, year) %>% 
    summarise(grand_total=sum(grand_total)) %>% 
    pivot_wider(id_cols=1, names_from=2, values_from=3, names_prefix='yr') %>% 
    filter(!is.na(yr2019)&!is.na(yr2020)) %>% 
    mutate(diff=yr2020-yr2019, 
        hjust=case_when(diff<0 ~ 0,
            TRUE ~ 1),
        country = case_when(diff<0 ~ str_c(" ",country),
            TRUE ~ str_c(country, " ")))

# plot
plot <-
    plastics_new %>% 
    ggplot(aes(diff, reorder(country, diff), fill=diff<0)) +
    geom_bar(stat = 'identity', show.legend = F) +
    geom_text(aes(label=country, x=0), hjust=plastics_new$hjust, fontface='bold') +
    ggtext::geom_richtext(data=NULL, aes(x=27000,y=24),label='<b style="font-size:15px;color:#696966"><b style="color:#ffa500;font-size:55px">Plastic Pollution</b><br><br> <b style="font-family:Calibri;font-size:25px;color:#576675"> Comparing data for 2019 and 2020</b><br><br> <b style="color:#ff6347"> DETERIORATION</b> and <b style="color:#407294">IMPROVEMENT</b> based on Plastics Counts</b>', show.legend = F, fill='#eeeeee') +
    scale_x_continuous(labels = scales::number_format(scale = 0.001, accuracy = 1), n.breaks = 10) +
    scale_fill_manual(values = c('#ff6347','#407294'))+
    labs(x = "Change in total plastic Count ('000s)",
        caption = "Graphic: #TidyTuesday week 5\nData: breakfreefromplastic.org/ via @sarah_sauve\nGithub: @johnmutiso")+
    theme_bw() +
    theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size=14), 
        axis.title.x = element_text(size = 16, color='grey40'))

# save plot
ggsave(
    plot = plot,
    height = 10,
    width =8.5,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

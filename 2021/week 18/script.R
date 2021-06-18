#' -------------------------------------------------------------------------------
#' CEO Departures
#' The data this week comes from [Gentry et al.](https://onlinelibrary.wiley.com/doi/abs/10.1002/smj.3278) [by way of DataIsPlural](https://www.data-is-plural.com/archive/2021-04-21-edition/)  
#' This [investors.com](https://www.investors.com/news/ceo-turnover-bailing-out-droves/) talks about CEO Departures over time in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 18
data_source <- "Gentry et al. by way of DataIsPlural"
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(ggtext)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

#departure codes
d_codes <-
"
 1 - Involuntary - CEO death<br>
 2 - Involuntary - CEO illness<br>
 _**3 - Involuntary - CEO dismissed for job performance<br>
 4 - Involuntary - CEO dismissed for legal violations or concerns**_<br>
 5 - Voluntary - CEO retired<br>
 6 - Voluntary - new opportunity (new career driven succession)<br>
 7 - Other<br>
 8 - Missing<br>
 9 - Execucomp error
"

# plot
plot <- 
  departures %>% 
    filter(!is.na(departure_code)) %>% 
    mutate(max_tenure_ceodb=factor(max_tenure_ceodb, ordered = T),
           departure_code=factor(departure_code, levels = 9:1)) %>% 
  ggplot(aes(fyear, departure_code)) +
    geom_point(aes(fill=max_tenure_ceodb, size=max_tenure_ceodb, col=max_tenure_ceodb), pch=21, alpha=0.7) +
    #facet_wrap(.~departure_code, ncol=1, scales = 'free_y') +
    scale_fill_viridis_d(aesthetics = c('color','fill'), ) +
    scale_x_continuous(n.breaks = 20)+
    scale_size_manual(values = c(1,2,4,6))+
  labs( x = "", y = "Departure Code", fill="", col="", size="",
        title="CEO Departures",
        subtitle = paste0("Number of CEO Tenures<br><br>(_**codes (3,4) are non-voluntary, non-health related**_)<br>", d_codes),
       caption = paste0("**Graphic:** #TidyTuesday week ",week_num, "<br> **Data:** ",data_source, "<br> **GitHub:** @johnmutiso"))  +
    theme_minimal()+
  theme(plot.caption=ggtext::element_markdown(), strip.text = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(size=7, color='grey90'),
        plot.background = element_rect(fill='snow1'),
        plot.title = element_text(size=25, face = 'bold'),
        plot.subtitle = ggtext::element_markdown(size=12, family = 'Arial'))
ggsave(
    plot = plot,
    height = 5,
    width = 9,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

# remove history file
file.remove('.Rhistory')
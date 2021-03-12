#' -------------------------------------------------------------------------------
#' College Enrollment
#' The data this week comes from [ Data.World ](https://data.world/nces/hbcu-fall-enrollment-1976-2015). 
#' This [HBCU Donations Article](https://theundefeated.com/features/how-hbcus-are-using-more-than-250-million-in-donations/) talks about College Enrollment in greater detail.
#' Credit: [John Mutiso](twitter: @johnmutiso_)
#' --------------------------------------------------------------------------------

rm(list = ls())
week_num = 6
#libraries -----------------------------------------------------------------------------
library(tidyverse)
library(extrafont)
library(patchwork)
loadfonts(device = 'win')

# download data from
# tuesdata <- tidytuesdayR::tt_load(2020, week = week_num)

source('read_TT_data.R')
read_TTdata(week = week_num, year=2021)

for(gender in c('male','female')) {
    
    plot_title <- paste0('Proportions of ', str_to_sentence(gender), ' Students Aged above 25 years\n(with 95% CIs)')
    x_lab <- paste0("\nProportion of ",str_to_sentence(gender), " Students")
    # plot
    plot <- 
    get(paste0(gender, '_bach_students')) %>% 
        rename(year=1, all_per=2, all_se=3, white=4, white_se=5, black=6, black_se=7, hisp=8, hisp_se=9) %>% 
        mutate_at(.vars = -1, .funs=as.double) %>% 
        select(1:9) %>% 
        pivot_longer(cols = c(2,4,6,8), names_to='stat1', values_to='per') %>% 
        pivot_longer(cols=2:5, names_to='stat2', values_to='se') %>% 
        filter(str_remove(stat1,'_per')==str_remove(stat2,'_se')) %>% 
        #plot
        ggplot(aes(per, reorder(as.factor(year), desc(year)),  col=stat1)) + 
        geom_point(position = position_dodge(0.7), size=1, pch=20, stroke=2) +
        geom_errorbar(aes(xmin=per-(1.96*se), xmax=per+(1.96*se)),position = position_dodge(0.7), size=1) +
        scale_x_continuous(n.breaks = 10, expand = c(0.05,0), labels = scales::unit_format(suffix = '%'))+
        scale_color_brewer(labels=c('Overall', 'Blacks','Hispanic','Whites'), palette = 'Set2')+
        facet_grid(year~., scales = 'free_y')+
        labs(col="",
            x=x_lab,
            title=plot_title)+
        theme_bw() +
        theme(panel.grid.major.y = element_blank(), 
            strip.text = element_blank(), panel.border = element_rect(color='grey88'),
            axis.ticks.y = element_blank(), 
            axis.text = element_text(size = 11),
            axis.title.y = element_blank(),
            plot.background = element_rect(fill='grey95'),
            legend.position = 'top',
            plot.title = element_text(size=16, family = 'Nirmala UI', hjust = 0.5)) 
    
    assign(paste0('plot_', gender), plot)
}

# using patchwork packae to combine the final plots for the for loop above
plot_final <- 
    plot_female+plot_male + 
    plot_annotation(title = 'HCBU Enrollments', 
        caption = 'HCBU: historically Black colleges and universities\n\n Graphic: #TidyTuesday week 6\nData: Data.World\n Github: @johnmutiso',
        theme = theme(plot.title = element_text(size = 20, hjust = 0.5, face = 'bold', family = 'Arial')))

# save plot
ggsave(
    plot = plot_final,
    height = 12,
    width = 13,
    dpi = 500,
    device = 'png',
    filename = paste0('week',week_num,'plot.png'),
    path = paste0('2021/week ', week_num)
)

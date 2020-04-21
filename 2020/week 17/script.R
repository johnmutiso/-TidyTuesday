# INPUT THE SUBJECT TITLE OF THE DATASET

#' The data this week[17] comes from [Privacy Affairs](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md). 

#' This [Roel Hogervorst Article](https://blog.rmhogervorst.nl/blog/2020/04/08/scraping-gdpr-fines/) talks about [Scraping Gdpr Fines] in greater detail.

#' Credit: [John Mutua](Twiter: @johnmutiso_)


# Library ------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
# data ---------------------------------------------------------------
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

setwd('~/Documents/CAL/Real_Life/Repository/Netflix/')
options(stringsAsFactors = F)
library(stringr)
library(ggplot2)
library(data.table)
library(plyr)
library(stringi)
library(ggrepel)
library(ggthemes)
source('netprocess.R')
netflix <- read.csv('NetflixViewingHistory.csv')
netflix$Date <- as.Date(netflix$Date, format = '%m/%d/%y')
#set order, reverse order
setDT(netflix)
netflix[(Date == '2017-04-17')]$Title <- 'Tropic Thunder'
netflix <- preprocess_netflix(netflix)
title_values <- strsplit(netflix$Title, ':')

netflix_counts <- data.frame(table(netflix$Name))
names(netflix_counts) <-
  mapvalues(names(netflix_counts), from = 'Var1', to = 'Title')
artifact <- fread('netflix_artifact.csv')
netflix_counts <- merge(netflix_counts, artifact, by='Title', all.x=T)
netflix_counts$Type <-
  ifelse(netflix_counts$Freq > 1, 'Series' , 'Movie')
netflix_counts$Type <- ifelse(
  netflix_counts$Title %in%
    c(
      'Donald Glover',
      'Ali Wong',
      'COMEDIANS of the world',
      'Patriot Act with Hasan Minhaj',
      'Historical Roasts',
      'My Next Guest Needs No Introduction With David Letterman'
    ),
  'Comedy Specials',
  netflix_counts$Type
)
netflix_counts$Type <- ifelse(
  netflix_counts$Title %in%
    c(
      'FYRE',
      'American Factory',
      "Inside Bill's Brain",
      'Amazing Hotels',
      'Icarus',
      'Rotten',
      'Killer Inside',
      'Tiger King',
      'Jeffrey Epstein',
      'The Last Dance'
    ),
  'Documentary',
  netflix_counts$Type
)
netflix_counts$Type <- ifelse(
  netflix_counts$Title %in%
    c(
      'Family Guy',
      'Tidying Up with Marie Kondo',
      'Orange is the New Black',
      'Always a Witch'
    ),
  'Series',
  netflix_counts$Type
)

foreign_language_df <-
  data.frame(
    Title = c(
      'Babylon Berlin',
      'Narcos',
      'Money Heist',
      'ROMA',
      'This Is Not What I Expected',
      "Marseille",
      'A Love So Beautiful',
      'Crash Landing on You',
      'Fauda',
      'Dark',
      'Lupin',
      'Taco Chronicles',
      'Vincenzo',
      'Squid Game',
      'Club de Cuervos'
    ),
    Language = c(
      'German',
      'Spanish',
      'Spanish',
      'Spanish',
      'Mandarin',
      'French',
      'Mandarin',
      'Korean',
      'Hebrew & Arabic',
      'German',
      'French',
      'Spanish',
      'Korean',
      'Korean',
      'Spanish'
    )
  )
netflix_counts <-
  merge(netflix_counts,
        foreign_language_df,
        by = 'Title',
        all.x = T)
netflix_counts$Language[is.na(netflix_counts$Language)] <- 'English'

netflix_counts$Cal <- T
netflix_counts$Cal <-
  ifelse(
    netflix_counts$Title %in% c(
      'American Horror Story',
      'Always a Witch',
      'Californication',
      'Orange Is the New Black',
      'Donnie Darko',
      'Love Actually',
      "Inside Bill's Brain",
      'Icarus',
      "Perú",
      '',
      'Beyond Stranger Things',
      'Ozark',
      'Breaking Bad'
    ),
    F,
    netflix_counts$Cal
  )

netflix_all <-
  merge(netflix, netflix_counts, by.x = 'Name', by.y = 'Title')
setDT(netflix_all)
netflix_all$Type <- factor(netflix_all$Type,
                           levels = c('Series', 'Movie', 'Documentary', 'Comedy Specials'))
today <- Sys.Date()
corona_lockdown <- as.Date('2020-03-12')
name_wrap <- function(name, width = 40) {
  wrapped_name <- paste(stri_wrap(name, width = width), collapse = '\n')
  return(wrapped_name)
}

netflix_all$Name <- sapply(netflix_all$Name, name_wrap)
netflix_ts <- netflix_all[Cal == T, .(Episodes = .N),
                          by = c('Date', 'Name', 'Language', 'Type')]
netflix_ts$Name <-
  factor(netflix_ts$Name, levels = unique(netflix_ts[order(Date)]$Name))

ggplot(netflix_ts) + geom_point(
  aes(
    x = Date,
    y = Name,
    fill = Language,
    size = Episodes
  ),
  shape = 21,
  alpha = 0.6
) +
  facet_grid(Type ~ ., scales = 'free', space = 'free') +
  geom_vline(aes(xintercept = corona_lockdown), linetype = 'dashed') +
  scale_fill_brewer(palette = 'Set1') +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text.y = element_text(angle = 0, size = 14),
    strip.background = element_rect(fill = 'dark red'),
    strip.placement = 'bottom',
    axix.text.y = element_text(size=4),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = 'black')
  ) +
  ggtitle("Cal's Netflix History")
ggsave(paste0('Netflix_history_', today, '.jpeg'),
       width = 11,
       height = 6)

ggplot(netflix_ts[Type == 'Movie']) +
  geom_point(aes(x = Date, y = 'Movie', fill = Language),
             shape = 21,
             alpha = 0.6) +
  geom_text_repel(aes(x = Date, y = 'Movie', label = Name), direction =
                    'y')

day_count <- netflix[, .(n = .N), by = 'Date']

#histogram
netflix <-
  merge(netflix, netflix_counts[, c('Title', 'Type')], by.x = 'Name', by.y =
          'Title')
ggplot(netflix) + geom_histogram(aes(x = Date, fill = Type), color = 'black', bins = 180) +
  geom_vline(aes(xintercept = corona_lockdown), linetype = 'dashed') +
  scale_fill_brewer(palette = 'Spectral') +
  ggtitle('Netflix Volume Consumption') +
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('Netflix_histogram_', today, '.jpeg'),
       width = 11,
       height = 6)

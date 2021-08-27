library(data.table)
library(plyr)
library(stringi)

preprocess_netflix <- function(df, title_col='Title'){
  title_values <- strsplit(df[[title_col]], ':')
  df$Name <- unlist(lapply(title_values, function(x)
    x[1]))
  df$Secondary <- unlist(lapply(title_values, function(x)
    x[2]))
  
  df$Name <-
    factor(df$Name, levels = rev(unique(df$Name)))
  return(df)
}
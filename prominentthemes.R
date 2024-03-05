library(igraph)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
library(dplyr)
library(igraph)
library(stringr)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(RColorBrewer)

setwd("D:/University/SOCY2169/assignment 4")
g <- read.graph("ukrainerussia.graphml",format="graphml")

df <- data.frame(
  status_id=E(g)$user_id,
  text=tail_of(g,E(g))$vosonTxt_tweet)

#text pre-processing
df$text <- gsub("http.*","",  df$text)              #remove URLs (https://...)
df$text <- gsub("https.*","",  df$text)             #remove URLs (http://...)
df$text <- gsub("@\\w+", "", df$text)               #remove usernames
df$text <- gsub("#\\w+","",  df$text)              #remove hashtags
df$text <- gsub("[[:digit:]]", "", df$text)         #remove numbers

#tokens and frequency
custom <- read.csv("removelist.csv",header = T)
custom$lexicon <- 'custom'
stopwords2 <-  rbind(get_stopwords(),custom)

df2 <- df %>% unnest_tokens(word, text, token = "ngrams",n=2) %>%
  anti_join(stopwords2) %>%
  count(word, sort=T) 

df2 <- df2[-c(1),]
rownames(df2) <- NULL 

#custom wordcloud
wordcloud2(df2,shape = "pentagon")

#-------------------------------------------------------------------------------

df2[1:10,]

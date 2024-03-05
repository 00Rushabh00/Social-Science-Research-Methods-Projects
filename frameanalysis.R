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

#reading stopwords from csv file to keep code cleaner
custom <- read.csv("removelist.csv",header = T)
custom$lexicon <- 'custom'
stopwords2 <-  rbind(get_stopwords(),custom)
df2 <- df %>% unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stopwords2)

g2 <- graph_from_data_frame(df2)

V(g2)$type <- FALSE                                      #terms
V(g2)$type[which(degree(g2, mode="in")==0)] <- TRUE       #indegree==0, these are texts

g_sem <- bipartite.projection(g2, which="false")
g_sem2 <- delete.edges(g_sem, which(E(g_sem)$weight<5))
g_sem2 <- delete.vertices(g_sem2, which(degree(g_sem2)==0))

write.graph(g_sem2, "frame_analysis.graphml", format="graphml")
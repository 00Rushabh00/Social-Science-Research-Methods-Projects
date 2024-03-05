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
library(ggplot2)

setwd("D:/University/SOCY2169/assignment 4")
g <- read.graph("ukrainerussia.graphml", format="graphml")

df_normal <- data.frame(
  status_id=E(g)$user_id,
  text=tail_of(g,E(g))$vosonTxt_tweet)
# 
# df_normal$text <- gsub("http.*","",  df_normal$text)              #remove URLs (https://...)
# df_normal$text <- gsub("https.*","",  df_normal$text)             #remove URLs (http://...)
# df_normal$text <- gsub("@\\w+", "", df_normal$text)               #remove usernames
df_normal$text <- gsub("#\\w+","",  df_normal$text)              #remove hashtags
# df_normal$text <- gsub("[[:digit:]]", "", df_normal$text)         #remove numbers


#stopwords
custom <- read.csv("removelist.csv",header = T)
custom$lexicon <- 'custom'
stopwords2 <-  rbind(get_stopwords(),custom)

twitter_corpus <- corpus(df_normal)

# convert corpus to tokens and apply sentiment dictionary to code tokens
sentiment_tokens  <- twitter_corpus %>%  tokens( remove_punct = TRUE, remove_numbers=TRUE, 
                                                 remove_separators =TRUE, remove_url = TRUE) %>% tokens_lookup(dictionary = data_dictionary_LSD2015)

# create document-feature matrix of sentiment
dfm_sentiment  <-  dfm(sentiment_tokens)

# convert to a data.frame to allow further calculations 
sentiment_df  <- convert(dfm_sentiment, to = 'data.frame')
sentiment_df1 <- sentiment_df[sentiment_df$negative>0 & sentiment_df$positive>0,]
sentiment_df1[1:3,]

#Find out what are the words
toks1 <- twitter_corpus %>% tokens()

# Three tweets
kwic(toks1[54], data_dictionary_LSD2015["positive"])
kwic(toks1[54], data_dictionary_LSD2015["negative"])

kwic(toks1[10], data_dictionary_LSD2015["positive"])
kwic(toks1[10], data_dictionary_LSD2015["negative"])

kwic(toks1[12], data_dictionary_LSD2015["positive"])
kwic(toks1[12], data_dictionary_LSD2015["negative"])

#Logit scale sentiment score
par(mfrow=c(1,1))
sentiment_df$sent_score <- log(sentiment_df$positive + sentiment_df$neg_negative + 0.5)  -  log(sentiment_df$negative + sentiment_df$neg_positive + 0.5)

plot(density(sentiment_df$sent_score), main = "Sentiment Density Distribution")

#Calculate another sentiment score: range.
sentiment_df$range  <- sentiment_df$positive  +
  sentiment_df$neg_negative - sentiment_df$negative - sentiment_df$neg_positive

sentiment_df$sent_overall <- ifelse(sentiment_df$range>0, "positive", 
                                    ifelse(sentiment_df$range==0, "neutral", "negative"))

#bring across all the other variables we have on the twitter data
sentiment_df <- cbind(sentiment_df, docvars(dfm_sentiment))



#add sent_overall to the original twitter dataframe, so we can use it as a document variable
df2 <- sentiment_df %>% select(status_id, sent_overall)
df_normal <- left_join(df_normal, df2, by=c("status_id"))

#corpus - only tweets scored as positive or negative (neutral removed)
twitter_corpus  <-  corpus(df_normal) %>% corpus_subset(sent_overall %in% c("positive", "negative"))

twitter_tokens  <- twitter_corpus %>% tokens( remove_punct = TRUE, remove_numbers=TRUE, remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords2) %>% tokens_ngrams(n=1)
twitter_dfm  <-  dfm(twitter_tokens)

#Comparison cloud
twitter_dfm <- dfm_group(twitter_dfm, twitter_dfm$sent_overall) %>%
  dfm_trim(min_termfreq = 3) 


 textplot_wordcloud(twitter_dfm, comparison = TRUE, max_words = 300,
                  , min_size = 1, max_size = 8)

textplot_wordcloud(twitter_dfm, rotation = 0.25, comparison = TRUE,
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")), min_size = 2, max_size = 8, max_words = 500)



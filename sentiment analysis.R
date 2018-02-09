#rm(list=ls())

setwd("C:/Users/User/Dropbox/IR Phase/05_Analysis_R")
load("C:/Users/User/Dropbox/IR Phase/04_Dataset/final_merged_data_1.RData")
alldata<-z

#install packages
install.packages("tidyverse")
install.packages("stringr")
install.packages("tidytext")
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(dplyr)

sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#set dataframe to analyse overview data
overviewtext<-as.character(alldata$overview1)
moviename<-(alldata$title1)

overview<-data_frame(title=moviename, overview_text=overviewtext)

str(overview)


#unnest_tokens 
tidy_overview <-overview %>%
  unnest_tokens(word, overview_text)

#remove stopwords from the whole data.
data(stop_words)
tidy_overview <- tidy_overview %>%
  anti_join(stop_words)


#count simple and plot data with counting info.
countword<- tidy_overview %>%
  count(word, sort = TRUE) 

library(ggplot2)

tidy_overview %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#inner_join using various types of sentiment Lexicons. 
library(tidyr)

overviewsentiment_bing <- tidy_overview %>%
  inner_join(get_sentiments("bing"))

overviewsentiment_afinn <- tidy_overview %>%
  inner_join(get_sentiments("afinn"))

overviewsentiment_nrc <- tidy_overview %>%
  inner_join(get_sentiments("nrc"))


#regarding lexicons, sentmentanalysis je nach titlenames
afinn <- tidy_overview %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(title) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
  
colnames(afinn)[colnames(afinn)=="title"]<-"index"

bing_and_nrc <- bind_rows(tidy_overview %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_overview %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                      filter(sentiment %in% c("positive","negative"))) %>%
                            mutate(method = "NRC")) %>%
                count(method, index = title, sentiment) %>%
                spread(sentiment, n, fill = 0) %>%
                mutate(sentiment = positive - negative)


bing <- tidy_overview %>% 
        inner_join(get_sentiments("bing")) %>%
        mutate(method = "Bing et al.")%>%
  count(method, index = title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
                          
nrc<- tidy_overview %>% 
      inner_join(get_sentiments("nrc") %>% 
      filter(sentiment %in% c("positive","negative"))) %>%
      mutate(method = "NRC") %>%
  count(method, index = title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(gtools)
all_sentiment_col<-merge(afinn, bing, by="index", all = T)
all_sentiment_col<-merge(all_sentiment_col, nrc, by="index", all = T)

all_sentiment<-bind_rows(afinn,bing_and_nrc) 

save(all_sentiment_col,file = "all_sentiment_col.RData")
write.csv2(all_sentiment_col,file = "all_sentiment_col.csv")


#aggregate it with the original data
colnames(all_sentiment_col)[colnames(all_sentiment_col)=="index"]<-"title1"

alldata<- merge(alldata, all_sentiment_col, by="title1", all = T)

save(alldata_w_sentiment,file = "alldata_w_sentiment.RData")
write.csv2(alldata_w_sentiment,file = "alldata_w_sentiment.csv")




#rm(list=ls())

setwd("C:/Users/User/Dropbox/IR Phase/05_Analysis_R")
#install.packages("devtools")
library(devtools)
devtools::install_github("bmschmidt/wordVectors")
library(wordVectors)
library(magrittr)

load("C:/Users/User/Dropbox/IR Phase/04_Dataset/final_merged_data_1.RData")
alldata<-z
overviewdata<-alldata$overview2
overview_in_one<-paste(overviewdata,collapse = "/")

write.table(overview_in_one,file="overview_in_one.txt", sep="\t")


prep_word2vec(origin="overview_in_one.txt",destination="overview_in_one_refine.txt",lowercase=T,bundle_ngrams=2)

if (!file.exists("overview_in_one.bin")) 
{model = train_word2vec("overview_in_one_refine.txt","overview_in_one.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0)} else model = read.vectors("overview_in_one_refine.bin")

model = read.vectors("overview_in_one.bin")


model %>% closest_to("adventure")
model %>% 
  closest_to(model[[c("adventure","epic","journey","extraordinary","dramatic","incredible","odyssey")]],50)

some_adventure = closest_to(model,model[[c("adventure","epic","journey","extraordinary","dramatic","incredible","odyssey")]],150)
adventure = model[[some_adventure$word,average=F]]
plot(adventure,method="pca")

set.seed(10)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 40)


sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

adventures_cluster = c("adventure","epic","journey","extraordinary")
term_set = lapply(adventures_cluster, 
                  function(adventures_cluster) {
                    nearest_words = model %>% closest_to(model[[adventures_cluster]],20)
                    nearest_words$word
                  }) %>% unlist

subset = model[[term_set,average=F]]

subset %>%
  cosineDist(subset) %>% 
  as.dist %>%
  hclust %>%
  plot

genre = model[[c("crime","thriller"),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
crime_and_thriller = model[1:3000,] %>% cosineSimilarity(genre)

# Filter to the top 20 crime or thriller.
crime_and_thriller = crime_and_thriller[
  rank(-crime_and_thriller[,1])<20 |
    rank(-crime_and_thriller[,2])<20,
  ]

plot(crime_and_thriller,type='n')
text(crime_and_thriller,labels=rownames(crime_and_thriller))

genre2 = model[[c("crime","thriller","horror","mystery","war"),average=F]]

# model[1:3000,] here restricts to the 3000 most common words in the set.
common_similarities_genre = model[1:3000,] %>% cosineSimilarity(genre2)

common_similarities_genre[20:30,]


high_similarities_to_genre = common_similarities_genre[rank(-apply(common_similarities_genre,1,max)) < 75,]

high_similarities_to_genre %>% 
  prcomp %>% 
  biplot(main="74 words in projection of genre")

install.packages("tsne")
library(tsne)
plot(model,perplexity=50)


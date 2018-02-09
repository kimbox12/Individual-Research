# Individual-Research

The purpose of this research is to develop the new movie recommendation system using genre information and text information to support the decision making of customers. 



-	R script for data collection.R

What I did was, first of all, I gathered raw Data from The Movie Database(TMDB) and The Numbers https://www.the-numbers.com/ Websites     using API and Web Scraping. 

-	R script for merging datasets.R
And I merged two raw data and refined the dataset.
About 5500 rows, 170 variables.

as a result,i gathered and refined data
final_merged_data_1.RData




-	Sentiment analysis.R
I did the sentiment analysis, to observe, whether the sentences in the overview(small description of the movie) are positive, neutral, and negative.

As a result, i got the sentiment analysis using various types of word lexicon
all_sentiment.RData




-	Vec2Word_real.R
Vec2Word which is the method for the text Mining. The sentences are divided into corpus unit and we can make really huge matrix consisted of words in sentences. And from this matrix, we can calculate the correlation between 2 words each. So if I put a word in this algorithm, then I can get the most relevant word and the most correlated words in the sentences. 

For further research, I will cluster the movie(k-mean or k-medoid) before I do the sentiment analysis because I want to compare the result between the clusters. Based on this process, I want to develop the recommendation system.

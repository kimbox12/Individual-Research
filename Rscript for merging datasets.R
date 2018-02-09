############################################################################################################################################################
#1. combined_first_iteration
colnames(combined_first_iteration)
str(combined_first_iteration)
install.packages("tidyr")
library(tidyr)
combined_first_iteration[!is.na(combined_first_iteration$genre_ids.10769L),]
#10769L= foreign this one is new genre 

#unite two columns, which shows a same meaning
a<-combined_first_iteration
a[is.na(a)] = ''
a<-unite(a, genre_Action, c("genre_ids.28L","X28L"), sep='')
a<-unite(a, genre_Adventure, c("genre_ids.12L","X12L"), sep='')
a<-unite(a, genre_Animation, c("genre_ids.16L","X16L"), sep='')
a<-unite(a, genre_Comedy, c("genre_ids.35L","X35L"), sep='')
a<-unite(a, genre_Crime, c("genre_ids.80L","X80L"), sep='')
a<-unite(a, genre_Documentry, c("genre_ids.99L","X99L"), sep='')
a<-unite(a, genre_Drama, c("genre_ids.18L","X18L"), sep='')
a<-unite(a, genre_Family, c("genre_ids.10751L","X10751L"), sep='')
a<-unite(a, genre_Fantasy, c("genre_ids.14L","X14L"), sep='')
a<-unite(a, genre_History, c("genre_ids.36L","X36L"), sep='')
a<-unite(a, genre_Horror, c("genre_ids.27L","X27L"), sep='')
a<-unite(a, genre_Music, c("genre_ids.10402L","X10402L"), sep='')
a<-unite(a, genre_Mystery, c("genre_ids.9648L","X9648L"), sep='')
a<-unite(a, genre_Romance, c("genre_ids.10749L","X10749L"), sep='')
a<-unite(a, genre_ScienceFiction, c("genre_ids.878L","X878L"), sep='')
a<-unite(a, genre_TVMovie, c("genre_ids.10770L","X10770L"), sep='')
a<-unite(a, genre_Thriller, c("genre_ids.53L","X53L"), sep='')
a<-unite(a, genre_War, c("genre_ids.10752L","X10752L"), sep='')
a<-unite(a, genre_Western, c("genre_ids.37L","X37L"), sep='')

str(a)
colnames(a)[colnames(a)=="genre_ids.10769L"] <- "genre_Foreign"
colnames(a)[colnames(a)=="genre_ids.10770L"] <- "genre_TVMovie"
colnames(a)[colnames(a)=="genre_ids.9648L"] <- "genre_Mystery"


#change colnames
colnames(a)<-c("vote_count1","id1","video1","vote_average1","title1","popularity1","poster_path1","original_language1","original_title1","genre_Action",
               "genre_Adventure","genre_Fantasy","genre_ScienceFiction","backdrop_path1","adult1","overview1","release_date1","genre_Crime","genre_Drama",
               "genre_Thriller","genre_Western","genre_Animation","genre_Family","genre_Comedy","genre_Romance","genre_Horror","genre_Mystery","genre_War",
               "genre_History","genre_Music","NA","genre_Documentry","genre_Foreign","genre_TVMovie","rank1")

#remove a column, its name is "NA"
a<-a[,-31]

#save 
save(a,file="a.RData")
write.csv2(a,file="a.csv")



############################################################################################################################################################
#2. combined_second_iteration
colnames(combined_second_iteration)
str(combined_second_iteration)

#change colnames 
colnames(combined_second_iteration)
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="adult"] <- "adult2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="backdrop_path"] <- "backdrop_path2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="budget"] <- "budget2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="homepage"] <- "homepage2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="id"] <- "id2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="imdb_id"] <- "imdb_id2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="original_language"] <- "original_language2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="original_title"] <- "original_title2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="overview"] <- "overview2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="popularity"] <- "popularity2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="poster_path"] <- "poster_path2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="revenue"] <- "revenue2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="runtime"] <- "runtime2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="title"] <- "title2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="video"] <- "video2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="status"] <- "status2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="tagline"] <- "tagline2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="vote_average"] <- "vote_average2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="vote_count"] <- "vote_count2"
colnames(combined_second_iteration)[colnames(combined_second_iteration)=="release_date"] <- "release_date2"

b<-combined_second_iteration
str(b)

save(b,file="b.RData")
write.csv2(b,file="b.csv")



############################################################################################################################################################
#3. combined_df
str(combined_df)
numbers<-combined_df
colnames(numbers)<-c("rank3","release_date3","movie3","production_bueget3","domestic_gorss3","worldwide_gross3")



############################################################################################################################################################
############################################################################################################################################################
#bind 3 dataframe (join(a,b,numbers))

#in these two data frames, there is a id. we can join these two data frames using cbind and later check,whether there is discordance.
x<-cbind(a,b)
y<-data.frame(x$title1,x$original_title1,x$original_title2,x$title2,x$id1,x$id2)
                
diff_y<-y[which(y$id1!=y$id2),]
nrow(diff_y)             

#inner Join by "rank"
#first of all, we have to change colnames rank1,rank3 to rank
colnames(x)[colnames(x)=="rank1"] <- "rank"
colnames(numbers)[colnames(numbers)=="rank3"] <- "rank"

str(x)
str(numbers)

#innerjoin by rank
z<-merge(x, numbers, by = "rank")

save(z, file="final_merged_data_1.RData")
write.csv2(z,file="final_merged_data_1.csv")






##############################################################################################################################################################
#Example of Jointype

#df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)))
#df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("Alabama", 2), rep("Ohio", 1)))


#Inner Join:
#merge(df1, df2, by = "CustomerId")

#Outer Join:
#merge(x = df1, y = df2, by = "CustomerId", all = TRUE)

#Left outer: 
#merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)

#Right outer: 
#merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)

#Cross join: 
#merge(x = df1, y = df2, by = NULL)

##############################################################################################################################################################

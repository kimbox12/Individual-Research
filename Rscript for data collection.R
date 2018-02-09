setwd("C:/Users/User/Dropbox/IR Phase/04_Dataset")

#rm(list=ls())

#installing Packages
install.packages("rvest")
library(rvest)
install.packages("stringr")
library(stringr)
install.packages("httr")
library(httr)
install.packages("gtools")
library(gtools)



###########################################################################################################################################################
#using URL and scrapping the website of www.the-numbers.com to collect the information about the bugets and gross.
###########################################################################################################################################################
#if you look at "http://www.the-numbers.com/movie/budgets/all" this page, we can see there are only 100 films in each webpages.
#so first, we have to set the url adresses, where i want to scrap. 
#make each url adresses
number<-seq(101,5401,100)
url<-"http://www.the-numbers.com/movie/budgets/all"
Numbers<-data.frame(number=seq(1,5401,100))
Numbers$number<-sapply(Numbers$number, function(x) paste(url,x,sep="/"))

#make a empty dataframe with valid variables
combined_df <- data.frame("number"=integer(),
                          "Release Date"=as.Date(character()),
                          "Movie"=character(),
                          "Production Budget"=character(),
                          "Domestic Gross"=character(),
                          "Worldwide Gross"=character()
                          )

#make for loop to scrap the informations in the table 
for (i in 1:nrow(Numbers)){
                            The_Numbers <- read_html(Numbers[i,])
                            The_Numbers_node <- html_node(The_Numbers,"table")
                            The_Numbers_all <- html_table(The_Numbers_node,fill = TRUE)
                            #we have to delete some rows which are NULL.
                            a<-seq(2,200,2) 
                            The_Numbers_all<- The_Numbers_all[-a,]
                            #combine new outcome into a empty dataframe.
                            combined_df <- rbind(combined_df, The_Numbers_all)
}

#generate a column which show the rank
colnames(combined_df)[1]<-"Rank"
combined_df$Rank<-c(1:5401)

#save whole dataset to RData 
save(combined_df,file = "numbers.RData")
write.csv2(combined_df,file= "numbers.csv")



############################################################################################################################################################
#select filmnames and change unmatched names or incorrect names and delete useless punctuations.
############################################################################################################################################################
#select filmnames
Movie <- combined_df[,3]
Moviee= gsub(":", "", Movie, fixed = TRUE)
Moviee= gsub(",", "", Moviee, fixed = TRUE)
Moviee= gsub("-", " ", Moviee, fixed = TRUE)
Moviee[] <- lapply(Moviee, str_replace_all, "'"," ")
Moviee= gsub("[[:punct:]]", " ",Moviee) 
Moviee= gsub(" ", "+", Moviee, fixed = TRUE)
Moviee[c(3,23,250,278,296,453,726,1008,1046,1425,1696,
         1751,1969,1981,2036,2073,2330,2731,3244,3419,
         3993,4015,4477,4641,4713,4731,4830,147,173,3952,2502,2509,5331)]<- c("Pirates+of+the+Caribbean+At+World+s+End",                    
                                                                              "Pirates+of+the+Caribbean+Dead+Man+s+Chest",
                                                                              "Assassin+s+Creed","The+Huntsman+Winter+s+War",
                                                                              "Miss+Peregrine+s+Home+for+Peculiar+Children",
                                                                              "It+s+Complicated","Pete+s+Dragon","Daddy+s+Home",                                     
                                                                              "Don+t+Say+a+Word","Billy+Lynn+s+Long+Halftime+Walk",                        
                                                                              "Mr++Holland+s+Opus","The+Hitman+s+Bodyguard","Rules+Don+t+Apply",     
                                                                              "New+Year+s+Eve","A+Dog+s+Purpose","Mr++Bean+s+Holiday"  ,
                                                                              "Tyler+Perry+s+Boo++A+Madea+Halloween","When+Harry+Met+Sally" ,                    
                                                                              "St++Trinian+s","Io+sono+lamore","God+s+Not+Dead+2",      
                                                                              "Hillary+s+America+The+Secret+History+of+the+Democratic+Party",
                                                                              "2016+Obama+s+America" ,"The+Legend+of+Hell+s+Gate+An+American+Conspiracy",
                                                                              "The+King+s+Thief","I+m+Not+Ashamed","God+s+Not+Dead",   
                                                                              "Mission+Impossible+Rogue+Nation","Mission+Impossible+Ghost+Protocol",
                                                                              "La+Vie+d+Ad+le+Chapitres+1+et+2","Jiyi+dashi", 
                                                                              "????????????","I+Love+You+Don+t+Touch+Me+")                           

#save names of Movies
Moviee<-as.data.frame(Moviee)
save(Moviee,file="Nameofmovies.RData")
write.csv2(Moviee,file="Nameofmovies.csv")

#save url+names to use it API searching
Moviee<-as.data.frame(Moviee)
Url="https://api.themoviedb.org/3/search/movie?api_key=f495edb42696a5be2ca46eb76355482d&query="
Moviee$Moviee<-sapply(Moviee$Moviee, function(x) paste(Url,x,sep=""))
Movienames<-Moviee
save(Movienames,file = "Movienames.RData")



#############################################################################################################################################################
#gathering genre information
#############################################################################################################################################################
#extract 19 genres
Url="https://api.themoviedb.org/3/genre/movie/list?language=en-US&api_key=f495edb42696a5be2ca46eb76355482d"
out = GET(url=Url)
http_status(out)
data= content(out)

#save as dataframe
Genre<-as.data.frame(data)

#change columns to rows and naming the columns. 
a<-t(Genre[,seq(1,38,2)])
b<-t(Genre[,seq(2,38,2)])
Genre<-data.frame("genres.id"=a,"genres.name"=b,row.names=NULL)
save(Genre,file="genrelist.RData")
write.csv2(Genre,file="genrelist.csv")


############################################################################################################################################################
#gathering Data using TMDB API referencing movienames, which is collected from "the Numbers" website
############################################################################################################################################################
#1st iteration

#change a dataframe to characters
Moviee<-Movienames[1:5401,]

#make an empty dataframe, to save the information from gathered API results. 
combined_dm<-data.frame("vote_count"=integer(),
                        "id"=integer(),
                        "video"=logical(),
                        "vote_average"=numeric(),
                        "title"=factor(),
                        "popularity"=numeric(),
                        "poster_path"=factor(),
                        "original_language"=factor(),
                        "original_title"=factor(),
                        #"genre_ids"=factor(),
                        "X35L"=integer(),
                        "backdrop_path"=factor(),
                        "adult"=logical(),
                        "overview"=factor(),
                        "release_date"=as.Date(character())
                        )

#make a first row of data
Url=Moviee[1]
out = GET(url=Url)
http_status(out)
data= content(out)
dm<-as.data.frame(data$results[[1]])
combined_dm <- rbind(combined_dm, dm)

####################################################################################################
# after fixing this script, this iteration is not anymore useful:)
# Using another function i can execute for loop freely depite of 40 request limit.
##we can only request 40 times per 10 seconds, so we had to divide the movienames. 
##Divide<-data.frame(seq(2,5401,40),seq(41,5401,40))
##Divide2<-paste(Divide$seq(2,5401,40),Divide$seq(41,5401,40),sep = ":")
##Divide3<-paste("Moviee1<-Moviee[",Divide2,sep = "")
##Divide4<-as.character(paste(Divide3,"]",sep = ""))
##Divide4 
######################################################################################################

#reset the instances, because we have already the first row. 
Moviee<-Movienames[2:5401,]

#via TMDB API service, we can gather informations using names of Moviee  
for (i in 1:length(Moviee)){
  Url=Moviee[i]
  #Url="https://api.themoviedb.org/3/search/movie?api_key=f495edb42696a5be2ca46eb76355482d&query=jiyi+dashi"
  out = GET(url=Url)
  http_status(out)
  data= content(out)
  
  if (!is.null(data$total_results) && data$total_results!=0){
                                                              if(is.null(data$results[[1]]$backdrop_path)){data$results[[1]]$backdrop_path<-NULL}
                                                              if(is.null(data$results[[1]]$poster_path)){data$results[[1]]$poster_path<-NULL}
                                                              if(is.null(data$results[[1]]$original_language)){data$results[[1]]$original_language<-NULL}
                                                              if(length(data$results[[1]]$genre_ids)==0){data$results[[1]]$genre_ids<-NULL}
                                                              dm<-as.data.frame(data$results[[1]])
                                                            } 
  else{dm<-rep(NA, ncol(combined_dm))
  }
  
  combined_dm <- smartbind(combined_dm, dm,fill=NA)
  
  #because of request limit, that we can only 40 request per 10 seconds, 
  #after 1.iteration of the forloop, wait 0.5 seconds before executing 2. iteration. 
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<0.5){}
}

#generate a column which show the rank
combined_dm$Rank<-c(1:5401)

#save all 5401 movies
save(combined_dm,file="allData1.RData")


############################################################################################################################################################
#in allData1.RData, there are many rows, which is not correct or do not match with previous Numebers data. 
############################################################################################################################################################

#so we have to compare each movie names in allData1.RData(combined_dm) and Numbers.RData(combined_df)
firstcoll<-combined_dm
movieidname<-data.frame(firstcoll$id,firstcoll$title,firstcoll$original_title,firstcoll$Rank,firstcoll$release_date,stringsAsFactors = FALSE)
NameMovie<-data.frame(combined_df$Movie, combined_df$`Release Date`,stringsAsFactors = FALSE)
NameMoviee<-cbind(movieidname,NameMovie)
new_firstcoll<-firstcoll[is.na(firstcoll$id),]
new_NameMoviee<-NameMoviee[is.na(NameMoviee$firstcoll.id),]
write.csv(NameMoviee, file = "NameMoviee.csv")


#select data which is indifferent
indiff_NameMoviee<-NameMoviee[which(NameMoviee$firstcoll.title==NameMoviee$combined_df.Movie),]
same_Movie<-firstcoll[which(NameMoviee$firstcoll.title==NameMoviee$combined_df.Movie),]

#select data which is different
diff_NameMoviee<-NameMoviee[which(NameMoviee$firstcoll.title!=NameMoviee$combined_df.Movie),]
same_Movie<-firstcoll[which(NameMoviee$firstcoll.title==NameMoviee$combined_df.Movie),]
 
#compare movie title and released date and select out which is not matched(from diff_NameMoviee) 

#all unmatched information
unmatch<-c(26,43,63,75,193,226,235,276,315,329,335,434,445,600,611,646,928,969,1008,1055,1089,1121,1209,1242,1243,1266,1305,
           1446,1453,1477,1740,1954,1970,2003,2068,2018,2197,2227,2321,2330,2355,2376,2464,2504,2558,2572,2632,2682,2736,2788,
           2792,2841,2870,2876,2914,2917,2924,2935,3169,3206,3223,3226,3230,3231,3237,3266,3278,3293,3307,3371,3420,3437,3500,
           3532,3547,3580,3634,3714,3787,3788,3844,3905,3913,3918,3920,3939,3947,3949,3954,4040,4101,4122,4131,4143,4145,4162,
           4180,4267,4297,4302,4303,4315,4319,4323,4337,4345,4346,4383,4412,4413,4463,4476,4512,4622,4645,4653,4699,4758,4773,
           4776,4787,4794,4835,4859,4887,4894,4908,4980,5014,5022,5070,5075,5086,5087,5114,5123,5138,5148,5184,5188,5206,5210,
           5213,5215,5221,5223,5231,5245,5257,5267,5276,5301,5310,5344,5349,5366,5371,5385,5388,5389,5396,5399)

#select which is NA values
notavailable<-c(which(is.na(firstcoll$id)))
combined_first_iteration<-combined_dm[-c(unmatch,notavailable),]
#total 5401-162unmatched-69NA=5170 movies

#so we got the information which is match with numbers.
save(combined_first_iteration,file="combined_first_iteration.RData")
write.csv2(combined_first_iteration,file="combined_first_iteration.csv")




############################################################################################################################################################
#gathering Data using TMDB API referencing TMDB ID, which is collected from "TMDB" website
############################################################################################################################################################
#2st iteration

#firsdt of all, make url adresses to request the data from TMDB API
combined_dmd_id<-as.data.frame(combined_first_iteration$id)
#For example, Url="https://api.themoviedb.org/3/movie/38757?api_key=f495edb42696a5be2ca46eb76355482d"
combined_dmd_id$`combined_first_iteration$id`<-sapply(combined_dmd_id$`combined_first_iteration$id`, function(x) paste("https://api.themoviedb.org/3/movie/",x,sep=""))
combined_dmd_id$`combined_first_iteration$id`<-sapply(combined_dmd_id$`combined_first_iteration$id`, function(x) paste(x,"?api_key=f495edb42696a5be2ca46eb76355482d",sep=""))
save(combined_dmd_id,file = "combined_dmd_id.RData")

#create a empty data frame to save the information. 
combined_dm2<-data.frame("adult"=logical(),
                         "backdrop_path"=character(),
                         "belongs_to_collection"=as.character(),
                         "budget"=integer(),
                         "genres"=as.character(),
                         "hompage"=character(),
                         "id"=integer(),
                         "imdb_id"=character(),
                         "original_language"=character(),
                         "original_title"=character(),
                         "overview"=character(),
                         "popularity"=numeric(),
                         "poster_path"=character(),
                         "production_companies"=as.character(),
                         "production_countries"=as.character(),
                         "release_date"=character(),
                         "revenue"=integer(),
                         "runtime"=integer(),
                         "spoken_languages"=as.character(),
                         "status"=character(),
                         "tagline"=character(),
                         "title"=character(),
                         "video"=logical(),
                         "vote_average"=numeric(),
                         "vote_count"=integer()
                          )

#first, before executing forloop, we have to add the first row to the empty dataframe. 
combined_url<-combined_dmd_id[1,]
Url=combined_url[1]
out = GET(url=Url)
http_status(out)
data= content(out)
dm2<-as.data.frame(data)
combined_dm2 <- rbind(combined_dm2,dm2)

#to execute the following forloop, reset the url instances.Because we have already the first row.
combined_url<-combined_dmd_id[2:5170,]

#gathering Data using TMDB API referencing TMDB ID.
for (i in 1:length(combined_url)){
                                  Url=combined_url[i]
                                  #Url="https://api.themoviedb.org/3/movie/38757?api_key=f495edb42696a5be2ca46eb76355482d"
                                  out = GET(url=Url)
                                  http_status(out)
                                  data= content(out)
  
                                  if(is.null(data$belongs_to_collection)){data[c("belongs_to_collection")]<-NULL}
                                  if(is.null(data$backdrop_path)){data$backdrop_path<-NULL}
                                  if(is.null(data$belongs_to_collection$backdrop_path)){data$belongs_to_collection$backdrop_path<-NULL}
                                  if(is.null(data$poster_path)){data$poster_path<-NULL}
                                  if(is.null(data$belongs_to_collection$poster_path)){data$belongs_to_collection$poster_path<-NULL}
                                  if(is.null(data$imdb_id)){data$imdb_id<-NULL}
                                  if(length(data$production_countries)==0){data$production_countries<-NULL}
                                  if(length(data$production_companies)==0){data$production_companies<-NULL}
                                  if(length(data$spoken_languages)==0){data$spoken_languages<-NULL}
                                  if(length(data$runtime)==0){data$runtime<-NULL}
                                  if(length(data$genres)==0){data$genres<-NULL}
                                  dm2<-as.data.frame(data)
  
                                  combined_dm2 <- smartbind(combined_dm2,dm2,fill=NA)
  
                                  #because of request limit, that we can only 40 request per 10 seconds, 
                                  #after 1.iteration of the forloop, wait 0.5 seconds before executing 2. iteration. 
                                  date_time<-Sys.time()
                                  while((as.numeric(Sys.time()) - as.numeric(date_time))<0.5){}
                                  }

#save data 
combined_second_iteration<-combined_dm2
save(combined_second_iteration,file="combined_second_iteration.RData") 
write.csv2(combined_second_iteration,file="combined_second_iteration.csv")




############################################################################################################################################################
#gathering Company information
############################################################################################################################################################
P_Companies_0<-unique(combined_second_iteration$production_companies.id)
P_Companies_1<-unique(combined_second_iteration$production_companies.id.1)
P_Companies_2<-unique(combined_second_iteration$production_companies.id.2)
P_Companies_3<-unique(combined_second_iteration$production_companies.id.3)
P_Companies_4<-unique(combined_second_iteration$production_companies.id.4)
P_Companies_5<-unique(combined_second_iteration$production_companies.id.5)
P_Companies_6<-unique(combined_second_iteration$production_companies.id.6)
P_Companies_7<-unique(combined_second_iteration$production_companies.id.7)
P_Companies_8<-unique(combined_second_iteration$production_companies.id.8)
P_Companies_9<-unique(combined_second_iteration$production_companies.id.9)
P_Companies_10<-unique(combined_second_iteration$production_companies.id.10)
P_Companies_11<-unique(combined_second_iteration$production_companies.id.11)
P_Companies_12<-unique(combined_second_iteration$production_companies.id.12)
P_Companies_13<-unique(combined_second_iteration$production_companies.id.13)
P_Companies_14<-unique(combined_second_iteration$production_companies.id.14)
P_Companies_15<-unique(combined_second_iteration$production_companies.id.15)
P_Companies_16<-unique(combined_second_iteration$production_companies.id.16)
P_Companies_17<-unique(combined_second_iteration$production_companies.id.17)
P_Companies_18<-unique(combined_second_iteration$production_companies.id.18)
P_Companies_19<-unique(combined_second_iteration$production_companies.id.19)
P_Companies_20<-unique(combined_second_iteration$production_companies.id.20)
P_Companies_21<-unique(combined_second_iteration$production_companies.id.21)
P_Companies_22<-unique(combined_second_iteration$production_companies.id.22)
P_Companies_23<-unique(combined_second_iteration$production_companies.id.23)
P_Companies_24<-unique(combined_second_iteration$production_companies.id.24)
P_Companies_25<-unique(combined_second_iteration$production_companies.id.25)

P_Companies<-cbind(P_Companies_0,P_Companies_1,P_Companies_2,P_Companies_3,
                   P_Companies_4,P_Companies_5,P_Companies_6,P_Companies_7,
                   P_Companies_8,P_Companies_9,P_Companies_10,P_Companies_11,
                   P_Companies_12,P_Companies_13,P_Companies_14,P_Companies_15,
                   P_Companies_16,P_Companies_17,P_Companies_18,P_Companies_19,
                   P_Companies_20,P_Companies_21,P_Companies_22,P_Companies_23,
                   P_Companies_24,P_Companies_25)
P_Companies<-as.data.frame(P_Companies)
1707*26
#stack all columns into one column
install.packages("reshape2")
library(reshape2)
P_Companies<- melt(P_Companies)
#or
P_Companies<-stack(P_Companies[1:26])

#select unique id
P_Companies<-unique(P_Companies$values)
# total 5424 production companies
P_Companies<-as.data.frame(P_Companies)
P_Companies<-order(P_Companies,decreasing = FALSE)
colnames(P_Companies)<-"P_Companies_id"

save(P_Companies, file="Prod_Companies_id_in_finaldata.RData")


#firsdt of all, make url adresses to request the data from TMDB API
P_Companies_number<-data.frame(seq(1,97595,1))
colnames(P_Companies_number)<-"P_Companies_id"
#For example, Url="https://api.themoviedb.org/3/company/97595?api_key=f495edb42696a5be2ca46eb76355482d"
P_Companies_number$P_Companies_id<-sapply(P_Companies_number$P_Companies_id, function(x) paste("https://api.themoviedb.org/3/company/",x,sep=""))
P_Companies_number$P_Companies_id<-sapply(P_Companies_number$P_Companies_id, function(x) paste(x,"?api_key=f495edb42696a5be2ca46eb76355482d",sep=""))


Prod_Companies_all<-data.frame("description"=character(),
                        "headquarters"=character(),
                        "hompage"=character(),
                        "id"=integer(),
                        "logo_path"=character(),
                        "name"=character(),
                        "parent_company"=character()
                        )


P_Companies_url<-P_Companies_number[1:97595,]
Url=P_Companies_url[1]
out = GET(url=Url)
http_status(out)
data= content(out)
if(is.null(data$description)){data$description<-NULL}
if(is.null(data$headquarters)){data$headquarters<-NULL}
if(is.null(data$homepage)){data$homepage<-NULL}
if(is.null(data$id)){data$id<-NULL}
if(is.null(data$logo_path)){data$logo_path<-NULL}
if(is.null(data$name)){data$name<-NULL}
if(is.null(data$parent_company)){data$parent_company<-NULL}
dm3<-as.data.frame(data)
Prod_Companies_all<- rbind(Prod_Companies_all,dm3)

P_Companies_url<-P_Companies_number[2033:97595,]

P_Companies_url<-P_Companies_number[2033:5000,]


for (i in 1:length(P_Companies_url)){
  Url=P_Companies_url[i]
  #Url="https://api.themoviedb.org/3/movie/38757?api_key=f495edb42696a5be2ca46eb76355482d"
  out = GET(url=Url)
  http_status(out)
  data= content(out)
  
  if(is.null(data$description)){data$description<-NULL}
  if(is.null(data$headquarters)){data$headquarters<-NULL}
  if(is.null(data$homepage)){data$homepage<-NULL}
  if(is.null(data$id)){data$id<-NULL}
  if(is.null(data$logo_path)){data$logo_path<-NULL}
  if(is.null(data$name)){data$name<-NULL}
  if(is.null(data$parent_company)){data$parent_company<-NULL}
  if(is.null(data$parent_company$name)){data$parent_company$name<-NULL}
  if(is.null(data$parent_company$id)){data$parent_company$id<-NULL}
  if(is.null(data$parent_company$logo_path)){data$parent_company$logo_path<-NULL}
  
  
  dm3<-as.data.frame(data)
  Prod_Companies_all<- smartbind(Prod_Companies_all,dm3,fill=NA)
  
  
  #because of request limit, that we can only 40 request per 10 seconds, 
  #after 1.iteration of the forloop, wait 0.5 seconds before executing 2. iteration. 
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<0.3){}
}




Url="https://api.themoviedb.org/3/company/97595?api_key=f495edb42696a5be2ca46eb76355482d"
out = GET(url=Url)
http_status(out)
data= content(out)
data

---------------------------------------------------------------------------------------------------------------------------------
Url= "https://api.themoviedb.org/3/movie/38757?api_key=f495edb42696a5be2ca46eb76355482d"

Url="https://api.themoviedb.org/3/movie/343611/reviews?page=1&language=en-US&api_key=f495edb42696a5be2ca46eb76355482d"

Url="https://api.themoviedb.org/3/find/tt0398286?api_key=f495edb42696a5be2ca46eb76355482d&external_source=imdb_id"

theURL <- "http://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/movies.csv";
movie_data <- read.table(file = theURL, header = TRUE, sep = ",");
head(movie_data);
#State column is named as 'X". Changing the column name to US_State
colnames(movie_data)[1] <- "RefNo";
head(movie_data);
#Write the data to a CSV
write.table(movie_data, file = "movie_data.csv", sep = ",", row.names = FALSE);
#The saved file is uploaded to githib and below is the URL which will be used hereafter.
#https://raw.githubusercontent.com/arunk13/MSDA-Assignments/master/BridgeCourse/Week5/movie_data.csv 


install.packages("ggplot2movies")
library(ggplot2movies)
movie_data<-movies

install.packages("devtools")
library(devtools)
devtools::install_github("rmhogervorst/imdb")
install.packages("imdb")


season2GOT <-imdbSeries("game of thrones", seasons = 2)
season2GOT_enriched <- enrichIMDB(season2GOT)
library(imdb)



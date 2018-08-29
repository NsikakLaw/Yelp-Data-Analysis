install.packages("RMySQL")
install.packages("factoextra")
library(ggplot2)
library(RMySQL)
library(factoextra)
install.packages("stringi")
library(stringi)
#Database connection, Replace *** with MYSQL password
database_connection = dbConnect(MySQL(), user = "root", password = "****", dbname = "yelp_db", host = "localhost") 

#Code to confirm businesses improving or declining
businessprogress = dbSendQuery(database_connection, "select old1.o,neww1.n,n-o as progress,old1.business_id,neww1.business_id from old1 inner join neww1 where old1.business_id=neww1.business_id;")
dataprogress = fetch(businessprogress, n = -1)
head(dataprogress)
BusinessImproving <- subset(dataprogress, progress > 0)
BusinessDeclining <- subset(dataprogress, progress < 0)
BusinessStatic <- subset(dataprogress, progress == 0)

ProgressBar <- c(nrow(BusinessDeclining), nrow(BusinessImproving), nrow(BusinessStatic))
BusinessStatus <- c("Businesses on Decline", "Businesses on Increase","Businesses Static")
plotting3 <- data.frame(ProgressBar, BusinessStatus)
p2 <- ggplot(plotting3, aes(x=BusinessStatus, y=ProgressBar, fill = BusinessStatus)) + geom_bar(stat ="identity") + theme_minimal()
p2 + labs(title = "Bar Chart showing the Businesses Status", x = "Status of Businesses", y = "Number of Businesses")
p2


#CODE to show relationship between review length and stars
rs2 = dbSendQuery(database_connection, "select text, year(date) as year, stars from review")
#rs3=dbSendQuery(mydb, "select *from checkin_review");
#data = fetch(rs, n = -1)
Ratings_ReviewCount = fetch(rs2, n = -1)
head(Ratings_ReviewCount) #Confirm the data gotten from the database

Ratings_2004_2015 <- subset(Ratings_ReviewCount, year< 2015) #Select a subset of the data for dates before 2015
Ratings_2004_2015$CountofCharacters <- stri_length(Ratings_2004_2015$text)
StarRatings1 <- subset(Ratings_2004_2015, stars == 1)
StarRatings2 <- subset(Ratings_2004_2015, stars == 2)
StarRatings3 <- subset(Ratings_2004_2015, stars == 3)
StarRatings4 <- subset(Ratings_2004_2015, stars == 4)
StarRatings5 <- subset(Ratings_2004_2015, stars == 5)

StarRatings1$CountofCharacters[is.na(StarRatings1$CountofCharacters)] <- 0
StarRatings2$CountofCharacters[is.na(StarRatings2$CountofCharacters)] <- 0
StarRatings3$CountofCharacters[is.na(StarRatings3$CountofCharacters)] <- 0
StarRatings4$CountofCharacters[is.na(StarRatings4$CountofCharacters)] <- 0
StarRatings5$CountofCharacters[is.na(StarRatings5$CountofCharacters)] <- 0

MeanofStar5 <- mean(StarRatings5$CountofCharacters)
MeanofStar4 <- mean(StarRatings4$CountofCharacters)
MeanofStar3 <- mean(StarRatings3$CountofCharacters)
MeanofStar2 <- mean(StarRatings2$CountofCharacters)
MeanofStar1 <- mean(StarRatings1$CountofCharacters)

meanstars <- c(MeanofStar1, MeanofStar2, MeanofStar3, MeanofStar4, MeanofStar5)
noOfRatings <- c("1* Ratings","2** Ratings","3*** Ratings","4**** Ratings","5***** Ratings")
plotting <- data.frame(meanstars, noOfRatings)


p <-ggplot(plotting, aes(x=noOfRatings, y=meanstars, fill =noOfRatings)) + geom_bar(stat="identity") 
p +  scale_fill_manual(values=c("#FF0000","#B22222","#999999","#32CD32", "#00FF00"))+
  theme_minimal()
p + labs(title = "Plot of Average Character Count against Rating Between 2015 and 2017", x = "Number of Stars (Ratings)", y = "Mean Character Count")

Ratings_2015_2017 <- subset(Ratings_ReviewCount, year >= 2015) #Subset of data after 2015
Ratings_2015_2017$CountofCharacters <- stri_length(Ratings_2015_2017$text)
dataStarRatings5 <- subset(Ratings_2015_2017, stars == 5)
dataStarRatings4 <- subset(Ratings_2015_2017, stars == 4)
dataStarRatings3 <- subset(Ratings_2015_2017, stars == 3)
dataStarRatings2 <- subset(Ratings_2015_2017, stars == 2)
dataStarRatings1 <- subset(Ratings_2015_2017, stars == 1)

dataStarRatings5$CountofCharacters[is.na(dataStarRatings5$CountofCharacters)] <- 0
dataStarRatings4$CountofCharacters[is.na(dataStarRatings4$CountofCharacters)] <- 0
dataStarRatings3$CountofCharacters[is.na(dataStarRatings3$CountofCharacters)] <- 0
dataStarRatings2$CountofCharacters[is.na(dataStarRatings2$CountofCharacters)] <- 0
dataStarRatings1$CountofCharacters[is.na(dataStarRatings1$CountofCharacters)] <- 0


MeanNewStar1 <- mean(dataStarRatings1$CountofCharacters)
MeanNewStar2 <- mean(dataStarRatings2$CountofCharacters)
MeanNewStar3 <- mean(dataStarRatings3$CountofCharacters)
MeanNewStar4 <- mean(dataStarRatings4$CountofCharacters)
MeanNewStar5 <- mean(dataStarRatings5$CountofCharacters)

Mean_Ratings <- c(MeanNewStar1, MeanNewStar2, MeanNewStar3, MeanNewStar4, MeanNewStar5)

plotting2 <- data.frame(Mean_Ratings,noOfRatings)
p2 <- ggplot(plotting2, aes(x=noOfRatings, y=Mean_Ratings, fill = noOfRatings)) + geom_bar(stat ="identity") + theme_minimal()
p2
p2 + scale_fill_manual(values = c("#FF0000","#B22222","#999999","#32CD32", "#00FF00"))
p2 + labs(title = "Plot of Average Character Count against Number of Stars Between 2015 and 2017", x = "Number of Stars (Ratings)", y = "Mean Character Count")


#CODE to find the relationship between the length and Useful count
rs5 = dbSendQuery(database_connection, "select text, useful from review limit 20000")
data7 <- fetch(rs5, n=-1)
testdata <- data7[1:10000,]
testdata2 <- data7[10001:20000,]

testdata$CountofCharacters <- stri_length(testdata$text) #count the number of characters in the set
head(testdat[,2:3])
starcount <- testdata[,2:3]
starcluster <- kmeans(starcount, 6, nstart = 20)
starclusterdf <- data.frame(starcluster$centers)
p4 <- ggplot(starclusterdf, aes(x=CountofCharacters, y=useful)) + geom_line(color = "steelblue", size = 2)
p4 + labs(title = "Plot of Useful Count against Review Count", x = "Word Count", y = "Useful Count" )

testdata2$CountofCharacters <- stri_length(testdata2$text)
#testdat <- subset(testdata, CountofCharacters  < 300)
starcount2 <- testdata2[,2:3]
starcluster2 <- kmeans(starcount2, 6, nstart = 20)
starcluster2df <- data.frame(starcluster2$centers)
p5 <- ggplot(starcluster2df, aes(x=CountofCharacters, y=useful)) + geom_line(color = "steelblue", size = 2)
p5 + labs(title = "Plot of Useful Count against Review Count", x = "Word Count", y = "Useful Count")


#CODE for relationship between popularity and useful count
rs3 = dbSendQuery(database_connection, "select name, useful, fans from user;")
data3 <- fetch(rs3, n = -1)
clustertesting <- data3[1:20000,]
cluster3 <- kmeans(clustertesting[,2:3], 4, nstart = 20)
cluster3
cluster3$size
cluster3df <- data.frame(cluster3$centers)
p6 <- ggplot(cluster3df, aes(x=fans, y=useful)) + geom_line(color = "steelblue", size = 2)
p6 + labs(title = "Plot of Useful Count against Number of Fans", x = "Number of fans", y = "Useful Count" )

clustertesting2 <- data3[20001:40000,]
cluster4 <- kmeans(clustering2[,2:3], 4, nstart = 20)
cluster4
cluster4$size
cluster4df <- data.frame(cluster4$centers)
p6 <- ggplot(cluster4df, aes(x=fans, y=useful)) + geom_line(color = "steelblue", size = 2)
p6 + labs(title = "Plot of Useful Count against Number of Fans", x = "Number of fans", y = "Useful Count" )
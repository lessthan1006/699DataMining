#AD 699 Assignment 1
#Student: Yuqi Zheng
#Student ID: U79555973

install.packages("ggmosaic")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(leaflet)
library(scales)
library(GGally)

#1,2 Read file and find out the dimension
Boston<-read.csv("boston311.csv")
dim(Boston)

#3 Filter precint and read the dimention again
Boston1<-filter(Boston,precinct=="1207")
dim(Boston1)

#4a Change closed_dt data type to Date
str(Boston1)
Boston1$closed_dt <- as.Date(Boston1$closed_dt)
str(Boston1)

#4b create a new column called month with data in closed_dt, just keep month
#and change the data type to numberic
Boston1 <-mutate(Boston1, month = closed_dt)
Boston1$month <- as.numeric(format(Boston1$closed_dt, "%m"))
View(Boston1)

#4c find if there exist NA value in Boston1, if it returns true, then remove NA
#also keep data type as numberic again and recheck if there exist NA still. And find dimention.
anyNA(Boston1)
Boston2 <-mutate(Boston1, month = months(Boston1$closed_dt)) %>% na.omit(Boston1$month)
Boston2$month <- as.numeric(format(Boston2$closed_dt, "%m"))
View(Boston2)
anyNA(Boston2)
dim(Boston2)

#5 Drop clomun "open_dt" and check the dimention
Boston2 <- Boston2[,-2]
View(Boston2)
dim(Boston2)

#6 Calculate the percentage of closed in case_status
#my precint's closed percentage is 100%
#the overall percentage for entire dataset is 82.2%
dfsum <- summary(Boston2$case_status)
Perct <- dfsum["Closed"]/(dfsum["Closed"]+dfsum["Open"])
percent(Perct)
dfsum1 <- summary(Boston1$case_status)
Perct1 <- dfsum1["Closed"]/(dfsum1["Closed"]+dfsum1["Open"])
percent(Perct1)

#7 identify the six most common reason from my precinct
reason_kind <- group_by(Boston2, reason)
reason_n <-count(Boston2, reason)
reason_6 <- arrange(reason_n, desc(n)) 
head(reason_6,6)

#8 create a dataframe only contains the six reason
Boston3 <- filter(Boston2, reason == "Enforcement & Abandoned Vehicles"| reason == "Code Enforcement"|reason == "Street Cleaning"|reason == "Sanitation"|reason == "Highway Maintenance"|reason =="Park Maintenance & Safety") 
View(Boston3)
str(Boston3)

#9 Create a barplot to show the six most reasons
counts <- table(Boston3$reason)
ggplot(Boston3,aes(x=reason))+geom_bar(fill=rainbow(n=6))+ggtitle("The Number of Occurence For The Six Most Reasons")+theme(plot.title = element_text(hjust=0.3), axis.text.x = element_text(angle=33, hjust=0.9, vjust=1))

#10 Create a barplot to show the season of the year
Boston3$season <-rep("winter", nrow(Boston3))
Boston3$season <-ifelse(Boston3$month > 3, "Spring", Boston3$season)
Boston3$season <-ifelse(Boston3$month > 6, "Summer", Boston3$season)
Boston3$season <-ifelse(Boston3$month > 9, "Fall", Boston3$season)
View(Boston3)
ggplot(Boston3, aes(x=reason,fill=season))+geom_bar()+ggtitle('The Number of The Six Reasons Based on Season')+theme(plot.title = element_text(hjust=0.4), axis.text.x = element_text(angle=40, hjust=0.9, vjust=1))

#11 Create a barplot and fill in "ontime" dataset
ggplot(Boston3, aes(x=reason,fill=ontime))+geom_bar()+ggtitle('Reasons Based On Ontime Condition')+theme(plot.title = element_text(hjust=0.4), axis.text.x = element_text(angle=30, hjust=0.9, vjust=1))

#12 Create a barplot and fill in "season" dataset
ggplot(Boston3, aes(x=reason,fill=season)) + geom_bar()+ theme(axis.text.x=element_text(angle = -60, hjust = 0))+ggtitle("Six Reasons with Season")+ facet_grid(~source)

#13 Filter dataset that only contains rows with the 5 most common types
typea<-group_by(Boston3,type)
typeb<-count(Boston3,type)
type5<-arrange(typeb,desc(n))
head(type5,5)

five<-c("Parking Enforcement","Schedule a Bulk Item Pickup","Missed Trash/Recycling/Yard Waste/Bulk Item","Request for Pothole Repair","Requests for Street Cleaning")
type<-filter(Boston3,type %in% five)
table(Boston3$type)

#14 Create a ggplot fill with season in type dataset
ggplot(type, aes(x=reason,fill=season)) + geom_bar()+ theme(axis.text.x=element_text(angle = -50, hjust = 0))+ggtitle("Six Reasons with Season and Type")+facet_grid(~type)

#15 Using table function to see the number of levels for type variable
table(type$type)
type<-droplevels(type)
table(type$type)

#16 Create a mosaic plot with type and fill with the season data
ggplot(data=type) + geom_mosaic(aes( x=product(type), fill=season))+ theme(axis.text.x=element_text(angle = -60, hjust = 0))+ggtitle("Mosaic Plot For Five Types with Season Fill")

#17 Install "leaflet" package
install.packages("leaflet")
library(leaflet)

#18 Run the following code to get a map
type %>% leaflet() %>% setView(lng=-71.105, lat=42.35, zoom=10) %>%
  addCircles %>% addTiles()

#19 Run the following code to get a map
type %>% leaflet() %>% setView(lng=-71.105, lat=42.35, zoom=10) %>%
  addCircles %>% addTiles() %>% addProviderTiles(providers$Esri.WorldImagery)

#20 Run the following code to get a map
type %>% leaflet() %>% setView(lng=-71.105, lat=42.35, zoom=10) %>% addCircles %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% addMarkers(lng=-71.120996, lat=42.351444, popup="BU MET")



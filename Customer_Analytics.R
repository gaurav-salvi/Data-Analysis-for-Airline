## Acquiring the datatset ##

dataset<-read.csv("Satisfaction Survey.csv", stringsAsFactors = FALSE)

str(dataset)

## Cleaning the dataset

unique(dataset$Satisfaction)

index_1 <- which(dataset$Satisfaction=='4.00.2.00') 
index_2 <- which(dataset$Satisfaction=='4.00.5') 



index_1
index_2

dataset <- dataset[-index_1, ] 
dataset<- dataset[-index_2, ]

colSums(is.na(dataset))
data_clean <- filter(dataset,  !is.na(Arrival.Delay.in.Minutes))
colSums(is.na(dataset))

nrow(dataset)

unique(dataset$Satisfaction)

##Selecting an airline

data_low <- dataset[dataset$Satisfaction<4,]

# install.packages("dplyr")
# library(dplyr)

data_full<-group_by(dataset, Airline.Name)
data_summ_full<-summarise(data_full, Total_Customer=n())
View(data_summ_full)

data_Name<-group_by(data_low,Airline.Name)
data_summ<-summarise(data_Name ,Low_Customer=n())
View(data_summ)

data_comp<-merge(data_summ,data_summ_full)
data_comp$low_ratio<-(data_comp$Low_Customer/data_comp$Total_Customer)*100
View(data_comp)

data_clean<-dataset[(trimws(dataset$Airline.Name,which="right")=="Cheapseats Airlines Inc."),]
rownames(dataset)<-NULL

## Data Munging ##

colSums(is.na(data_clean))
data_clean <- filter(data_clean,  !is.na(Arrival.Delay.in.Minutes))
colSums(is.na(data_clean))

str(data_clean)
summary(data_clean)

unclean_names <- colnames(data_clean)
clean_names <- gsub("\\.", "_", unclean_names)
colnames(data_clean) <- clean_names

data_clean$Satisfaction <- as.numeric(data_clean$Satisfaction)

## Descriptive Statistics

###################################################
data_clean$Satisfaction <- as.numeric(as.character(data_clean$Satisfaction))


# group by the origin state
data_clean.groupByOrigin_City <- group_by(data_clean,Orgin_City)
originCityCount <- summarize(data_clean.groupByOrigin_City,count=n())
View(originCityCount)
originCityAvgSatisfaction <- summarize(data_clean.groupByOrigin_City, mean(Satisfaction))
View(originCityAvgSatisfaction)
# group by the destination state
data_clean.groupByDest_City <- group_by(data_clean, Destination_City)
DestCityCount <- summarize(data_clean.groupByDest_City,count=n())
View(DestCityCount)
DestCityAvgSatisfaction <- summarize(data_clean.groupByDest_City, mean(Satisfaction))
View(DestCityAvgSatisfaction)



colnames(originCityAvgSatisfaction) <- c("Origin_City","Mean_Satisfaction")
colnames(DestCityAvgSatisfaction) <- c("Destination_City","Mean_Satisfaction")
View(originCityAvgSatisfaction)
View(DestCityAvgSatisfaction)


ScatterPlot_Origin_City <- ggplot(originCityAvgSatisfaction,aes(x=Origin_City, y=Mean_Satisfaction))
ScatterPlot_Origin_City <- ScatterPlot_Origin_City + geom_point() + ggtitle("Satisfaction across Origin City")
ScatterPlot_Origin_City <- ScatterPlot_Origin_City + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ScatterPlot_Origin_City #scatterplot of origin city with satisfacion 

ScatterPlot_Destination_City <- ggplot(DestCityAvgSatisfaction,aes(x=Destination_City, y=Mean_Satisfaction))
ScatterPlot_Destination_City <- ScatterPlot_Destination_City + geom_point()+ ggtitle("Satisfaction across Destination City")
ScatterPlot_Destination_City <- ScatterPlot_Destination_City + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ScatterPlot_Destination_City #scatterplot of destination city with satisfacion 
################################################### 

#Maps

# group by the origin state
data_clean.groupByOrigin <- group_by(data_clean, Origin_State)
originStateCount <- summarize(data_clean.groupByOrigin,count=n())
originStateAvgSatisfaction <- summarize(data_clean.groupByOrigin, mean(Satisfaction))
View(originStateAvgSatisfaction)

# group by the destination state
data_clean.groupByDest <- group_by(data_clean, Destination_State)
destStateCount <- summarize(data_clean.groupByDest,count=n())
destStateAvgSatisfaction <- summarize(data_clean.groupByDest, mean(Satisfaction))
View(destStateAvgSatisfaction)

Destination_State <- state.name
area <- state.area
center <- state.center
df <- data.frame(Destination_State,area,center)
colnames(destStateAvgSatisfaction) <- c("Destination_State","Mean_of_Satisfaction")
otherDF <- merge(df, destStateAvgSatisfaction, all.x=TRUE)
otherDF$Destination_State <- tolower(otherDF$Destination_State)
us<- map_data("state")#use maps package for plotting with ggplot2

map.simple <- ggplot(otherDF, aes(map_id = Destination_State)) + guides(fill=guide_legend(title = "Mean of Satisfaction"))
map.simple <- map.simple + geom_map(map = us,aes(fill=Mean_of_Satisfaction))#plot map on basis of area of states
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)+ ggtitle("Distribution across Destination State")#define x and yaxis limits
map.simple

Origin_State <- state.name
area <- state.area
center <- state.center
df <- data.frame(Origin_State,area,center)
colnames(originStateAvgSatisfaction) <- c("Origin_State","Mean_of_Satisfaction")
otherDF <- merge(df, originStateAvgSatisfaction, all.x=TRUE)
View(otherDF)
otherDF$Origin_State <- tolower(otherDF$Origin_State)
us<- map_data("state")#use maps package for plotting with ggplot2
map.simple <- ggplot(otherDF, aes(map_id = Origin_State)) + guides(fill=guide_legend(title = "Mean of Satisfaction"))
map.simple <- map.simple + geom_map(map = us,aes(fill=Mean_of_Satisfaction))#plot map on basis of area of states
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat) + ggtitle("Distribution across Origin State")#define x and yaxis limits
map.simple

# status distribution

ggplot(data=data_clean)+geom_bar(mapping = aes(x=data_clean$Airline_Status)) + scale_x_discrete("Airline Status")

table(data_clean$Airline_Status, data_clean$Satisfaction)

table(data_clean$Airline_Status)

# satisfaction distribution of Blue

data_clean_Blue <- data_clean[data_clean$Airline_Status=='Blue',]

hist(data_clean_Blue$Satisfaction)

t_blue <- table(data_clean_Blue$Satisfaction)# 1,2,3,4,5:530 4431 5460 6539 619

n <- names(table(data_clean_Blue$Satisfaction))

d <- c(530, 4431, 5460, 6539, 619)

piepercent <- paste(round(100*d/sum(d),2),"%")

 

# class

summary(data_clean$Class)

table(data_clean$Class)

# class distribution

ggplot(data=data_clean)+geom_bar(mapping = aes(x=data_clean$Class))+ scale_x_discrete(name="Class") + ggtitle("Class Distribution")

table(data_clean$Class, data_clean$Satisfaction)

# mean by group, group: status
barplot(tapply(data_clean$Satisfaction, data_clean$Airline_Status, mean),col="light blue")
table(data_clean$Airline_Status)


ggplot(data_clean_sat,aes(x=Airline_Status,fill=Satisfaction))+geom_bar(position='dodge')+scale_fill_manual(values = colorblue)
str(data_clean_sat)
ggplot(data_clean_sat,aes(x=Airline_Status,fill=Satisfaction))+geom_bar(position='fill')+scale_fill_manual(values = colorblue)

# type of travel visulization
# type of travel distribution
table(data_clean$Type_of_Travel)
barplot(tapply(data_clean$Satisfaction, data_clean$Type_of_Travel, mean), col="light blue")
barplot(table(data_clean$Type_of_Travel), col = "light blue")
table(data_clean$Type_of_Travel, data_clean$Satisfaction)

data_clean_sat <- data_clean
data_clean_sat$Satisfaction <- as.factor(data_clean_sat$Satisfaction)
ggplot(data_clean_sat,aes(x=Type_of_Travel,fill=Satisfaction))+geom_bar(position='dodge')+scale_fill_manual(values = colorblue)
ggplot(data_clean_sat,aes(x=Type_of_Travel,fill=Satisfaction))+geom_bar(position='fill')+scale_fill_manual(values = colorblue)




#ggplot(data_clean, aes(y=Satisfaction, x=Class)) + geom_point() + labs(x="Class", y="customer satisfaction")

data_clean_sat <- data_clean# i will use data_clean_sat, because i has to change satisfaction from numeric to factor

data_clean_sat$Satisfaction <- as.factor(data_clean_sat$Satisfaction)

ggplot(data_clean_sat,aes(x=Class,fill=Satisfaction))+geom_bar(position='dodge')+scale_fill_manual(values = colorblue)

ggplot(data_clean_sat,aes(x=Class,fill=Satisfaction))+geom_bar(position='fill')+scale_fill_manual(values = colorblue)

createBucketsSurvey<-function(vec)
{ vBuckets <- replicate(length(vec), "Medium") 
  vBuckets[vec<3 ] <- "Low" 
  vBuckets[vec>3 ] <- "High" 
  return(vBuckets) }

sat_viz<-createBucketsSurvey(data_clean$Satisfaction)



male<- data_clean[data_clean$Gender=='Male',]
View(male) 
nrow(male)

hist(male$Satisfaction, xlab = "Satisfaction Rating",main = "Male Satisfaction Survey")

female<- data_clean[data_clean$Gender=='Female',]
View(female) 
nrow(female)

hist(female$Satisfaction, xlab = "Satisfaction Rating",main = "Female Satisfaction Survey")

ggplot(data_clean,aes(x=Gender,y=Satisfaction))+geom_boxplot()

agefunction<-function(vec)
{ vBuckets <- replicate(length(vec), "Adult") 
  vBuckets[vec <= 18] <- "Child" 
  vBuckets[vec >= 65] <- "Senior" 
  return(vBuckets) }


age<-agefunction(data_clean$Age) 
ggplot(data_clean,aes(x=age,fill=sat_viz))+geom_bar(position='dodge')+ guides(fill=guide_legend(title = "Satisfaction Range"))

createBucketsOther <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

price<-createBucketsOther(data_clean$Price_Sensitivity) 
ggplot(data_clean,aes(x=price,y=Satisfaction))+geom_boxplot()

sat_vize<-createBucketsSurvey(dataset$Satisfaction)
ggplot(dataset,aes(x=Airline.Name,fill=sat_vize))+geom_bar(position='dodge')+theme(axis.text.x = element_text(angle=90,hjust=1))+guides(fill=guide_legend(title = "Satisfaction Range"))


##Linear Regression ##

str(data_clean)

cheapseatnew<-data_clean[,c(-11,-12,-16:-21)]

str(cheapseatnew)


which(colnames(cheapseatnew)=="Flight_cancelled")
cheapseatnew <- cheapseatnew[,-(17)]

# cheapseatnew[1:21]<-lapply(cheapseatnew[1:21],as.factor)

str(cheapseatnew)

numairline<-as.factor(cheapseatnew$Airline_Status)
numgender<-as.factor(cheapseatnew$Gender)
numttravel<-as.factor(cheapseatnew$Type_of_Travel)
numclass<-as.factor(cheapseatnew$Class)
num5min<-as.factor(cheapseatnew$Arrival_Delay_greater_5_Mins)

cheaplineardata<-data.frame(cheapseatnew$Satisfaction,numairline,cheapseatnew$Age,numgender,
cheapseatnew$Price_Sensitivity,cheapseatnew$Year_of_First_Flight,cheapseatnew$No_of_Flights_p_a_,
cheapseatnew$X__of_Flight_with_other_Airlines,numttravel,cheapseatnew$No__of_other_Loyalty_Cards,
numclass,cheapseatnew$Scheduled_Departure_Hour,cheapseatnew$Departure_Delay_in_Minutes,
cheapseatnew$Flight_time_in_minutes,cheapseatnew$Flight_Distance,num5min)

str(cheaplineardata)

cheaplinear<-lm(cheapseatnew.Satisfaction~.,data=cheaplineardata)
summary(cheaplinear)

library('MASS') 
null<-lm(cheapseatnew.Satisfaction~1,cheaplineardata) 
stepAIC(cheaplinear, direction='backward')

lm_backward <- lm(cheapseatnew.Satisfaction ~ numairline + cheapseatnew.Age + 
     numgender + cheapseatnew.Price_Sensitivity + cheapseatnew.Year_of_First_Flight + 
     cheapseatnew.No_of_Flights_p_a_ + numttravel + numclass + 
     cheapseatnew.Scheduled_Departure_Hour + num5min, data = cheaplineardata)

summary(lm_backward)

stepAIC(null,direction='forward',scope=list(upper=cheaplinear,lower=null))

lm_forward <- lm(formula = cheapseatnew.Satisfaction ~ numttravel + numairline + 
                   num5min + numgender + cheapseatnew.No_of_Flights_p_a_ + cheapseatnew.Age + 
                   cheapseatnew.Price_Sensitivity + numclass + cheapseatnew.Scheduled_Departure_Hour + 
                   cheapseatnew.Year_of_First_Flight, data = cheaplineardata)

summary(lm_forward)



# Association Rules ##

colSums(is.na(data_arules))
data_clean <- filter(data_arules,  !is.na(Arrival_Delay_in_Minutes))
colSums(is.na(data_arules))

createBuckets <- function(v){
  vBuckets <- replicate(length(v), "Average")
  vBuckets[v > 3] <- "High"
  vBuckets[v <= 3] <- "Low"
  return(vBuckets)
}

data_arules = data_clean

str(data_arules)

satcust <- createBuckets(data_arules$Satisfaction)
# satcust

# price sensitivity
priceSen <- createBuckets(data_clean$Price_Sensitivity)
# priceSen


createBucketsCard <- function(v){
  vBuckets <- replicate(length(v), "No")
  vBuckets[v > 0] <- "Yes"
  vBuckets[v >= 2] <- "more"
  return(vBuckets)
}

# No..of other loyalty cards
createBucketsCard <- function(v){
  vBuckets <- replicate(length(v), "No")
  vBuckets[v > 0] <- "Yes"
  vBuckets[v >= 2] <- "more"
  return(vBuckets)
}

NumCards <- createBucketsCard(data_arules$No__of_other_Loyalty_Cards)
# NumCards

# age
# summary(data_arules$Age)
createBucketsAge <- function(v){
  vBuckets <- replicate(length(v), "teenager")
  vBuckets[v >= 20] <- "adult"
  vBuckets[v >= 65] <- "senior"
  return(vBuckets)
}
age <- createBucketsAge(data_arules$Age)
# age

createBucketsOther <- function(vec){
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

# No. of flight p.a
NumFlight <- createBucketsOther(data_arules$No_of_Flights_p_a_)
# NumFlight
# shopping amount at airport
shopping <- createBucketsOther(data_arules$Shopping_Amount_at_Airport)
eatdrink <- createBucketsOther(data_arules$Eating_and_Drinking_at_Airport)
# departure hour
departureHour <- createBucketsOther(data_arules$Scheduled_Departure_Hour)
# flight time in minutes

colSums(is.na(data_arules))

flightTime <- createBucketsOther(data_arules$Flight_time_in_minutes)
# flight distance
flightDis <- createBucketsOther(data_arules$Flight_Distance)
# X.. of flight with other airline
otherAirline <- createBucketsOther(data_arules$X__of_Flight_with_other_Airlines)
# otherAirline




# delay in munutes
createBucketsDelay <- function(v){
  vBuckets <- replicate(length(v), "NoDelay")
  vBuckets[v > 0] <- "small"
  vBuckets[v >= 17] <- "middle"
  vBuckets[v >= 45] <- "long"
  return(vBuckets)
}
# departure delay
depDelay <- createBucketsDelay(data_arules$Departure_Delay_in_Minutes)
# depDelay
# arrive delay
arrDelay <- createBucketsDelay(data_arules$Arrival_Delay_in_Minutes)
# arrDelay

# other categracal variable
status <- data_arules$Airline_Status
gender <- data_arules$Gender
firstflight <- data_arules$Year_of_First_Flight
typetravel <- data_arules$Type_of_Travel
class <- data_arules$Class
airlinename <- data_arules$Airline_Name
dayOfMonth <- data_arules$Day_of_Month
flightdate <- data_arules$Flight_date
depState <- data_arules$Origin_State
arrState <- data_arules$Destination_State
delay5min <- data_arules$Arrival_Delay_greater_5_Mins
cancel <- data_arules$Flight_cancelled


data_arules <- data.frame(satcust, priceSen, otherAirline,NumCards,age, NumFlight, shopping, eatdrink,departureHour,flightTime,flightDis,depDelay,
                          arrDelay, status, gender, firstflight, typetravel,class, delay5min, cancel )

data_arules[1:20]<-lapply(data_arules[1:20],as.factor)

str(data_arules)

data_arules.trans <- as(data_arules, "transactions")
itemFrequencyPlot(data_arules.trans, support=0.3,cex.names=0.6)

ruleset_High <- apriori(data_arules.trans, parameter=list(support=0.2, confidence=0.2),
                   appearance=list(default="lhs",rhs=("satcust=High")))

ruleset_Low <- apriori(data_arules.trans, parameter=list(support=0.1, confidence=0.5),
                    appearance=list(default="lhs",rhs=("satcust=Low")))

ruleset_high <- sort(ruleset_High, decreasing=TRUE, by="confidence")
ruleset_low <- sort(ruleset_Low, decreasing=TRUE, by="confidence")

library(grid)
library(arulesViz)

plot(ruleset_high)
plot(ruleset_low)

goodrules_high <- ruleset_high[quality(ruleset_high)$lift > 1.4]
goodrules_high

goodrules_high <- sort(goodrules_high, descreasing=TRUE, by="lift")

inspect(head(goodrules_high,15))

plot(goodrules_high)

inspect(goodrules_high)

goodrules_low <- ruleset_low[quality(ruleset_low)$lift > 1.95]
goodrules_low

goodrules_low <- sort(goodrules_low, descreasing=TRUE, by="lift")

inspect(head(goodrules_low,20))
plot(goodrules_low)







##Support Vector Machine ##


data_clean$happy_customer <- createBuckets(data_clean$Satisfaction)

dim(data_clean)

table(data_clean$happy_customer)

randIndex <- sample(1:dim(data_clean)[1])
summary(randIndex)
length(randIndex)
head(randIndex)
cutPoint2_3 <- floor(2 * dim(data_clean)[1]/3)
cutPoint2_3
trainData <- data_clean[randIndex[1:cutPoint2_3],]
View(trainData)
testData <-data_clean[randIndex[(cutPoint2_3+1):dim(data_clean)[1]],]
View(testData)
dim(trainData)
dim(testData)

install.packages('kernlab')
library(kernlab)

str(trainData)

svmOutput <- ksvm(happy_customer ~ Type_of_Travel + Age + Airline_Status + No_of_Flights_p_a_ + Scheduled_Departure_Hour + Gender + Arrival_Delay_greater_5_Mins + Class + Price_Sensitivity + Airline_Status , data=trainData,kernel= "rbfdot",kpar = "automatic",C=5,cross=3,prob.model=TRUE)
svmOutput

Pred <- predict(svmOutput, testData, type = "votes")


compTable2 <- data.frame(testData[,29], Pred[2,])
comp2 <- table(compTable2)
comp2

fourfoldplot(comp2, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

error_rate_percentage <- (comp2[2,1] + comp2[1,2])/nrow(testData)*100
error_rate_percentage


## End of Code ##

































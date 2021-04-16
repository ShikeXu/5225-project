setwd("~/Desktop/2021spring/5225/group_assign/5225-project")
library(readxl)
library(dplyr)

Data<-read_xls("~/Desktop/2021spring/5225/group_assign/5225-project/2019.xls",skip = 3)
head(Data,12)
### rename incorrect colnames
colnames(Data)[c(3)] <- c("Actual")
colnames(Data)[c(5)] <- c("ViolentCrime")
colnames(Data)[c(6)] <- c("Murderandnonnegligentmanslaughter")
colnames(Data)[c(10)] <- c("PropertyCrime")


# Check categories of column Area
table(Data$Area)
Data$Area[which(Data$Area == "Total")]<-"State Total"
ttlpos<-which(Data$Area == "State Total")

Population<-Data[ttlpos,4]
State<-Data[which(Data$Area == "Metropolitan Statistical Area"),1]

pop2019<-data.frame(State, Population)
pop2019

#write.csv(pop2019, file = "~/Desktop/2021spring/5225/group_assign/5225-project/2019population.csv")


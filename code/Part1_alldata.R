setwd("~/Desktop/2021spring/5225/group_assign/5225-project/raw data")
library(dplyr)

alldata_Part1 <- data.frame()
for(i in 1999:2019){
  path1 <- paste0("~/Desktop/2021spring/5225/group_assign/5225-project/raw data/Data",i, "_Part1.csv")
  dt1 <-  read.csv(file = path1, header = TRUE) 
  dt1$Year<-rep(i,nrow(dt1))
  dt1<-dt1[,c('Year','State','Area','Actual','Percentage',
              'ViolentCrime','Murderandnonnegligentmanslaughter', 
              'Robbery','Aggravatedassault','PropertyCrime',
              'Burglary','Larcenytheft','Motorvehicletheft')]
  alldata_Part1<-rbind(alldata_Part1,dt1)
}

View(alldata_Part1)

write.csv(alldata_Part1, file = "~/Desktop/alldata_Part1.csv")

table(alldata_Part1$Year)
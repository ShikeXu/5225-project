setwd("~/Desktop/2021spring/5225/group_assign/5225-project/raw data")
library(dplyr)

alldata_Part2 <- data.frame()

for(i in 1999:2004){
  path2 <- paste0("~/Desktop/2021spring/5225/group_assign/5225-project/raw data/Data",1999, "_Part2.csv")
  dt2 <-  read.csv(file = path2, header = TRUE, skip=1) 
  dt2$Year<-rep(i,nrow(dt2))
  dt2<-dt2[,c("Year", "State", "ViolentCrime", "Murderandnonnegligentmanslaughter", "Robbery", 
              "Aggravatedassault", "PropertyCrime", "Burglary", "Larcenytheft", "Motorvehicletheft")]
  alldata_Part2<-rbind(alldata_Part2,dt2)
}
colnames(dt2)
for(i in 2005:2019){
  path2 <- paste0("~/Desktop/2021spring/5225/group_assign/5225-project/raw data/Data",i, "_Part2.csv")
  dt2 <-  read.csv(file = path2, header = TRUE) 
  dt2$Year<-rep(i,nrow(dt2))
  dt2<-dt2[,c("Year", "State", "ViolentCrime", "Murderandnonnegligentmanslaughter", "Robbery", 
              "Aggravatedassault", "PropertyCrime", "Burglary", "Larcenytheft", "Motorvehicletheft")]
  alldata_Part2<-rbind(alldata_Part2,dt2)
}

View(alldata_Part2)

write.csv(alldata_Part2, file = "~/Desktop/alldata_Part2.csv")


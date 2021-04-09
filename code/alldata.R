

library(dplyr)
alldata_Part1 <- data.frame()
alldata_Part2 <- data.frame()

for(i in 1999:2019){
  path1 <- paste0("C:/Users/sirius/Desktop/5225project/5525-project/raw data/Data",i, "_Part1.csv")
  dt1 <-  read.csv(file = path1, header = TRUE) 
  dt1$Year<-rep(i,nrow(dt1))
  dt1<-dt1[,c('Year','State','Area','Actual','Percentage',
            'ViolentCrime','Murderandnonnegligentmanslaughter', 
            'Robbery','Aggravatedassault','PropertyCrime',
            'Burglary','Larcenytheft','Motorvehicletheft')]
  alldata_Part1<-rbind(alldata_Part1,dt1)
}



for(i in 1999:2019){
  path2 <- paste0("C:/Users/sirius/Desktop/group assignment/data×îÖÕÕûºÏ/Data",i, "_Part2.csv")
  dt2 <-  read.csv(file = path2, header = TRUE) 
  dt2$Year<-rep(i,nrow(dt2))
  dt2<-dt2[,c('Year','State',
              'ViolentCrime','Murderandnonnegligentmanslaughter', 
              'Robbery','Aggravatedassault','PropertyCrime',
              'Burglary','Larcenytheft','Motorvehicletheft')]
  alldata_Part2<-rbind(alldata_Part2,dt2)
}

View(alldata_Part1)
View(alldata_Part2)

write.csv(alldata_Part1,'C:/Users/sirius/Desktop/group assignment/alldata_Part1.csv')
write.csv(alldata_Part2,'C:/Users/sirius/Desktop/group assignment/alldata_Part2.csv')



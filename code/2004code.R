library(readxl)
Data2004<-read_xls("~/Desktop/2021spring/5225/group_assign/2004.xls",skip = 3)
str(Data2004)
Data2004<-Data2004[,1:11]
colnames(Data2004)<-c("Area", "Population", "Violent crime", "Murder and nonnegligent manslaughter",
                      "Forcible rape", "Robbery", "Aggravated assault", "Property crime",
                      "Burglary", "Larceny-theft", "Motor vehicle theft")

# Reform the Area/State
# First we need to obtain the names for all the states in the data
StateName<-rep(NA,nrow(Data2004))

for (i in 1:nrow(Data2004)) {
  if(sum(is.na(Data2004[i,2:11])) == 10){
    StateName[i]<-Data2004[i,1]
  }
}
StateName<-StateName[-which(is.na(StateName))]
StateName
# drop useless information at the bottom of the data table
StateName<-StateName[1:52]

# get State Positions, Area Positions, Actual Positions, Estimated Positions, 
# Total positions, and Rate Positions
StateNamePos<-which(Data2004$Area %in% StateName)
AreaPos<-which(Data2004$Area %in% 
                 c('Metropolitan Statistical Area','Cities outside metropolitan areas','Nonmetropolitan counties'))
ActualPos<-which(Data2004$Area =='Area actually reporting')
EstimPos<-which(Data2004$Area =='Estimated total')
TotalPos<-which(Data2004$Area =='State Total')
RatePos<-which(Data2004$Area =='Rate per 100,000 inhabitants')

# drop NA rows and digits in Var names
tail(Data2004,10)# the last 6 rows are useless
Data2004<-Data2004[1:(nrow(Data2004)-6),]

StateName<-gsub('\\d','',StateName)
Data2004$Area[StateNamePos]<-StateName
colnames(Data2004)<-gsub('[^A-Za-z]','',colnames(Data2004))


### assign [State] & [Area] type to Actual/Estimated data
# NOTE: this code is different for different years!!!
# choose the colomns you need
Data2004_Part1<-subset(Data2004, select = -c(Area))
Data2004_Part1$State<-rep('', nrow(Data2004_Part1))
Data2004_Part1$Area<-rep(NA, nrow(Data2004_Part1))
Data2004_Part1$Actual<-rep(NA, nrow(Data2004_Part1))
for(i in 1:(length(StateNamePos)-1)){
  Data2004_Part1$State[(StateNamePos[i]+1):(StateNamePos[i+1]-1)]<-StateName[i]
  Data2004_Part1$State[(tail(StateNamePos,1)+1):(nrow(Data2004_Part1))]<-tail(StateName,1)
}

for (i in ActualPos) {
  if(Data2004$Area[i-1]=='Metropolitan Statistical Area'){
    Data2004_Part1$Area[i]='M'
    Data2004_Part1$Actual[i]="Y"
  }
  if(Data2004$Area[i-1]=='Cities outside metropolitan areas'){
    Data2004_Part1$Area[i]='O'
    Data2004_Part1$Actual[i]="Y"
  }    
  if(Data2004$Area[i-1]=='Nonmetropolitan counties'){
    Data2004_Part1$Area[i]='R'
    Data2004_Part1$Actual[i]="Y"
  }
}

for (i in EstimPos) {
  if(Data2004$Area[i-2]=='Metropolitan Statistical Area'){
    Data2004_Part1$Area[i]='M'
    Data2004_Part1$Actual[i]="N"
  }
  if(Data2004$Area[i-2]=='Cities outside metropolitan areas'){
    Data2004_Part1$Area[i]='O'
    Data2004_Part1$Actual[i]="N"
  }    
  if(Data2004$Area[i-2]=='Nonmetropolitan counties'){
    Data2004_Part1$Area[i]='R'
    Data2004_Part1$Actual[i]="N"
  }
}

colnames(Data2004_Part1)[colnames(Data2004_Part1) == 'Population'] <- 'Percentage'
colnames(Data2004_Part1)[colnames(Data2004_Part1) == 'Violentcrime'] <- 'ViolentCrime'
colnames(Data2004_Part1)[colnames(Data2004_Part1) == 'Propertycrime'] <- 'PropertyCrime'

Data2004_Part1<-Data2004_Part1[,c('State','Area','Actual','Percentage','ViolentCrime','PropertyCrime',
                                  'Murderandnonnegligentmanslaughter',
                                  'Forciblerape','Robbery','Aggravatedassault','Burglary','Larcenytheft',
                                  'Motorvehicletheft')]

Data2004_Part1<-Data2004_Part1[which(is.na(Data2004_Part1$Area)*is.na(Data2004_Part1$Actual) == 0),]


#############################################################################################################
Data2004_Part2<-subset(Data2004,select = -c(Population,Area))
Data2004_Part2<-Data2004_Part2[RatePos,]
Data2004_Part2<-cbind(State=StateName,Data2004_Part2)
str(Data2004_Part2)


write.csv(Data2004_Part1,"~/Desktop/2021spring/5225/group_assign/Data2004_Part1.csv", row.names = FALSE)
write.csv(Data2004_Part2,"~/Desktop/2021spring/5225/group_assign/Data2004_Part2.csv", row.names = FALSE)






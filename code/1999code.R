library(readxl)
Data1999<-read_xls('C:/Users/sirius/Desktop/1995-2003/1999.xls',skip = 3)
Data1999<-Data1999[,-11]
str(Data1999[,-11])
# Reform the Area/State
attach(Data1999)

StateNA<-which(is.na(Area))
# get State Positions and Area Positions
StateNamePos<-(StateNA[seq(1,length(StateNA)-1,by=2)]+StateNA[seq(2,length(StateNA)-1,by=2)])/2
AreaPos<-which(Area %in% c('Metropolitan Statistical Area','Cities outside metropolitan areas','Rural'))
ActualPos<-which(Area =='Area actually reporting')
EstimPos<-which(Area =='Estimated totals')
TotalPos<-which(Area =='State Total')
RatePos<-which(Area =='Rate per 100,000 inhabitants')
# drop useless information and drop digits in Var names
Data1999<-Data1999[1:tail(StateNA,1)-1,]
StateName<-Area[StateNamePos]
StateName<-gsub('\\d','',StateName)
Area[StateNamePos]<-StateName
colnames(Data1999)<-gsub('[^A-Za-z]','',colnames(Data1999))
detach(Data1999)
### assign [State] & [Area] type to Actual/Estimated data
# NOTE: this code is different for different years!!!
# choose the colomns you need
Data1999_Part1<-subset(Data1999,select = -c(ModifiedCrimeIndexTotal,Area,Arson))
Data1999_Part1$State<-rep('',nrow(Data1999_Part1))
Data1999_Part1$Area<-rep('',nrow(Data1999_Part1))
Data1999_Part1$Actual<-rep('',nrow(Data1999_Part1))
for (i in 1:(length(StateNamePos)-1)){
  Data1999_Part1$State[(StateNamePos[i]+2):(StateNamePos[i+1]-2)]=StateName[i]
  Data1999_Part1$State[(tail(StateNamePos,1)+2):nrow(Data1999_Part1)]=tail(StateName,1)
  }

for (i in ActualPos) {
  if(Data1999$Area[i-1]=='Metropolitan Statistical Area'){
  Data1999_Part1$Area[i]='M'
  Data1999_Part1$Actual[i]="Y"
  }
  if(Data1999$Area[i-1]=='Cities outside metropolitan areas'){
  Data1999_Part1$Area[i]='O'
  Data1999_Part1$Actual[i]="Y"
  }    
  if(Data1999$Area[i-1]=='Rural'){
  Data1999_Part1$Area[i]='R'
  Data1999_Part1$Actual[i]="Y"
  }
}

for (i in EstimPos) {
  if(Data1999$Area[i-2]=='Metropolitan Statistical Area'){
    Data1999_Part1$Area[i]='M'
    Data1999_Part1$Actual[i]="N"
  }
  if(Data1999$Area[i-2]=='Cities outside metropolitan areas'){
    Data1999_Part1$Area[i]='O'
    Data1999_Part1$Actual[i]="N"
  }    
  if(Data1999$Area[i-2]=='Rural'){
    Data1999_Part1$Area[i]='R'
    Data1999_Part1$Actual[i]="N"
  }
}

colnames(Data1999_Part1)[colnames(Data1999_Part1) == 'Population'] <- 'Percentage'
Data1999_Part1<-Data1999_Part1[sort(c(EstimPos,ActualPos)),]
Data1999_Part1<-Data1999_Part1[,c('State','Area','Actual','Percentage','ViolentCrime','PropertyCrime','Murderandnonnegligentmanslaughter',
                                  'Forciblerape','Robbery','Aggravatedassault','Burglary','Larcenytheft','Motorvehicletheft')]
with(Data1999_Part1,
     {State<-as.factor(State)
     Area<-as.factor(Area)
     Actual<-as.factor(Actual)
     })
###

### 
Data1999_Part2<-subset(Data1999,select = -c(Population,ModifiedCrimeIndexTotal,Area,Arson))
Data1999_Part2<-Data1999_Part2[RatePos,]
Data1999_Part2<-cbind(State=StateName,Data1999_Part2)
###
write.csv(Data1999_Part1,"C:/Users/sirius/Desktop/1995-2003/Data1999_Part1.csv", row.names = FALSE)
write.csv(Data1999_Part2,"C:/Users/sirius/Desktop/1995-2003/Data1999_Part2.csv", row.names = FALSE)



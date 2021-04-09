library(readxl)
Data1999<-read_xls('C:/Users/sirius/Desktop/1995-2003/1999.xls',skip = 3)
str(Data1999)
# Reform the Area/State
attach(Data1999)

StateNA<-which(is.na(Area))
# drop useless information
Data1999<-Data1999[1:tail(StateNA,1)-1,]
# get State Positions and Area Positions
StateNamePos<-(StateNA[seq(1,length(StateNA)-1,by=2)]+StateNA[seq(2,length(StateNA)-1,by=2)])/2
AreaPos<-which(Area %in% c('Metropolitan Statistical Area','Cities outside metropolitan areas','Rural'))
ActualPos<-which(Area =='Area actually reporting')
EstimPos<-which(Area =='Estimated totals')
TotalPos<-which(Area =='State Total')

### assign [State] & [Area] type to Actual/Estimated data
code here
Data1999_Part1 colnames=('State','Area','Percentage','Crime Index Total','Violent Crime',...)
###

### 
code here
Data1999_Part2 colnames=('State','Population','State total','Violent Crime',...)
###

# remove digits from the State Names
StateName<-gsub('\\d','',StateName)
Area[StateNamePos]<-StateName


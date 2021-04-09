#function 2
#for instance
#Func 2(strat year, end year, crime, state range,type) 
#Func2( start_year = 2001, end_year = 2005, crime=c(“Robbery”, “Burglary”), State range=c(“ALABAMA”,”CONNECTICUT”)) 
#returns the variation trend plot for the crimes in ALABAMA and CONNECTICUT 
#with the type of 'Robbery' and 'Burglary' from 2001 to 2005.



#data for function testing
data<-data.frame(year = c(seq(2000,2002),seq(2000,2002),seq(2000,2002)),
                 StateName = c(rep("ALABAMA",3), c(rep("CONNECTICUT",3)), c(rep(" NEW YORK",3))),
                 Burglary = c(seq(1,9)),
                 Robbery = c(5,3,4,5,0,7,8,9,10),
                 Theft = c(rep(0,9)),
                 violent = c(rep(99,9)),
                 property = c(rep(100,9)))


start_year<-2000
end_year<-2002
state<-c("ALABAMA","CONNECTICUT")
crime<-c("Robbery", "Burglary")

years<-seq(start_year, end_year)
col<-c(4,3)




#data processing

state_rep<-NA

for (i in 1:length(state)) {
  state_rep = c(rep(state[i],length(crime)),state_rep)
  
}
state_rep<-state_rep[-which(is.na(state_rep))]


crime_rep<-rep(crime,length(state))


name<-rep(NA,(length(state)*length(crime)))

for (i in 1:(length(state)*length(crime))) {
  name[i] = paste(state_rep[i], '_', crime_rep[i],sep = '')
}

name


y<-data.frame(matrix(data = rep(NA, length(crime)*length(years)*length(state)),nrow = length(years)))
colnames(y)<-name
y
col<-rep(col,length(state))

for (j in 1:(length(state)*length(crime))) {
  for(i in 1:length(years)){
    y[i,j] = data[which((data$StateName == state_rep[j]) & (data$year == years[i])),col[j]]
  }
  
}

y

x<-years

#seperate plots
for(i in 1:ncol(y)){
  plot(x,y[,i], type = 'o',
       main = paste("plot for ", name[i]))
}



#overlapping plots according to state
library(dygraphs)
for (i in 1:length(state)) {
  Data<-data.frame(
    time = years,
    y[,seq(1+(i-1)*length(crime),i*length(crime))]
  )
  png(filename = paste0(i, "_", ".jpg"),width = 2400,height = 1800,res = 300)
  print(dygraph(Data))
  dev.off()
}


#overlapping plots according to crime
for (i in 1:length(crime)) {
  Data<-data.frame(
    time = years,
    y[,seq(i, i + (length(state) - 1)*length(crime), by= length(crime))]
  )
  png(filename = paste0(i, "_", ".jpg"),width = 2400,height = 1800,res = 300)
  print(dygraph(Data))
  dev.off()
}










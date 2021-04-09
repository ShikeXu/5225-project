setwd("~/Desktop/2021spring/5225/group_assign")
library(dygraphs)
p2data<-read.csv("alldata_Part2.csv")
p2data<-p2data[,-1]

trend<-function(state, start_year, end_year, crime, by_state){
  col<-rep(NA, length(crime))
  for (i in 1:length(crime)) {
    if (crime[i] == "ViolentCrime"){
      col[i] = 3
    }
    if (crime[i] == "Murderandnonnegligentmanslaughter"){
      col[i] = 4
    }
    if (crime[i] == "Robbery"){
      col[i] = 5
    }
    if (crime[i] == "Aggravatedassault"){
      col[i] = 6
    }
    if (crime[i] == "PropertyCrime"){
      col[i] = 7
    }
    if (crime[i] =="Burglary"){
      col[i] = 8
    }
    if (crime[i] == "Larcenytheft"){
      col[i] = 9
    }
    if (crime[i] == "Motorvehicletheft"){
      col[i] =10
    }
  }
  
  
  #data processing
  
  state_rep<-as.character()
  
  for (i in 1:length(state)) {
    state_rep = c(state_rep,rep(state[i],length(crime)))
    
  }
  
  
  crime_rep<-rep(crime,length(state))
  
  
  name<-as.character()
  
  for (i in 1:(length(state)*length(crime))) {
    name[i] = paste(state_rep[i], '_', crime_rep[i],sep = '')
  }
  
  years<-seq(start_year, end_year)
  
  
  
  subset<-data.frame(matrix(data = rep(NA, length(crime)*length(years)*length(state)),nrow = length(years)))
  
  colnames(subset)<-name
  
  col<-rep(col,length(state))
  
  for (j in 1:(length(state)*length(crime))) {
    for(i in 1:length(years)){
      subset[i,j] = p2data[which((p2data$State == state_rep[j]) & (p2data$Year == years[i])),col[j]]
    }
  }
  
  
  
  #overlapping plots according to state
  if(by_state == TRUE){
    for (i in 1:length(state)) {
      Data<-data.frame(
        time = years,
        subset[,seq(1+(i-1)*length(crime),i*length(crime))]
      )
      png(filename = paste0(i, "_", ".jpg"),width = 2400,height = 1800,res = 300)
      print(dygraph(Data))
      dev.off()
    } 
  }
  
  if(by_state == FALSE){
    for (i in 1:length(crime)) {
      Data<-data.frame(
        time = years,
        subset[,seq(i, i + (length(state) - 1)*length(crime), by= length(crime))]
      )
      png(filename = paste0(i, "_", ".jpg"),width = 2400,height = 2400,res = 300)
      print(dygraph(Data))
      dev.off()
    }
  }
  
  
}



trend(c("ALABAMA", "CONNECTICUT"), 2002, 2009, c("Burglary", "Robbery"), by_state = TRUE)
trend(c("CONNECTICUT", "ALASKA", "ARIZONA"), 2002, 2010, c("ViolentCrime", "PropertyCrime"), by_state = FALSE)




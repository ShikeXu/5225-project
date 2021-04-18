library(dplyr)
library(magrittr)
library(trend)
alldata_Part1<-read.csv('C:/Users/sirius/Desktop/5225project/5525-project/raw data/alldata_Part1.csv')
alldata_Part2<-read.csv('C:/Users/sirius/Desktop/5225project/5525-project/raw data/alldata_Part2.csv') 

# test the trend of increasing in crime rate
trend.test<-function(year,state,crime){
  cat(format('state',width = 12),format('crime type',width = 20),
      format('estimate',width = 9,justify = 'right'),format('p-value',width = 9,justify = 'right'),
      format('MK.test',width = 9,justify = 'right'),'\n')
  for (s in state) {
    for (c in crime) {
      dt<-
        alldata_Part2 %>%
        filter(State==s & Year %in% year) 
      
      lm_coef<-
        dt %$%
        lm(.[,c]~.[,'Year']) %>%
        summary() %$%
        coefficients[2,c(1,4)]
      
      mk_test<-
        dt %$%
        mk.test(.[,c]) %$%
        p.value
      cat(format(s,width=12),format(c,width=20),format(lm_coef,width = 9,digits = 2),format(mk_test,width = 9,digits = 2),'\n')
    }
  }
}

trend.test(year = 1999:2005,state = c('ALABAMA','FLORIDA'),crime = c('ViolentCrime','PropertyCrime'))
trend.test(year = 2003:2019,state = c('ALASKA','MAINE','NEVADA'),crime = c('Robbery','Burglary'))

# find the state pairs with close pop

popmatch_2019<-function(s){

  o<-order(abs(pop[state==s]-pop[]))[2:6]
  pair<-state[o]
  diff<-abs(pop[state==s]-pop[o])
  return(data.frame(pair,diff))
}

# test

popmatch_2019("CONNECTICUT")
pop2019<-read.csv('C:/Users/sirius/Desktop/5225project/5525-project/raw data/2019population.csv')[,-1]
state<-pop2019$State
pop<-pop2019$Population
popmatch_2019("CONNECTICUT")

trend.test(year = 2001:2019,state = c("UTAH", "CONNECTICUT","OKLAHOMA","IOWA"),crime = c("PropertyCrime"))  

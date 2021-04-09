library(dplyr)
library(magrittr)
library(trend)
alldata_Part1<-read.csv('C:/Users/sirius/Desktop/group assignment/alldata_Part1.csv')
alldata_Part2<-read.csv('C:/Users/sirius/Desktop/group assignment/alldata_Part2.csv') 

# test the trend of increasing in crime rate
trend.test<-function(year,state,crime){
  cat(format('state',width = 12),format('crime type',width = 20),
      format('estimate',width = 9,justify = 'right'),format('std error',width = 9,justify = 'right'),format('p-value',width = 9,justify = 'right'),
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
        coefficients[2,-3]
      
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

popmatch<-function(state,pop){
  pair=c()
  diff=c()
  
  for (i in 1:length(pop)) {
    o<-order(abs(pop[i]-pop[]))[2:6]
    pair<-c(pair,paste(state[i],state[o]))
    diff<-c(diff,abs(pop[i]-pop[o]))
  }
  
  data.frame(pair,diff) %>%
    .[order(diff)[1:20],] %>%
  return()
}

# test

popmatch(state = dt2$State,pop = pop)



  

  

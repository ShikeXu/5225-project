library(ggplot2)
library(tidyr)
library(dplyr)


dt <- read.csv("/Users/mli0316/Desktop/UConn/stat 5225/group proj/raw data/alldata_Part1.csv")


### By area by crime type ###

visual.fun <- function(year=NULL, state = 'WISCONSIN',  type='reported', data=dt){
  
  #mydata <- paste(paste("Data", year, sep=""), "Part1", sep = "_")
  indicator <- ifelse(type=='reported', 'Y', 'N')
  
  if(!is.null(year)){
    mydata <- data[which(data$Year == year),]
    
    fig <- 
      mydata  %>%
      dplyr::filter(State == state) %>%
      gather(., crimetype, number_of_crime, ViolentCrime:Motorvehicletheft, factor_key = TRUE) %>%
      dplyr::filter(Actual == indicator)%>%
      dplyr::filter(!is.na(as.numeric(number_of_crime)))
    
    fig$number_of_crime <- as.numeric(fig$number_of_crime)
    
    min.dt <- min(fig$number_of_crime, na.rm = T)
    max.dt <- max(fig$number_of_crime, na.rm = T)
    
    p1 <- ggplot(aes(x=crimetype, y=number_of_crime,fill=Area), data=fig) +
      geom_bar(stat="identity", width=.5, position = "dodge") +
      facet_wrap(~Area)+
      scale_y_continuous(breaks = seq(min.dt, max.dt, length.out = 5)) + 
      xlab("Crime Type") +
      ylab("Number of Cases") + 
      ggtitle(paste("Barplot of Crime Type by Area for State", state, sep=" ")) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 1),
            legend.position = 'none') 
    
    
    ### by crime type ###
    
    fig1 <- 
      mydata  %>%
      dplyr::filter(State == state) %>%
      gather(., crimetype, number_of_crime, ViolentCrime:Motorvehicletheft, factor_key = TRUE) %>%
      dplyr::filter(Actual == indicator) %>%
      group_by(State, crimetype) %>%
      summarise(total = sum(as.numeric(number_of_crime))) %>%
      dplyr::filter(!is.na(total))
    
    
    min.dt <- min(fig1$total, na.rm = T)
    max.dt <- max(fig1$total, na.rm = T)
    
    
    p2 <- ggplot(aes(x=crimetype, y=total), data=fig1) +
      geom_bar(stat="identity", width=.5, position = "dodge") +
      scale_y_continuous(breaks = seq(min.dt, max.dt, length.out = 5)) + 
      xlab("Crime Type") +
      ylab("Number of Cases") + 
      ggtitle(paste("Barplot of Crime Type for State", state, sep=" ")) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 1),
            legend.position = 'none') 
    
    
    ### Spine plot ###
    
    p3 <- ggplot(aes(x=crimetype, y=number_of_crime, fill=Area), data=fig) +
      geom_bar(stat="identity", position="fill") +
      xlab("Crime Type") +
      ylab("Number of Cases") + 
      ggtitle(paste("Spineplot of Crime Type for State", state, sep=" ")) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 1)) 
    
    return.list <- list("Barplot.by.area" = p1, 
                        "Barplot.by.type" = p2,
                        "spineplot" = p3)
  }
  
#  if(is.null(year)){
#   ## property crime & violent crime ## 
    
    
#    fig1 <- 
#      dt  %>%
#      dplyr::filter(State == state) %>%
#      gather(., crimetype, number_of_crime, ViolentCrime:Motorvehicletheft, factor_key = TRUE) %>%
#      dplyr::filter(Actual == indicator) %>%
#      group_by(Year, State, crimetype) %>%
#     summarise(total = sum(as.numeric(number_of_crime))) %>%
#      dplyr::filter(!is.na(total)) %>%
#      dplyr::filter(crimetype %in% c("PropertyCrime", "ViolentCrime")) %>%
#      arrange(Year, crimetype)
#    
#    fig1$Year <- as.character(fig1$Year)
    
#    min.dt <- min(fig1$total)
#    max.dt <- max(fig1$total)
    
#    p4 <- ggplot(aes(x=Year, y=total, group=crimetype, col=crimetype), data=fig1) +
#      geom_line()+
#      scale_y_continuous(breaks = seq(min.dt, max.dt, length.out = 10)) + 
#      #scale_x_discrete()
#      xlab("Year") +
#      ylab("Number of Cases") + 
#      ggtitle(paste("Lineplot of Number of Crimes by Year", state, sep="~")) +
#      theme_bw() +
#      theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 1),
#            legend.position = 'right') 
    
#    return.list <- list("lineplot" = p4)
#  }
  
  
  return(return.list)
  
}

aaa <- visual.fun(year = 2010, state = "ALABAMA")

aaa$Barplot.by.area
aaa$Barplot.by.type
aaa$spineplot

#aaa$lineplot
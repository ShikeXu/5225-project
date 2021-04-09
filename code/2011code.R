library(readxl)
library(dplyr)
library(zoo)

Data<-read_xls('/Users/mli0316/Desktop/UConn/stat 5225/group proj/raw data/2011.xls',skip = 3)
Data$State <- gsub('[0-9,]','',Data$State)
colnames(Data)<-gsub('[^A-Za-z]','',colnames(Data))

### rename incorrect colnames
colnames(Data)[c(3)] <- c("Actual")
#colnames(Data)[c(5)] <- c("Violent")
#colnames(Data)[c(6)] <- c("Murder and non-negligent man-slaughter")
#colnames(Data)[c(10)] <- c("Property Crime")

### remove the last column which is not useful
#Data <- Data[, -ncol(Data)]


# Check categories of column actual
table(Data$Actual)
table(Data$Area)

dt <- 
  Data %>%
  filter(!(Actual %in% c("Rate per 100,000 inhabitants"))) %>%     ## Remove "Rate per 100,000 inhabitants" in Column Actual
  filter(!(Area %in% c("Total", "State Total")))                   ## Remove rows containing 'Total' and 'State Total' in Column Area

## fill NA with previous value
dt$State <- na.locf(dt$State, fromLast = FALSE)
dt$Area <- na.locf(dt$Area, fromLast = FALSE)



dt1 <- 
  dt %>%
  filter(!is.na(Actual)) %>%
  mutate(
    Area1 = case_when(
      Area == "Metropolitan Statistical Area" ~ "M",
      Area == "Cities outside metropolitan areas" ~"O",
      Area == "Nonmetropolitan counties" ~ "R"
    ),
    Actual1 = ifelse(Actual == "Area actually reporting", "Y", "N")
  ) 

dt2 = dt1[,c(1,14,15,4:13)]
dt2$Population <- as.numeric(dt2$Population)
names(dt2) <- c('State','Area','Actual','Percentage','ViolentCrime','Murderandnonnegligentmanslaughter', 'Forciblerape',
                'Robbery','Aggravatedassault','PropertyCrime','Burglary','Larcenytheft','Motorvehicletheft')

Data2011_Part1 <- dt2

#### Part 2 ######

state_pop <- 
  Data %>%
  select(State, Area, Population)

state_pop$State <- na.locf(state_pop$State, fromLast = FALSE)
state_pop$Population <- as.numeric(state_pop$Population)

state_pop <- 
  state_pop %>%
  filter(Area %in% c('State Total', 'Total'))%>%
  select(-Area)

names(dt1) <- c('State','Area','Actual','Population','ViolentCrime',
                'Murderandnonnegligentmanslaughter', 'Forciblerape',
                'Robbery','Aggravatedassault','PropertyCrime',
                'Burglary','Larcenytheft','Motorvehicletheft','Area1','Actual1')


dt11 <- 
  dt2 %>%
  mutate(ViolentCrime1 = ViolentCrime/Percentage,
         Murderandnonnegligentmanslaughter1 = Murderandnonnegligentmanslaughter/Percentage,
         Forciblerape1 = Forciblerape/Percentage,
         Robbery1 = Robbery/Percentage,
         Aggravatedassault1 = Aggravatedassault/Percentage,
         PropertyCrime1 = PropertyCrime/Percentage,
         Burglary1 = Burglary/Percentage,
         Larcenytheft1 = Larcenytheft/Percentage,
         Motorvehicletheft1 = Motorvehicletheft/Percentage) %>%
  select(-ViolentCrime, -Murderandnonnegligentmanslaughter, -Forciblerape, -Robbery,
         -Aggravatedassault, -PropertyCrime, -Burglary, -Larcenytheft, -Motorvehicletheft)


dt3 <- 
  dt11 %>%
  select(-Area, -Percentage) %>%
  filter(Actual == 'Y') %>%
  group_by(State) %>%
  summarise(ViolentCrime2 = sum(ViolentCrime1),
            Murderandnonnegligentmanslaughter2 = sum(Murderandnonnegligentmanslaughter1),
            Forciblerape2 = sum(Forciblerape1),
            Robbery2 = sum(Robbery1),
            Aggravatedassault2 = sum(Aggravatedassault1),
            PropertyCrime2 = sum(PropertyCrime1),
            Burglary2 = sum(Burglary1),
            Larcenytheft2 = sum(Larcenytheft1),
            Motorvehicletheft2 = sum(Motorvehicletheft1)) %>%
  right_join(., state_pop, by='State') 


Data2011_Part2 <- 
  dt3 %>%
  mutate(
    ViolentCrime = ViolentCrime2/Population*100000,
    Murderandnonnegligentmanslaughter = Murderandnonnegligentmanslaughter2/Population*100000,
    Forciblerape = Forciblerape2/Population*100000,
    Robbery = Robbery2/Population*100000,
    Aggravatedassault = Aggravatedassault2/Population*100000,
    PropertyCrime = PropertyCrime2/Population*100000,
    Burglary = Burglary2/Population*100000,
    Larcenytheft = Larcenytheft2/Population*100000,
    Motorvehicletheft = Motorvehicletheft2/Population*100000
  ) %>%
  select(-Population, -ViolentCrime2, -Murderandnonnegligentmanslaughter2, 
         -Forciblerape2, -Robbery2, -Aggravatedassault2, -PropertyCrime2, 
         -Burglary2, -Larcenytheft2, -Motorvehicletheft2) %>%
  arrange(State)


write.csv(Data2011_Part1,"/Users/mli0316/Desktop/UConn/stat 5225/group proj/raw data/Data2011_Part1.csv", row.names = FALSE)
write.csv(Data2011_Part2,"/Users/mli0316/Desktop/UConn/stat 5225/group proj/raw data/Data2011_Part2.csv", row.names = FALSE)

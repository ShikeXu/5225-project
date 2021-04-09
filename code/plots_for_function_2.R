#four states with large population
trend(state = c("CALIFORNIA", "TEXAS", "FLORIDA", "NEW YORK"), 
      start_year = 2000, end_year = 2019, 
      crime = "ViolentCrime", by_state = FALSE)

#four states with small population
trend(state = c("ALASKA", "VERMONT", "WYOMING", "RHODE ISLAND"), 
      start_year = 2000, end_year = 2019, 
      crime = "ViolentCrime", by_state = FALSE)
#district of columbia has no data on 2014

#four states in the east part
trend(state = c("WASHINGTON", "OREGON", "CALIFORNIA", "NEVADA"),
      start_year = 2000, end_year = 2019, 
      crime = "ViolentCrime", by_state = FALSE)

#four states in the middle part
trend(state = c("WYOMING", "NEBRASKA", "COLORADO", "KANSAS"),
      start_year = 2000, end_year = 2019, 
      crime = "ViolentCrime", by_state = FALSE)

#four states in the south part
trend(state = c("ARIZONA", "NEW MEXICO", "TEXAS", "LOUISIANA"),
      start_year = 2000, end_year = 2019, 
      crime = "ViolentCrime", by_state = FALSE)

#four states in the north part
trend(state = c("MONTANA", "NORTH DAKOTA", "MINNESOTA", "MICHIGAN"),
      start_year = 2000, end_year = 2019, 
      crime = "ViolentCrime", by_state = FALSE)
#Maine has no data on 2013, Minnesota has no data from 2006 to 2012

#four states in the west part
trend(state = c("CONNECTICUT", "NEW JERSEY", "MASSACHUSETTS", "MARYLAND"),
      start_year = 2000, end_year = 2019, 
      crime = "ViolentCrime", by_state = FALSE)


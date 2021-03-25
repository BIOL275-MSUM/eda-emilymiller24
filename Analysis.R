
library(tidyverse)
library(readxl)

Poverty <- read_excel("Data/Entire Population Living in Poverty .xlsx") %>% 
  filter(LocationType == "County") %>% 
  filter(DataFormat == "Percent")

Unemprate1 <- read_excel("Data/Ann_UnempRate_NSA (1).xlsx") # first 50 counties
Unemprate2 <- read_excel("Data/Ann_UnempRate_NSA (2).xlsx") # second batch

unemployment <-
  Unemprate1 %>% 
  full_join(Unemprate2, by="Year/Month") %>% 
  pivot_longer(!`Year/Month`,names_to="county",values_to="unemployment") %>% 
  mutate(year=str_sub(`Year/Month`,start = 1, end = 4),
         year=as.integer(year)) %>% 
  select(year, county, unemployment) %>% #Ctrl + Shift + M to get piping
  print()



Poverty<- select(Poverty, TimeFrame, Location, Data) %>%   #1. Get rid of LocationType and Percent
  rename(year=TimeFrame, county=Location, poverty=Data) #2. Rename other columns to year, county and poverty


unemployment <- unemployment %>%
  mutate(county=str_remove_all(county," County")) #3. str_remove() county name and space from unemployment table


unemployment <- unemployment %>% 
  mutate(county=str_replace_all(county, "Saint Louis", "St. Louis"))


poverty <- Poverty %>% 
  mutate(year=as.integer(year),
         poverty=as.numeric(poverty)) #4. Convert year and data to numbers as.numeric() for data, integer for year




pe <- poverty %>% 
  full_join(unemployment, by=c("year",
            "county"))                 #5. Join poverty and unemployment data tables 
                                      #6. by = c("year", "county")





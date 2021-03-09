
library(tidyverse)
library(readxl)

Poverty <- read_excel("Data/Entire Population Living in Poverty .xlsx")

Unemprate1 <- read_excel("Data/Ann_UnempRate_NSA (1).xlsx")

Unemprate2 <- read_excel("Data/Ann_UnempRate_NSA (2).xlsx")

Poverty2 <- filter(Poverty, LocationType == "County")

Poverty3 <- filter(Poverty2,DataFormat == "Percent")




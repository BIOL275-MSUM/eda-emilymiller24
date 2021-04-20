
# Loading Packages --------------------------------------------------------


library(tidyverse)
library(readxl)
library(sf)
library(maps)


# Loading Data Sets -------------------------------------------------------


Poverty <- read_excel("Data/Entire Population Living in Poverty .xlsx") %>% 
  filter(LocationType == "County") %>% 
  filter(DataFormat == "Percent")

Unemprate1 <- read_excel("Data/Ann_UnempRate_NSA (1).xlsx") # first 50 counties
Unemprate2 <- read_excel("Data/Ann_UnempRate_NSA (2).xlsx") # second batch


# Combining Data Sets -----------------------------------------------------


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
  mutate(county=str_replace_all(county, "Saint Louis", "St Louis"))


Poverty <- mutate(Poverty, county=str_replace_all(county, "St. Louis", "St Louis"))


poverty <- Poverty %>% 
  mutate(year=as.integer(year),
         poverty=as.numeric(poverty)) #4. Convert year and data to numbers as.numeric() for data, integer for year


pe <- poverty %>% 
  full_join(unemployment, by=c("year",
            "county"))                 #5. Join poverty and unemployment data tables 
                                      #6. by = c("year", "county")


# Making Maps -------------------------------------------------------------


map1 <- map_data("county") %>% 
  as_tibble()


map2 <-filter(map1, region=="minnesota")


ggplot()+
  geom_polygon(data=map2, aes(x=long, y=lat, group=group),
               color="black", fill="lightblue")


merge1 <- rename(pe, subregion=county)


merge2 <- mutate(merge1, subregion=str_to_lower(subregion))


mergedcounties <- inner_join(map2, merge2, by= "subregion")


mergedcounties %>% 
  filter(year == 2000) %>% 
  ggplot() + 
  geom_polygon(
    mapping = aes(x=long, y=lat, group=group, fill = poverty), 
    color = "white", size = 0.2
  ) +
  coord_map("albers", lat0 = 43.5, lat1 = 49) +
  scale_fill_viridis_c(
    limits = c(0.03,0.3),
    direction = -1
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank()
  )


mergedcounties %>% 
  filter(year == 2012) %>% 
  ggplot() + 
  geom_polygon(
    mapping = aes(x=long, y=lat, group=group, fill = unemployment), 
    color = "white", size = 0.2
  ) +
  coord_map("albers", lat0 = 43.5, lat1 = 49) +
  scale_fill_viridis_c(
    limits = c(2,15),
    direction = -1
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank()
  )



# Correlation -------------------------------------------------------------

cor(
  mergedcounties$poverty,
  mergedcounties$unemployment
)


ggplot(mergedcounties,
       aes(x = poverty, y = unemployment, color = year)) +
  geom_point()

mergedcounties


<img src="https://www2.mnstate.edu/uploadedImages/Content/Marketing/logos/MSUM_Signature_Vert_Color.jpg" alt="MSUM logo" width="200" style="float:right">

# Correlation Between Poverty and Unemployment Rates by County in Minnesota

Emily Miller & Chris Merkord

Biosciences Department, Minnesota State University Moorhead, 1104 7th
Avenue South, Moorhead, MN 56563 USA

## Abstract

Poverty is an issue all around the world and I wanted to see what the
specifics were like in my home state. I wanted to see if some of the
causes for it had a major effect on it as well. I aimed to see if there
was any connection to the poverty and unemployment rates in Minnesota. I
examined how poverty is affected and thought that unemployment could be
a good example to use.

For my experiment, I will be using R Studio to look at the different
datasets I have and create graphs out of it. I got the population
poverty rates from the KIDS COUNT data center. I got the unemployment
rates from the Minnesota Employment and Economic Development site.

My hypothesis is that the poverty and unemployment rates have a
relationship and progress along with each other. I expect the rates for
both poverty and unemployment to correlate with each other during the
years. Meaning that if one goes down, I expect the other to follow. And
vice versa if one increases.

Poverty and unemployment are both issues that everyday people have to
deal with. I just aspired to see if these two issues somehow had a
connection with each other.

## Introduction

Poverty is a problem all around the globe. One that we should be
focusing on more to ensure happy and healthy people everywhere. Poverty,
to explain a little bit, is someone living in a society in where they do
not have the basic needs for life. Such as food, water or clothing.

There are about 40.6 million people just in the United states that are
living in poverty. There are many factors that could affect poverty, a
few being; access to food and water, having disabilities, lack of health
care and many other factors. But one that I wanted to compare to
poverty, was unemployment.

Unemployment is another problem that we as people have to deal with.
There are about 10 million unemployed people in the United States alone.
Unemployed people not only are losing income, but they face many other
challenges. Some so that lead to physical or mental problems.

Unemployment is a factor that wages in on poverty, and I wanted to
further look at the stats and see if there was a real correlation
between the two things. So I dug into some data for both in relation to
the state of Minnesota. I wanted to see what this looked like on a
smaller level, and how it affected one state aside from them all. I also
just wanted to see how it affected the state that I live in.

## Methods

### Data Acquisition

I’m going to start off by explaining where I found and obtained all of
my data from.

First off, I clicked on the KIDS COUNT (Annie E. Casey Foundation 2021)
website link from Dr. Merkord’s github list. The URL is included below:

<https://datacenter.kidscount.org/>

Then I clicked on the state of Minnesota to look for things specific to
the state. I found my data set under the Poverty tab, with the name as
Entire Population Living in Poverty.

After clicking on the data set, I clicked by county to show all of the
counties since my data set is for Minnesota counties. I wanted all of
the county data and not just the state.

I then clicked on raw data, which created an excel spreadsheet for me. I
then went into my folders and put the spreadsheet into my data folder
under my R studio project to save it onto R Studio. I was then able to
open it and process it from there.

Before we get into how I processed the data, I had another data set that
I used. But I got it from a different website. I looked this up myself,
searching unemployment rate mn dataset into google.

I then clicked on the second link that said State and National
Employment and Unemployment (DEED) 2021. The URL is listed below:

<https://mn.gov/deed/data/current-econ-highlights/state-national-employment.jsp>

This link brought me to the Employment and Economic Development section
of the Minnesota Government website.

I scrolled to the bottom and clicked on, ‘Select this link for more
data’ and it brought me to a place to select data.

I examined the list and clicked on Minnesota Counties, and then hit
submit. I was then brought to a list of Minnesota counties. I wasn’t
able to select all of them at the same time, so I held the Ctrl button
and I clicked on the first forty counties.

I then clicked on current statistics and it brought me to a list of the
counties with a lot of different columns. Such as a graph, the area,
employment, unemployment and the unemployment rate. In order to find the
data of all the counties selected, I had to click See Historical Data
for these areas at the bottom of the list.

Once I clicked on this, I was brought to the data set table that had the
list of all the counties I had clicked on with their unemployment rate
listed by month. I then set the years of which I needed, being from
2000-2012, and changed the frequency from monthly to annually. I then
clicked download data, where I then downloaded the excel file.

I did the same thing with the poverty data to bring it into R Studio
through the data folder. I did the whole process with getting the county
data the same way for the next forty seven counties I had left over.

### Data Preparation

To start analyzing the data, I used R version 4.0.3 (R Core Team 2020),
RStudio Version 1.3.1093 (RStudio Team 2020), and then the packages
Tidyverse (Wickham et al., 2019), and readxl (Hadley Wickham and
Jennifer Bryan 2019).

Before I could start analyzing the data, I had to put the data into
RStudio. To do that, I opened the file folder on my laptop and found the
excel files under downloads. I dragged the files into the folder for my
project, titled EDA. To which I then put into the data folder so that
all of my data was in one place.

Now I have three data files to use, one for poverty, and two for the
unemployment rate since I split them in half. To start it off, I took my
poverty data table and read it using the read\_excel function. I then
filtered out the LocationType column so that it only had results with
County in them. I did the same thing with the column DataFormat so that
it only had Percent.

I then loaded in the two unemployment rate data sets, naming them
Unemprate 1 and Unemprate 2. I took the two data sets and joined them so
they were on one data set, to which I named unemployment.

I changed the name of the column Year/Month so that it was just year and
turned that column into an integer. To get the columns I need for the
rest of my project, I used the select function and chose the year,
county and unemployment columns.

I then went back to the poverty data set and selected the TimeFrame,
Location and Data columns. To which I then renamed so they would match
the unemployment table. I changed TimeFrame to year, Location to county,
and Data to poverty.

I then took the unemployment data set and used the mutate function to
get rid of the space and the word county from the county column. So that
it didn’t say county after every single one. So that it would combine
with the poverty data set later on.

I also had to change the county Saint Louis, to St. Louis in the
unemployment data set so that it matched the poverty one.

After that was done, I went back to the poverty data set and used the
mutate function to change the year as an integer and make sure poverty
was a numeric property.

Now, all of the columns are labeled the same thing in both the poverty
and unemployment data sets. I combined the data sets and labeled the new
data set pe. Now all of my data is on one set and I’m ready to start
analyzing it.

## Results

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

The map above shows the unemployment rate of every county in the state
of Minnesota for the year 2000. So the colors of the counties correlate
with the legend to the side to show what the rate was for that county in
that year. This map is called Map 1.

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The map above shows the unemployment rate of every county in the state
of Minnesota for the year 2012. So the colors of the counties correlate
with the legend to the side to show what the rate was for that county in
that year. This map is called Map 2.

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The map above shows the poverty rate of every county in the state of
Minnesota for the year 2000. So the colors of the counties correlate
with the legend to the side to show what the rate was for that county in
that year. This map is called Map 3.

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The map above shows the poverty rate of every county in the state of
Minnesota for the year 2012. So the colors of the counties correlate
with the legend to the side to show what the rate was for that county in
that year. This map is called Map 4.

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The graph above shows the correlation between unemployment and poverty
rates in the state of Minnesota. Each dot represents a year per county
in Minnesota. So since there are 13 years and 87 counties in my data
set, there should be 1131 dots. The different colored dots show the
changes in years. This graph is called Map 5.

## Discussion

To start off my explanation of my results, I’ll make a note that map 1
and map 2 are of the poverty levels. Then map 2 and map 3 are comparing
unemployment rates. And finally map 5 is a correlation map between
poverty and unemployment. I’ll go into more details on all of them as I
discuss them.

Map 1 is a map of Minnesota’s counties with the unemployment rate during
the year 2000. So that means the colors shown on the map represent the
counties unemployment data for that year. The lighter colors in the
yellow and light greens show a lower unemployment rate for that specific
county. So the counties on that map in the lower part of Minnesota have
low unemployment rates, but those in Northern Minnesota have higher
ones.

Now map 2 is also showing the unemployment rates in the counties in
Minnesota, but this map was during the year 2012. This map shows that
the colors change significantly between the two maps. This one is darker
in color all around for the most part. The yellow coloring in the lower
half of the state are now green in color and in the north the colors are
a darker blue/green.

This helps with my hypothesis, because over the thirteen years that I
have represented in the maps, for the most part the unemployment rate
increased.

Maps 3 and 4 represent the poverty rates in Minnesota instead of the
unemployment rates. Map 3 is the year 2000 and it represents the poverty
rates per county of that year. This one has lighter colors like yellow
and greens just like the unemployment map for the same year.

Map 4 shows the poverty rates for the year 2012, so like the other maps,
thirteen years are between the two maps. The fourth map is also a lot
darker than it’s counterpart. With some dark blues in a few in the north
and some darker greens in the south. But overall the counties are all
just a little bit darker.

This also helps with my hypothesis by showing that the poverty rate for
the most part has increased.

The last map, map 5, shows how poverty and unemployment have correlated
throughout the years in all the counties. The darker colors indicate
further back years such as 2000 and 2001, but as the colors get lighter,
it means the years are getting later. Overall, the dots are bunched up,
but you can tell that there seems to be a sort of increase as both
numbers rise the other one does too.

This also sort of helps my hypothesis. I would say I would not reject my
hypothesis. Although I can’t tell directly if one affects the other, the
data that I had shows what my hypothesis was; that both correlate with
each other. The maps that I had showed this since they both increased
throughout the years.

## References

The Annie E. Casey Foundation, KIDS COUNT Data Center,
<https://datacenter.kidscount.org>.

Department of Employment and Economic Development (DEED) (2021). Monthly
Employment. State and National Employment and Unemployment.
<https://mn.gov/deed/data/current-econ-highlights/state-national-employment.jsp>
Accessed 2021-03-25

RStudio Team (2020). RStudio: Integrated Development Environment for R.
RStudio, PBC, Boston, MA URL <http://www.rstudio.com/>.

R Core Team (2020). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria. URL
<https://www.R-project.org/>.

Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source
Software, 4(43), 1686, <https://doi.org/10.21105/joss.01686>

Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel Files. R
package version 1.3.1. <https://CRAN.R-project.org/package=readxl>

#Exam 3 R

#clear the global environment 
rm(list = ls(all = TRUE))

#set the working directory
setwd("C:/Users/Divya Koothan/Desktop/GOV 355M/Exam_3")

#2
#library tidycensus 
library(tidycensus)
#install census api key
census_api_key("ad36f8ac89c353658437a6bcb4af62cbc5e6a8c7")

#getting ACS data 
df <- load_variables(year = 2015,
                      "acs5")
df2 <- df <- load_variables(year = 2010,
                            "acs5")

#getting the data for both years
 gini2015<- get_acs(geography = "state",
                 variables = c(estimate = c("B19083_001")),
                year = 2015)
 #creating year variable
 gini2015 <- gini2015 %>%
   mutate(year = 2015) %>%

#loading lable package     
library(data.table)
 #seting names for state 
setnames(gini2015, "NAME", "state")
 #getting 2010 data 
 gini2010<- get_acs(geography = "state",
                    variables = c(estimate = c("B19083_001")),
                    year = 2010)
 #creating year variable 
 gini2010 <- gini2010 %>%
   mutate(year = 2010)
 #seting names for state
setnames(gini2010, "NAME", "state")

 #appending the data frames
library(tidyverse)
inequality_panel <- bind_rows(gini2015, gini2010)
#peak at data
head(inequality_panel)

#3
#making the panel data wide to display years
inequality_wide <-
  inequality_panel %>%
  pivot_wider(id_cols = c("GEOID","state", "year"),
              names_from = "year", 
              values_from = "estimate", 
              names_prefix = "year_")
#peak at panel
head(inequality_wide)


#4
#pivot table so it is long again
inequality_long <- 
  inequality_wide %>%
  pivot_longer(cols = starts_with("year"),
               names_to = "year",
               names_prefix = "year_",
               values_to = "estimate",
               values_drop_na = FALSE) 
#pak at the panel
head(inequality_long)

#5
#check to see equal observation count, listed observations then columns
dim(inequality_long)
dim(inequality_panel)

#6
#collapsing the data
inequality_collapsed <-
  inequality_long %>%
  group_by(GEOID, state, year) %>%
  summarize(across(where(is.numeric), mean))

#7
#loading libraries 
library(ggplot2)
#figure out shapefile stuff

#8
#loading library
library(WDI)
#importing GDP data from WDI from 2006-2007
gdp_current <- WDI(country = "all",
                     indicator = "NY.GDP.MKTP.CD", #GDP deflator for 2015
                     start = 2006, end= 2007, 
                     extra = FALSE, cache = NULL) 
#change variable name
setnames(gdp_current, "NY.GDP.MKTP.CD", "GDP_current")

#9
#deflating to the base year 2015
deflator_data <- WDI(country = "all",
                     indicator = "NY.GDP.DEFL.ZS", #GDP deflator for 2015
                     start = 2001, end= 2017, 
                     extra = FALSE, cache = NULL) 

setnames(deflator_data, "NY.GDP.DEFL.ZS", "deflator")

#subset data fram to get only US $s
usd_deflator <- subset(deflator_data, country == "United States")
#drop unnecessary variables
usd_deflator$iso2c <- NULL
usd_deflator$country <- NULL

gdp_current = left_join(x = gdp_current,
                          y = usd_deflator,
                          by = "year")
#deflation and creating new variable
gdp_current$gdp_deflated <-
  gdp_current$GDP_current/
  (gdp_current$deflator/100)

#remove excess data table
rm(deflator_data)
#peak at table 
head(gdp_current)


#11
#loading libraries
## Pulling from PDF documents
library(pdftools) #allows to read pdf files locally and the web
library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)

#loading the text from the pdf
armeniatext <- pdf_text(pdf = "PA00TNMG.pdf")

#12
#converting into a data frame
armeniatext <-as.data.frame(armeniatext)

#13
#get stop words
data("stop_words")
#tokenize 
armeniatext <-
  armeniatext %>%
  unnest_tokens(word, armeniatext)
#removing stop words
armeniatext <-
  armeniatext %>%
  anti_join(stop_words)

#14
#finding top 5 most used words
armeniatext%>%
  count(word, sort = TRUE) 
head(armeniatext)





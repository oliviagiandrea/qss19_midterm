## Advanced Data Visualization (QSS 19) Spring 2023
## Project 1: Improving Existing Data Visualizations
##
## Name: Olivia Giandrea
## Date: May 12th, 2023

library(tidyverse)
library(readxl)
library(dplyr)

# read in the data from a csv file
wgmData <- read.csv(file = "C:/Users/olivi/Desktop/qss19/proj1/wgm2018.csv")
colnames(wgmData) <- c("country", "questionNum", "question", "response", "percent")

glimpse(wgmData)
head(wgmData)

# filter only the responses from the question we care about
# sum up the percentages that "strongly agree"/"agree" that vaccines are safe
wgmData <- wgmData %>%
  filter(questionNum == "Q25" & response %in% c("A lot", "Some")) %>%
  group_by(country) %>%
  summarize(percentage = sum(percent, na.rm = TRUE))

# categorize each country into a global region
# using the region_un field from our wiid data
wiid <- read_excel("C:/Users/olivi/Desktop/qss19/hw1/hw1_data/WIID_30JUN2022_0.xlsx")
wiid <- wiid %>%
  filter(wiid$country %in% wgmData$country) %>%
  select(country, region_un) %>%
  group_by(country) %>%
  distinct()

wiid

#take care of the countries that are named differently in the wiid dataset
extraMatches <- data.frame(country = c("Bosnia Herzegovina", "Congo, Rep.", 
                                       "Czech Republic", "Ivory Coast", 
                                       "Macedonia", "Northern Cyprus", 
                                       "Palestine", "South Korea", "The Gambia", 
                                       "UAE", "UK", "USA"), 
                           region = c("Europe", "Africa", "Europe", "Africa", 
                                      "Europe", "Asia", "Asia", "Asia", 
                                      "Africa", "Asia", "Europe", "Americas"))

# define a function that appropriately matches a country with its region
matchRegions <- function(df1, df2, df3) {
  # create a new empty column called "region" in df1
  df1$region <- NA
  
  # iterate over each row in df1
  for (i in 1:nrow(df1)) {
    # get the country name from the current row in df1
    country <- df1$country[i]
    
    # check if the country name is in df2
    if (country %in% df2$country) {
      # if it is, get the corresponding region value from df2 
      # and assign it to df1's "region" column
      df1$region[i] <- as.character(df2[df2$country == country, "region_un"])
    } else {
      # if it's not in df2, get the corresponding region value from df3 
      # and assign it to df1's "region" column
      df1$region[i] <- as.character(df3[df3$country == country, "region"])
    } 
  }
  return(df1)
}

# match countries and regions
wgmFinal <- matchRegions(wgmData, wiid, extraMatches)

# arrange by region and safe for plotting
wgmFinal <- wgmFinal %>% 
  arrange(region, desc(percentage)) %>%
  mutate(order = row_number()) 

# create a factor for plotting
wgmFinal$country <- factor(wgmFinal$country, levels = wgmFinal$country)

wgmFinal

# Calculate median percentage by region
wgmFinalMedian <- wgmFinal %>%
  group_by(region) %>%
  summarise(medianPercentage = median(percentage))

# Calculate ymin and ymax values for geom_segment by region
yminValues <- wgmFinal %>%
  group_by(region) %>%
  summarize(ymin = min(as.numeric(order))-0.2)

ymaxValues <- wgmFinal %>%
  group_by(region) %>%
  summarize(ymax = max(as.numeric(order))+0.2)

# Merge ymin and ymax values with median percentage by region
wgmFinalMedian <- wgmFinalMedian %>%
  left_join(yminValues, by = "region") %>%
  left_join(ymaxValues, by = "region")

wgmFinalMedian

# Plot graph with geom_segment adjusted by region
wgmPlot <- wgmFinal %>%
  ggplot(aes(x = percentage, y = country, fill = region)) + 
  geom_col() + 
  geom_segment(data = wgmFinalMedian, 
               aes(x = medianPercentage, y = ymin, xend = medianPercentage, yend = ymax),
               linetype = "solid", color = "black", size = 1.2) +
  theme_minimal() +
  labs(x = "Percentage of People who Agree/Strongly Agree that Vaccines are Safe", y = "Region", # changed y label to "Region"
       title = "Percent of People who Believe Vaccines are safe, by Country and Global Region",
       fill = "Region",
       size = 12,
       font = "Helvetica") +
  theme(axis.text.y = element_blank()) 
wgmPlot


# in future iterations of this graph, I want to: 
  # 1. flip the y axis so each region internally descends instead of ascends
  # 2. arrange regions by median descending (Asia, Americas, Africa, Europe, Oceania)
  # 3. add region labels to Y axis
  # 4. add each country's flag to hover info

# Using the plotly package to convert wgmPlot to an interactive plot
library(plotly)
ggplotly(wgmPlot, tooltip = c("percentage", "country")) 
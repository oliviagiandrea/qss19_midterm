## Advanced Data Visualization (QSS 19) Spring 2023
## Project 1: Improving Existing Data Visualizations
##
## Name: Olivia Giandrea
## Date: May 12th, 2023

library(tidyverse)
library(readxl)
library(dplyr)
library(plotly)
library(ggtext)

# read in the data from a csv file
wgmData <- read.csv(file = "C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj1/wgm2018.csv")
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
wiid <- read_excel("C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj1/wiid.xlsx")
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
wgmFinal <- wgmFinal %>% 
  arrange(region, desc(percentage))
wgmFinal

# Define the custom order of the regions
regionOrder <- c("Asia", "Americas", "Africa", "Europe")

# Create a new factor variable with custom levels for the region column
wgmFinal$region_factor <- factor(wgmFinal$region, levels = regionOrder)

# Order the dataframe by the custom order of the regions and percentage
wgmFinal <- wgmFinal %>% arrange(region_factor, desc(percentage))

# Remove the temporary region_factor variable
wgmFinal$region_factor <- NULL

# add order for plotting
wgmFinal <- wgmFinal %>%
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
ymaxValues

# Merge ymin and ymax values with median percentage by region
wgmFinalMedian <- wgmFinalMedian %>%
  left_join(yminValues, by = "region") %>%
  left_join(ymaxValues, by = "region")

wgmFinalMedian

# Define color palette for each region
colors <- c("Europe" = "#2E4052",
            "Africa" = "#BDD9BF",
            "Americas" = "#A997DF",
            "Asia" = "#FFC857")

# Plot graph with geom_segment adjusted by region with empty y-axis
wgmPlot <- wgmFinal %>%
  ggplot(aes(x = percentage, y = country, fill = region)) + 
  geom_col() + 
  geom_segment(data = wgmFinalMedian, 
               aes(x = medianPercentage, y = ymin, xend = medianPercentage, yend = ymax),
               linetype = "solid", color = "black", linewidth = 1.2, alpha = 0.5) +
  theme_minimal() +
  labs(x = "Percentage of People who Agree/Strongly Agree that Vaccines are Safe", y = "",
       title = "Percent of People who Believe Vaccines are safe, by Country and Global Region",
       fill = "Region",
       size = 12,
       font = "Helvetica") +
  scale_fill_manual(values = colors, limits = c("Europe", "Africa", "Americas", "Asia")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank())

# Use plotly to make graph interactive 
wgmPlot <- ggplotly(wgmPlot, tooltip = c("country", "percentage")) %>%
  layout(margin = list(t = 100, r = 50, b = 80, l = 100),
         legend = list(orientation = "h",
                       y = 1,
                       x = 0.5,
                       yanchor = "bottom",
                       xanchor = "center",
                       bgcolor = "white",
                       bordercolor = "black",
                       borderwidth = 1,
                       pad = list(10, 10),
                       margin = list(t = 10, b = 50)))

# manually add y-axis region labels
yLabels <- data.frame(region = c("Asia", "Americas", "Africa", "Europe"), 
                      yPos = c(20, 52, 82, 124))
for (i in seq_along(yLabels$region)) {
  wgmPlot <- wgmPlot %>% 
    layout(annotations = list(
      list(
        x = 0,
        y = yLabels$yPos[i],
        text = yLabels$region[i],
        showarrow = FALSE,
        xref = "paper",
        yref = "y",
        xanchor = "right",
        xshift = -5
      )
    ))
}

wgmPlot

# this is a good base, but it can be improved!
# next, we'll plot some "static" tooltips for a few targeted countries of 
# interest to try and encourage interaction with the plot

# function to add annotations to the graph
add_country_annotations <- function(plot, data, countries) {
  for (country in countries) {
    # Get tooltip data for current country
    countryData <- data[data$country == country, c("country", "percentage")]
    
    # Get y position of the current country row in the plot
    countryY <- which(data$country == country)
    
    # Add text annotation for current country tooltip
    plot <- plot %>%
      add_annotations(x = countryData$percentage, y = countryY, 
                      text = paste(countryData$country, ",", 
                                   countryData$percentage, "%"),
                      showarrow = TRUE,
                      arrowhead = 1,
                      arrowsize = 1.5,
                      arrowwidth = 1,
                      ax = 10,
                      ay = -20,
                      font = list(size = 10))
  }
  
  return(plot)
}

# annotate plot using function defined above
wgmPlot <- add_country_annotations(wgmPlot, wgmFinal, 
                                   c("France", "UK", "Norway",
                                     "Egypt", "USA", "Venezuela",
                                     "Japan", "China", "India"))

wgmPlot

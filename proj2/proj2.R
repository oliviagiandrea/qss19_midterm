## Advanced Data Visualization (QSS 19) Spring 2023
## Project 2: Exploring Multiple Aspects of a Single Dataset
##
## Name: Olivia Giandrea
## Date: May 12th, 2023

library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(shiny)

###############################################################################
##
## Preparing for Data Cleaning
##
###############################################################################


# creating a named vector of state abbreviations
stateAbbreviations <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR", 
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", 
  "Delaware" = "DE", "District of Columbia" = "DC", "Florida" = "FL", 
  "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", 
  "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS", "Kentucky" = "KY", 
  "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD", 
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", 
  "Mississippi" = "MS", "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", 
  "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ", 
  "New Mexico" = "NM", "New York" = "NY", "North Carolina" = "NC", 
  "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", 
  "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC", 
  "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", 
  "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", 
  "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY"
)


###############################################################################
##
## Groups Bar Chart
##
###############################################################################

# read in file data
groups <- read.csv(file = "C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj2/barGroups.csv")
glimpse(groups)

groups <- groups %>% 
  # convert the full state names to state abbreviations
  mutate(state = stateAbbreviations[groups$stateName]) %>%
  # convert all times in minutes to hours by dividing by 60
  mutate(work = round(groups$work / 60, 2)) %>%
  mutate(social = round(groups$social / 60, 2)) %>%
  mutate(sports = round(groups$sports / 60, 2)) %>%
  mutate(pcare = round(groups$pcare / 60, 2)) %>%
  mutate(educ = round(groups$educ / 60, 2)) %>%
  mutate(leisure = round(groups$leisure / 60, 2))
glimpse(groups)

# function to find frequencies for each group
frequency_by_hour <- function(df, category) {
  hours <- seq(0, 24, by = 1) # create sequence of hours
  df %>%
    mutate(hour = cut(!!sym(category), breaks = hours, include.lowest = TRUE)) %>% # set hour buckets for all 24 hours
    group_by(hour, sex) %>% # group by hour and sex
    summarise(percentage = (n() / 1346) * 100) %>% # count frequencies as percentages
    mutate(hour = str_extract(hour, "\\d+")) # change hour from (0, 1], (1, 2], ... to 0, 1, ...
}

# call fbh function to find freqencies by sex for work, educ, leisure, pcare
sexFrequencies_work <- frequency_by_hour(groups, "work")
sexFrequencies_educ <- frequency_by_hour(groups, "educ")
sexFrequencies_leisure <- frequency_by_hour(groups, "leisure")
sexFrequencies_pcare <- frequency_by_hour(groups, "pcare")

# check the frequency variables 
sexFrequencies_work
sexFrequencies_educ
sexFrequencies_leisure
sexFrequencies_pcare

# function to filter df, then plot bar charts for each frequency category
plot_groups_bar <- function(df, category) {
  ggplot(data = df, aes(x = as.numeric(hour), y = percentage, fill = sex)) + 
    geom_col(position = "dodge", width = 0.8) +
    scale_fill_manual(values = c("#0072B2", "#E69F00"), name = "Sex") +
    ggtitle(paste("Frequency of", category, "by Hour and Sex")) +
    xlab("Hour") +
    ylab("Frequency (%)") +
    theme_minimal() +
    ylim(0, 100)
} 

# plot each of the freq dfs by sex for work, educ, leisure, pcare
freqPlot_work <- plot_groups_bar(sexFrequencies_work, "Work")
freqPlot_educ <- plot_groups_bar(sexFrequencies_educ, "Education")
freqPlot_leisure <- plot_groups_bar(sexFrequencies_leisure, "Leisure")
freqPlot_pcare <- plot_groups_bar(sexFrequencies_pcare, "Personal Care")

# view each of the plots
freqPlot_work 
freqPlot_educ 
freqPlot_leisure 
freqPlot_pcare 

###############################################################################
##
## choropleth
##
###############################################################################


choropleth <- read.csv(file = "C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj2/choropleth.csv")
glimpse(choropleth)

choropleth <- choropleth %>% 
  # convert the full state names to state abbreviations
  mutate(state = stateAbbreviations[choropleth$stateName]) %>%
  # convert all times in minutes to hours by dividing by 60
  mutate(avg = round(choropleth$avg / 60, 2)) 

glimpse(choropleth)

# function to filter df, then plot a choropleth
plot_choropleth <- function(type) {
  choropleth %>% 
    filter(activity == type) %>%
    plot_ly(locations = ~state, 
        z = ~avg, 
        type = "choropleth", 
        locationmode = "USA-states") %>%
      layout(title = paste("Average", type, "Time (Hours) by State"),
           geo = list(scope = "usa", projection = list(type = "albers usa")))
} 

# plot each of the freq dfs by sex for work, educ, leisure, pcare
choropleth_work <- plot_choropleth("Work")
choropleth_educ <- plot_choropleth("Education")
choropleth_leisure <- plot_choropleth("Leisure")
choropleth_pcare <- plot_choropleth("Personal Care")

# view each of the plots
choropleth_work 
choropleth_educ 
choropleth_leisure 
choropleth_pcare 


###############################################################################
##
## diversity pie charts
##
###############################################################################

# load our required data from the respective csv file
pieData <- read.csv(file = "C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj2/differencesPie.csv")
glimpse(pieData)

# function to filter df, then plot a pie chart
plot_pie <- function(type) { 
  pieData %>% 
    filter(category == type) %>%
    plot_ly(labels = ~label, 
        values = ~percent, 
        type = "pie") %>%
    layout(title = paste(type, "Distribution"))
}

# plot pie charts for each of the 4 categories
pie_gender <- plot_pie("Gender")
pie_race <- plot_pie("Race")
pie_income <- plot_pie("Income")
pie_emp <- plot_pie("Employment")

# view each of the pie charts
pie_gender 
pie_race 
pie_income 
pie_emp 


###############################################################################
##
## diversity bar charts
##
###############################################################################

barData <- read.csv(file = "C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj2/differencesBar.csv")
glimpse(barData)

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
            "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", 
            "#9edae5", "#1b9e77", "#7570b3")

barData <- barData %>% 
  # convert all times in minutes to hours by dividing by 60
  mutate(time = round(as.integer(barData$time) / 60, 2))
glimpse(barData)

# function to filter df, then plot a bar chart
plot_bars <- function(type) { 
  barData %>% 
    filter(category == type) %>%
    ggplot(aes(x = bar, y = as.numeric(time), fill = label)) +
      geom_col(position = "dodge", width = 0.8) +
      scale_fill_manual(values = colors, name = "Label") +
      ggtitle(paste("Time Spent in Categories by", type)) +
      xlab("Activity") +
      ylab("Time (hours)") +
      theme_minimal() 
}

# plot pie charts for each of the 4 categories
bars_gender <- plot_bars("Gender")
bars_race <- plot_bars("Race")
bars_income <- plot_bars("Income")
bars_emp <- plot_bars("Employment")

# view each of the pie charts
bars_gender 
bars_race 
bars_income 
bars_emp 






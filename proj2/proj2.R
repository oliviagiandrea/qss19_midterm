## Advanced Data Visualization (QSS 19) Spring 2023
## Project 2: Exploring Multiple Aspects of a Single Dataset
##
## Name: Olivia Giandrea
## Date: May 14th, 2023

library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(shiny)
library(colorBlindness)


###############################################################################
##
## Data Cleaning
##
###############################################################################

# first, read in all the data I found at https://www.bls.gov/tus/data/datafiles-2021.htm
geoData <- read.table("C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj2/atuscps_2021.dat", header=TRUE, sep=",")
glimpse(geoData)
timeData <- read.table("C:/Users/olivi/Desktop/qss19/projects/proj1_2/proj2/atussum_2021.dat", header=TRUE, sep=",")
glimpse(timeData)

# select only respondent ID (TUCASEID) and state codes from geoData
# using FIPS guide from Appendix A in https://www.bls.gov/tus/dictionaries/atuscpscodebk21.pdf
geoData <- geoData %>%
  mutate(state = case_when(
    GESTFIPS == 1 ~ "AL",
    GESTFIPS == 2 ~ "AK",
    GESTFIPS == 4 ~ "AZ",
    GESTFIPS == 5 ~ "AR",
    GESTFIPS == 6 ~ "CA",
    GESTFIPS == 8 ~ "CO",
    GESTFIPS == 9 ~ "CT",
    GESTFIPS == 10 ~ "DE",
    GESTFIPS == 11 ~ "DC",
    GESTFIPS == 12 ~ "FL",
    GESTFIPS == 13 ~ "GA",
    GESTFIPS == 15 ~ "HI",
    GESTFIPS == 16 ~ "ID",
    GESTFIPS == 17 ~ "IL",
    GESTFIPS == 18 ~ "IN",
    GESTFIPS == 19 ~ "IA",
    GESTFIPS == 20 ~ "KS",
    GESTFIPS == 21 ~ "KY",
    GESTFIPS == 22 ~ "LA",
    GESTFIPS == 23 ~ "ME",
    GESTFIPS == 24 ~ "MD",
    GESTFIPS == 25 ~ "MA",
    GESTFIPS == 26 ~ "MI",
    GESTFIPS == 27 ~ "MN",
    GESTFIPS == 28 ~ "MS",
    GESTFIPS == 29 ~ "MO",
    GESTFIPS == 30 ~ "MT",
    GESTFIPS == 31 ~ "NE",
    GESTFIPS == 32 ~ "NV",
    GESTFIPS == 33 ~ "NH",
    GESTFIPS == 34 ~ "NJ",
    GESTFIPS == 35 ~ "NM",
    GESTFIPS == 36 ~ "NY",
    GESTFIPS == 37 ~ "NC",
    GESTFIPS == 38 ~ "ND",
    GESTFIPS == 39 ~ "OH",
    GESTFIPS == 40 ~ "OK",
    GESTFIPS == 41 ~ "OR",
    GESTFIPS == 42 ~ "PA",
    GESTFIPS == 44 ~ "RI",
    GESTFIPS == 45 ~ "SC",
    GESTFIPS == 46 ~ "SD",
    GESTFIPS == 47 ~ "TN",
    GESTFIPS == 48 ~ "TX",
    GESTFIPS == 49 ~ "UT",
    GESTFIPS == 50 ~ "VT",
    GESTFIPS == 51 ~ "VA",
    GESTFIPS == 53 ~ "WA",
    GESTFIPS == 54 ~ "WV",
    GESTFIPS == 55 ~ "WI",
    GESTFIPS == 56 ~ "WY",
    TRUE ~ NA_character_
  )) %>%
  select(TUCASEID, state)
glimpse(geoData)

# join geoData and timeData on TUCASEID to associate state codes with activities
rawData <- merge(geoData, timeData, by = "TUCASEID")
glimpse(rawData)

# Refer to lexicon (https://www.bls.gov/tus/lexicons/lexiconnoex2021.pdf) for
# naming conventions in df rows.

# The 2021 Activity Summary file contains data for the total number of minutes 
# that each respondent spent doing each 6-digit activity.
# Each record corresponds to a unique respondent, as indicated by a unique 
# value of the variable TUCASEID. 

# The variable names identify activities by their 6-digit codes (starting with "t").
# Digits 1 and 2 of the 6-digit code correspond to the variable tutier1code; 
# Digits 3 and 4 of the 6-digit code correspond to the variable tutier2code; 
# Digits 5 and 6 of the 6-digit code correspond to the variable tutier3code.
# For example, the variable "t040102" refers to the activity with 
# TUTIER1CODE=04, TUTIER2CODE=01, and TUTIER3CODE=02.  

# demographic categories (sex, race, income, etc.) were decoded using the 
# guides at https://www.bls.gov/tus/dictionaries/atusintcodebk21.pdf

# filter for various categories, then clean data: 
data <- rawData %>%
  # sum up all categories based on their lexicons, then convert minutes to hours
  mutate(work = round(rowSums(select(., starts_with("t05")), na.rm = TRUE) / 60, 2)) %>%
  mutate(educ = round(rowSums(select(., starts_with("t06")), na.rm = TRUE) / 60, 2)) %>%
  mutate(leisure = round(rowSums(select(., starts_with("t12")), na.rm = TRUE) / 60, 2)) %>%
  mutate(exercise = round(rowSums(select(., starts_with("t13")), na.rm = TRUE) / 60, 2)) %>%
  mutate(pcare = round(rowSums(select(., starts_with("t01")), na.rm = TRUE) / 60, 2)) %>%
  # convert sex boolean to strings
  mutate(TESEX = ifelse(TESEX == 1, "Male", "Female")) %>%
  # convert race from ints to strings
  mutate(PTDTRACE = case_when(
    PTDTRACE == 1 ~ "White Only",
    PTDTRACE == 2 ~ "Black Only",
    PTDTRACE == 4 ~ "Asian Only",
    PTDTRACE == 3 | PTDTRACE == 5 ~ "Other",
    TRUE ~ "Mixed"
  )) %>%
  # convert employment status from ints to strings
  mutate(TELFS = case_when(
    TELFS == 1 | TELFS == 2 ~ "Employed",
    TELFS == 3 | TELFS == 4 ~ "Unemployed",
    TRUE ~ "Not in Labor Force"
  )) %>%
  # convert emp status from boolean to strings
  mutate(TRDPFTPT = ifelse(TRDPFTPT == 1, "Full Time", "Part Time")) %>% 
  # add implied decimal to weekly earnings, then convert to annual salary
  mutate(TRERNWA = ifelse(TRERNWA == -1, 0, 52*(TRERNWA/100))) %>%
  # rename appropriate columns
  rename(sex = TESEX, race = PTDTRACE, emp = TELFS, 
         ptft = TRDPFTPT, income = TRERNWA) %>%
  # select only columns with values we care about
  select(state, work, educ, leisure, exercise, pcare, sex, race, emp, ptft, income)

glimpse(data)

###############################################################################
##
## Preparing for Plotting
##
###############################################################################

colors <- c("#FFC857", "#A997DF", "#BDD9BF", "#929084", "#2E4052")

###############################################################################
##
## Groups Bar Chart
##
###############################################################################

# clean data for bar charts
groups <- data %>%
  select(state, sex, work, educ, leisure, exercise, pcare) %>%
  replace_na(list(work = 0, educ = 0, leisure = 0, exercise = 0, pcare = 0))
glimpse(groups)

# function to find frequencies for each group
frequency_by_hour <- function(df, category) {
  hours <- seq(0, 24, by = 1) # create sequence of hours
  df %>%
    mutate(hour = cut(!!sym(category), breaks = hours, include.lowest = TRUE)) %>% # set hour buckets for all 24 hours
    group_by(hour, sex) %>% # group by hour and sex
    summarise(percentage = (n() / 22546) * 100) %>% # count frequencies as percentages
    mutate(hour = str_extract(hour, "\\d+")) # change hour from (0, 1], (1, 2], ... to 0, 1, ...
}

# call fbh function to find freqencies by sex for work, educ, leisure, pcare
sexFrequencies_work <- frequency_by_hour(groups, "work")
sexFrequencies_educ <- frequency_by_hour(groups, "educ")
sexFrequencies_leisure <- frequency_by_hour(groups, "leisure")
sexFrequencies_exercise <- frequency_by_hour(groups, "exercise")
sexFrequencies_pcare <- frequency_by_hour(groups, "pcare")

# check the frequency variables 
sexFrequencies_work
sexFrequencies_educ
sexFrequencies_leisure
sexFrequencies_exercise
sexFrequencies_pcare

# function to filter df, then plot bar charts for each frequency category
plot_groups_bar <- function(df, category) {
  ggplot(data = df, aes(x = as.numeric(hour), y = percentage, fill = sex)) + 
    geom_col(position = "dodge", width = 0.8) +
    scale_fill_manual(values = c("#A997DF", "#FFC857"), name = "Sex") +
    ggtitle(paste("Frequency of", category, "Per Day by Hour and Sex")) +
    xlab("# Hours per Day") +
    ylab("Frequency (%)") +
    theme_minimal() +
    ylim(0, 51)
} 

# plot each of the freq dfs by sex for work, educ, leisure, pcare
freqPlot_work <- plot_groups_bar(sexFrequencies_work, "Work")
freqPlot_educ <- plot_groups_bar(sexFrequencies_educ, "Education")
freqPlot_leisure <- plot_groups_bar(sexFrequencies_leisure, "Leisure")
freqPlot_exercise <- plot_groups_bar(sexFrequencies_exercise, "Exercise")
freqPlot_pcare <- plot_groups_bar(sexFrequencies_pcare, "Personal Care")

# view each of the plots
freqPlot_work 
freqPlot_educ 
freqPlot_leisure 
freqPlot_exercise
freqPlot_pcare 

# check color accessibility
colorBlindness::cvdPlot(freqPlot_work)

###############################################################################
##
## choropleth
##
###############################################################################

# clean data for choropleth
choropleth <- data %>%
  select(state, work, educ, leisure, exercise, pcare) %>%
  pivot_longer(cols = -state, names_to = "activity", values_to = "time") %>%
  # Group by state and activity to calculate the average time spent
  group_by(state, activity) %>%
  summarise(avg = mean(time))

# Rename activity values
activity_map <- c("work" = "Work", "educ" = "Education", "leisure" = "Leisure", 
                  "exercise" = "Exercise", "pcare" = "Personal Care")
choropleth$activity <- activity_map[choropleth$activity]

# Define the color scale
choroplethColors <- list(
  list(0, "#2E4052"),
  list(0.4, "#A997DF"),
  list(0.8, "#BDD9BF"),
  list(1, "#FFC857")
)

# function to filter df, then plot a choropleth
plot_choropleth <- function(type) {
  choropleth %>% 
    filter(activity == type) %>%
    plot_ly(
      locations = ~state, 
      z = ~avg, 
      type = "choropleth", 
      locationmode = "USA-states",
      colorscale = choroplethColors
    ) %>%
    layout(
      title = paste("Average", type, "Time (Hours) Per Day by State"),
      geo = list(scope = "usa", projection = list(type = "albers usa"))
    )
}

# plot each of the freq dfs by sex for work, educ, leisure, pcare
choropleth_work <- plot_choropleth("Work")
choropleth_educ <- plot_choropleth("Education")
choropleth_leisure <- plot_choropleth("Leisure")
choropleth_exercise <- plot_choropleth("Exercise")
choropleth_pcare <- plot_choropleth("Personal Care")

# view each of the plots
choropleth_work 
choropleth_educ 
choropleth_leisure
choropleth_exercise
choropleth_pcare 

# note: cvdPlot doesn't work with choropleths, so I can't test accessibility
# here, but I know my palette is accessible from the other plot tests!


###############################################################################
##
## diversity pie charts
##
###############################################################################

# clean data for pie charts
pieData <- data.frame(category = c("Sex", "Sex",
                                   "Race", "Race", "Race", "Race", "Race",
                                   "Income", "Income", "Income", "Income", "Income", 
                                   "Employment", "Employment", "Employment", "Employment"
                                   ), 
                      label = c(
                        "Female", "Male",
                        "White Only", "Black Only", "Asian Only", "Mixed", "Other", 
                        "Below $25k", "$25k-$50k", "$50k-$75k", "$75k-$100k", "$100k+", 
                        "Full Time", "Part Time", "Unemployed", "Not in Labor Force"
                      ), 
                      percent = c(
                        mean(data$sex == "Female") * 100,
                        mean(data$sex == "Male") * 100,
                        mean(data$race == "White Only") * 100,
                        mean(data$race == "Black Only") * 100,
                        mean(data$race == "Asian Only") * 100,
                        mean(data$race == "Mixed") * 100,
                        mean(data$race == "Other") * 100,
                        mean(data$income < 25000) * 100,
                        mean(data$income >= 25000 & data$income < 50000) * 100,
                        mean(data$income >= 50000 & data$income < 75000) * 100,
                        mean(data$income >= 75000 & data$income < 100000) * 100,
                        mean(data$income >= 100000) * 100,
                        mean(data$emp == "Employed" & data$ptft == "Full Time") * 100,
                        mean(data$emp == "Employed" & data$ptft == "Part Time") * 100,
                        mean(data$emp == "Unemployed") * 100,
                        mean(data$emp == "Not in Labor Force") * 100
                      ))

pieData <- pieData %>%
  mutate(percent = round(pieData$percent, 2))
pieData

# function to filter df, then plot a pie chart
plot_pie <- function(type) { 
  pieData %>% 
    filter(category == type) %>%
    plot_ly(labels = ~label, 
        values = ~percent, 
        type = "pie",
        marker = list(colors = colors)) %>%
    layout(title = paste(type, "Distribution in Survey Responders"))
}

# plot pie charts for each of the 4 categories
pie_sex <- plot_pie("Sex")
pie_race <- plot_pie("Race")
pie_income <- plot_pie("Income")
pie_emp <- plot_pie("Employment")

# view each of the pie charts
pie_sex 
pie_race 
pie_income 
pie_emp 

# note: cvdPlot doesn't work with pie charts so I can't test accessibility
# here, but I know my palette is accessible from the other plot tests!


###############################################################################
##
## diversity bar charts
##
###############################################################################

# clean data for the bars charts by each category, then merge into a single df

# cleaning for sex category
sexData <- data %>% 
  gather(key = "bar", value = "time", -state, -sex, -race, -emp, -ptft, -income) %>%
  group_by(sex, bar) %>%
  summarise(time = mean(time)) %>%
  ungroup() %>%
  mutate(bar = case_when(
    bar == "educ" ~ "Education",
    bar == "exercise" ~ "Exercise",
    bar == "leisure" ~ "Leisure",
    bar == "pcare" ~ "Personal Care",
    TRUE ~ "Work"
  )) %>%
  rename(label = sex) %>%
  mutate(category = "Sex")
sexData

# cleaning for race category
raceData <- data %>% 
  gather(key = "bar", value = "time", -state, -sex, -race, -emp, -ptft, -income) %>%
  group_by(race, bar) %>%
  summarise(time = mean(time)) %>%
  ungroup() %>%
  mutate(bar = case_when(
    bar == "educ" ~ "Education",
    bar == "exercise" ~ "Exercise",
    bar == "leisure" ~ "Leisure",
    bar == "pcare" ~ "Personal Care",
    TRUE ~ "Work"
  )) %>%
  rename(label = race) %>%
  mutate(category = "Race")
raceData

# cleaning for income category
incomeData <- data %>% 
  mutate(income = case_when(
    income < 25000 ~ "Less than $25k",
    income >= 25000 & income < 50000 ~ "$25k-$50k",
    income >= 50000 & income < 75000 ~ "$50k-$75k",
    income >= 75000 & income < 100000 ~ "$75k-$100k",
    TRUE ~ "$100k+"
  )) %>%
  gather(key = "bar", value = "time", -state, -sex, -race, -emp, -ptft, -income) %>%
  group_by(income, bar) %>%
  summarise(time = mean(time)) %>%
  ungroup() %>%
  mutate(bar = case_when(
    bar == "educ" ~ "Education",
    bar == "exercise" ~ "Exercise",
    bar == "leisure" ~ "Leisure",
    bar == "pcare" ~ "Personal Care",
    TRUE ~ "Work"
  )) %>%
  rename(label = income) %>%
  mutate(category = "Income")
incomeData

# cleaning for employment category
empData <- data %>% 
  mutate(emp = case_when(
    emp == "Employed" & ptft == "Full Time" ~ "Full Time",
    emp == "Employed" & ptft == "Part Time" ~ "Part Time",
    emp == "Unemployed" ~ "Unemployed",
    TRUE ~ "Not in Labor Force"
  )) %>%
  gather(key = "bar", value = "time", -state, -sex, -race, -emp, -ptft, -income) %>%
  group_by(emp, bar) %>%
  summarise(time = mean(time)) %>%
  ungroup() %>%
  mutate(bar = case_when(
    bar == "educ" ~ "Education",
    bar == "exercise" ~ "Exercise",
    bar == "leisure" ~ "Leisure",
    bar == "pcare" ~ "Personal Care",
    TRUE ~ "Work"
  )) %>%
  rename(label = emp) %>%
  mutate(category = "Employment Status")
empData

# combine all 4 mini category dfs into one single categorized df
barData <- rbind(sexData, raceData, incomeData, empData)
glimpse(barData)

# function to filter df on given category parameter, then plot a bar chart
plot_bars <- function(type) { 
  barData %>% 
    filter(category == type) %>%
    ggplot(aes(x = bar, y = as.numeric(time), fill = label)) +
      geom_col(position = "dodge", width = 0.8) +
      scale_fill_manual(values = colors, name = "Label") +
      ggtitle(paste("Time Spent in Categories Per Day by", type)) +
      xlab("Activity") +
      ylab("Time (hours)") +
      theme_minimal() 
}

# plot bar charts for each of the 4 categories
bars_sex <- plot_bars("Sex")
bars_race <- plot_bars("Race")
bars_income <- plot_bars("Income")
bars_emp <- plot_bars("Employment Status")

# view each of the pie charts
bars_sex
bars_race 
bars_income 
bars_emp

cvdPlot(bars_income)






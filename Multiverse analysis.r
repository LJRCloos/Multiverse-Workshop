# Install necessary packages
install.packages(c("tidyverse", "dplyr", "stringr"))
library(tidyverse)
library(dplyr)
library(stringr)

# Define the path to the folder containing the RData files
data <- read.csv2("https://osf.io/t7g4f/download", sep = ",")   

# Rename columns
data <- data |> rename_with(~c("Stress_1", "Stress_2", "Anxiety_1", "Anxiety_2", "Depression_1", "Depression_2",  
                            "Fatigue", "Hunger",  "Loneliness", "Anger", "Social_Contact", 
                            "Social_Media", "Music", "Procrastination", "Ourdoors", "OccupiedCovid", 
                            "HealthCovid", "IsolationCovid"),
                            .cols = starts_with("Q"))

# Transform Duration column to numeric, Number ID, order columns
data <- data |> mutate(Duration = case_when(Duration == "Expired" ~ NA_character_,
                                            TRUE ~ Duration)) |>
                mutate(Duration = as.numeric(Duration)) |> 
                nest(.by = ID) |>
                mutate(ID = row_number()) |>
                unnest(cols = c(data)) |>
                relocate(c("ID", "Day", "time", "beepvar"))

# Compute index for Stress, Anxiety, and Depression
data <- data |> mutate(Stress = (Stress_1 + Stress_2) / 2,
                       Anxiety = (Anxiety_1 + Anxiety_2) / 2,
                       Depression = (Depression_1 + Depression_2) / 2) 
                
# Compute compliance per participant (e.g. number of duration is not NA)
data <- data |> group_by(ID) |> 
                mutate(Compliance = sum(!is.na(Duration))/56*100) |>
                ungroup()

## Make 24 Datasets
# Define the criteria
negative_affect_criteria <- list(
  "StDeAnFa" = function(df) df |> mutate(Negative_Affect = Stress + Anxiety + Depression + Fatigue),
  "Stress" = function(df) df |> mutate(Negative_Affect = Stress),
  "Depression" = function(df) df |> mutate(Negative_Affect = Depression),
  "Anxiety" = function(df) df |> mutate(Negative_Affect = Anxiety)
)

response_time_criteria <- list(
  "Keep All" = function(df) df,
  "Remove Duration > 2700" = function(df) df |> filter(Duration <= 2700 | is.na(Duration)),
  "Remove Duration > 1800" = function(df) df |> filter(Duration <= 1800 | is.na(Duration))
)

compliance_criteria <- list(
  "Keep All" = function(df) df,
  "Remove Compliance < 75" = function(df) df |> filter(Compliance >= 75)
)

# Create 24 datasets based on the criteria
datasets <- list()
dataset_index <- 1

for (na_criterion in negative_affect_criteria) {
  for (rt_criterion in response_time_criteria) {
    for (comp_criterion in compliance_criteria) {
      df <- data 
      df <- na_criterion(df)
      df <- rt_criterion(df)
      df <- comp_criterion(df)
      df <- df |> select(ID, Day, time, beepvar, Social_Media, Negative_Affect, Duration, Compliance)
      datasets[[dataset_index]] <- df
      dataset_index <- dataset_index + 1
    }
  }
}

# Print the first few rows of each dataset to verify
for (i in 1:length(datasets)) {
  cat("Dataset", i, "\n")
  print(head(datasets[[i]]))
  cat("\n")
}


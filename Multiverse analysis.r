# Install necessary packages
install.packages(c("tidyverse", "dplyr", "stringr"))
library(tidyverse)
library(dplyr)
library(stringr)

# Download the data from OSF 
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

# Define criteria levels
negative_affect_levels <- c("Composite", "Stress", "Depression", "Anxiety")
response_time_levels <- c(Inf, 2700, 1800)
compliance_levels <- c(0, 75)

# Create a list to store the datasets
datasets <- list()

# Generate datasets based on criteria
for (na_level in negative_affect_levels) {
  for (rt_level in response_time_levels) {
    for (comp_level in compliance_levels) {
      
      # Filter data based on response time
      filtered_data <- data |> filter(Duration <= rt_level)
      
      # Filter data based on compliance
      filtered_data <- filtered_data |> filter(Compliance >= comp_level)
      
      # Select columns to retain
      retained_columns <- c("ID", "Day", "time", "beepvar", "Duration", "Compliance", "Social_Media")
      
      # Compute negative affect based on the level
      if (na_level == "Composite") {
        filtered_data <- filtered_data |> mutate(Negative_Affect = (Stress + Anxiety + Depression + Fatigue) / 4)
      } else if (na_level == "Stress") {
        filtered_data <- filtered_data |> mutate(Negative_Affect = Stress)
      } else if (na_level == "Depression") {
        filtered_data <- filtered_data |> mutate(Negative_Affect = Depression)
      } else if (na_level == "Anxiety") {
        filtered_data <- filtered_data |> mutate(Negative_Affect = Anxiety)
      }
      
      # Retain necessary columns
      filtered_data <- filtered_data |> select(all_of(retained_columns), Negative_Affect)
      
      # Store the dataset in the list
      dataset_name <- paste("dataset", na_level, rt_level, comp_level, sep = "_")
      datasets[[dataset_name]] <- filtered_data
    }
  }
}

# mean, sd, min, max of Negative Affect for each dataset
for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  summary_stats <- dataset |> 
    summarise(mean = mean(Negative_Affect, na.rm = TRUE),
              sd = sd(Negative_Affect, na.rm = TRUE),
              min = min(Negative_Affect, na.rm = TRUE),
              max = max(Negative_Affect, na.rm = TRUE),
              n = length(unique(ID)),
              obsv = n())
  print(paste("Dataset:", dataset_name))
  print(summary_stats)
}

# On each dataset run the analyses: relationship between negative affect and social media use
library(lme4)
library(broom.mixed)
library(lmerTest)

# Save the results 
results <- expand_grid(dataset_name = names(datasets), coefficient = c("Intercept", "Social Media")) |> 
  mutate(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_) 

# Run the analysis on each dataset
for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  
  # Compute correlation between negative affect and social media use
  model <- lmer(Negative_Affect ~ Social_Media + (1 + Social_Media|ID), 
                REML=FALSE, lmerControl(optCtrl=list(maxit=100L)),
                data = dataset)
  
  # if model doesnt converge print error message
    if (is.null(model@optinfo$conv$lme4$warnings)) {
        print(paste("Model converged for", dataset_name))
        # Extract the fixed effects results
        fixed_effects <- tidy(model, effects = "fixed", conf.int = TRUE)
        
        # Update the results dataframe
        results <- results |> 
          mutate(estimate = ifelse(dataset_name == !!dataset_name & coefficient == "Intercept", fixed_effects$estimate[1], estimate),
             std.error = ifelse(dataset_name == !!dataset_name & coefficient == "Intercept", fixed_effects$std.error[1], std.error),
             p.value = ifelse(dataset_name == !!dataset_name & coefficient == "Intercept", fixed_effects$p.value[1], p.value),
             conf.low = ifelse(dataset_name == !!dataset_name & coefficient == "Intercept", fixed_effects$conf.low[1], conf.low),
             conf.high = ifelse(dataset_name == !!dataset_name & coefficient == "Intercept", fixed_effects$conf.high[1], conf.high),
             estimate = ifelse(dataset_name == !!dataset_name & coefficient == "Social Media", fixed_effects$estimate[2], estimate),
             std.error = ifelse(dataset_name == !!dataset_name & coefficient == "Social Media", fixed_effects$std.error[2], std.error),
             p.value = ifelse(dataset_name == !!dataset_name & coefficient == "Social Media", fixed_effects$p.value[2], p.value),
             conf.low = ifelse(dataset_name == !!dataset_name & coefficient == "Social Media", fixed_effects$conf.low[2], conf.low),
             conf.high = ifelse(dataset_name == !!dataset_name & coefficient == "Social Media", fixed_effects$conf.high[2], conf.high))
    } else {
        print(paste("Model did not converge for", dataset_name))
    }
  }

  # plot the p-values across datasets for Social Media
    results |> 
        filter(coefficient == "Social Media") |>
        ggplot() +
        geom_histogram(aes(x = round(p.value, digits = 3)),fill = "white", color = "black") +
        geom_vline( xintercept = 0.05, color = "red", linetype = "dashed", size = 2) +
        theme_minimal() +
        labs(x = "p-value", y = "count") +
        theme(strip.placement = "outside",
                            axis.text = element_text(size = 18, color = "white"),
                            axis.text.y = element_text(size = 16, color = "white"),
                            axis.title.y = element_text(size = 20, color = "white"),
                            axis.title.x = element_text(size = 20, color = "white"),
                            #axis.ticks.y = element_blank(),  
                            strip.background = element_rect(fill=NA,colour=NA),
                            panel.spacing.x=unit(0.01,"cm"), 
                            strip.text.y = element_text(angle = 180, color = "white", face="bold", size=16), 
                            panel.spacing = unit(0.01, "lines")) 
ggsave("pvalues.png", 
       plot = last_plot(), 
       width = 10, 
       height = 6, 
       units = "in", 
       dpi = 300) 

spec.curve = results |> filter(coefficient == "Social Media") |>
                        mutate(negative_affect = rep(negative_affect_levels, each = 6),
                               response_time = rep(as.character(response_time_levels), each = 2, times = 4),
                               compliance = rep(as.character(compliance_levels), times = 12)
                               ) |> 
                        arrange(estimate) |> 
                        mutate(parameter = str_remove(dataset_name, "dataset_"),
                               dataset_number = row_number()) 

spec.curve |>   gather(parameter_name, parameter_option, negative_affect, response_time, compliance) |> 
                    select(parameter_name, parameter_option, dataset_number) |>
                    mutate(parameter_name = factor(str_replace(parameter_name, "_", "\n"))) |>
                    ggplot() +
                    geom_point(aes(x = dataset_number, y = parameter_option, color = parameter_name), 
                    size = 3 ) +
                    labs( x = "dataset #", y = "option included in the analysis specification") + 
                    facet_grid(parameter_name ~ ., space="free_y", scales="free_y", switch="y") +
                    theme_minimal(base_family = "Arial") +
                    theme(  strip.placement = "outside",
                            axis.text = element_text(size = 18, color = "white"),
                            axis.text.y = element_text(size = 16, color = "white"),
                            axis.title.y = element_text(size = 20, color = "white"),
                            axis.title.x = element_text(size = 20, color = "white"),
                            #axis.ticks.y = element_blank(),  
                            plot.title = element_text(size = 20, color = "white", face = "bold", hjust = 0.5),  # Center title #
                            plot.subtitle = element_text(size = 16, color = "white", hjust = 0.5),
                            strip.background = element_rect(fill=NA,colour=NA),
                            panel.spacing.x=unit(0.01,"cm"), 
                            strip.text.y = element_text(angle = 180, color = "white", face="bold", size=16), 
                            panel.spacing = unit(0.01, "lines"),
                            legend.position = "none") 

ggsave("specs.png", 
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300) 

spec.curve |>  ggplot() +
                    geom_point( aes(dataset_number , estimate, color = negative_affect), size = 4) +
                    labs(x = "Multiverse", y = "Coefficient of Social Media", color ="Negative Affect") +
                    theme_minimal() +
                    theme(strip.placement = "outside",
                            axis.text = element_text(size = 18, color = "white"),
                            axis.text.y = element_text(size = 16, color = "white"),
                            legend.title = element_text(color = "white"), 
                            legend.text = element_text(color = "white"), 
                            axis.title.y = element_text(size = 20, color = "white"),
                            axis.title.x = element_text(size = 20, color = "white"),
                            strip.background = element_rect(fill=NA,colour=NA),
                            panel.spacing.x=unit(0.01,"cm"), 
                            strip.text.y = element_text(angle = 180, color = "white", face="bold", size=16), 
                            panel.spacing = unit(0.01, "lines"),
                            legend.position = "top") 

ggsave("estimates.png", 
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300) 

results |> filter(coefficient == "Social Media") |> ggplot(aes(x=estimate)) +
                    geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
                    theme_minimal() +
                    labs(x="Coefficient of Social Media", y="count") +
                    theme(strip.placement = "outside",
                            axis.text = element_text(size = 18, color = "white"),
                            axis.text.y = element_text(size = 16, color = "white"),
                            axis.title.y = element_text(size = 20, color = "white"),
                            axis.title.x = element_text(size = 20, color = "white"),
                            axis.text.x = element_text(size = 15, color = "white"),  
                            strip.background = element_rect(fill=NA,colour=NA),
                            strip.text.y = element_text(angle = 180, color = "white", face="bold", size=16), 
                           ) 

ggsave("hist.png", 
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300) 

# order estimates by p-value and make table of dataframe names in descending order with estimate p-value and conf int
results |> filter(coefficient == "Social Media") |> mutate(dataset_name = str_remove(dataset_name, "dataset_")) |>
arrange(desc(estimate)) |>  mutate_if(is.numeric, round, 3) |>
select(dataset_name, estimate, p.value, conf.low, conf.high) 

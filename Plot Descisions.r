# Install necessary packages
install.packages(c("tidyverse", "ggalluvial", "vidiris", "ggplot2"))
library(tidyverse)
library(ggalluvial)
library(vidiris)
library(ggplot2)

# Create a data frame with all unique paths
decision_data <- expand.grid(
  Outliers = c("Remove", "Replace Mean", "Replace Median", "Keep"),
  Transformations = c("No Transform", "Log Transform", "Categorize"),
  MissingData = c("Impute", "Remove Cases")
)

# Add a count column for visualization purposes
decision_data$Count <- 1
decision_data |> 
  mutate(across(everything(), as.character))

ggplot(decision_data, 
       aes(axis1 = Outliers, axis2 = Transformations, axis3 = MissingData, y = Count, fill = Outliers)) +
  
  # Flow lines (all visible)
  geom_alluvium(aes(fill = Outliers), width = 0.2, alpha = 0.8) +  

  # Nodes (Strata) - First column solid, others transparent
  geom_stratum(aes(fill = Outliers), alpha = 0) +  

  # Labels with padding
  geom_label(stat = "stratum", aes(label=after_stat(stratum)), color = "white", 
           fill = "#0A0A26", fontface = "bold", size = 5) + 

  # Custom colors
  scale_fill_manual(values = c("#335B74", "#2683C6", "#3E8856", "#62A39F")) + 
  scale_x_discrete(limits = c("Outliers", "Transformations", "Missing Data"),expand = c(0, 0)) +

  # Titles
  labs(title = "Decision Tree for Data Transformation",
       subtitle = "Visualizing 24 Unique Paths",
       x = "Decision Steps") +

  # Theme customization
  theme_minimal(base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "#0A0A26", color = "#0A0A26"),  # Dark blue background # 
    plot.background = element_rect(fill = "#0A0A26", color = "#0A0A26"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 18, color = "white"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    plot.title = element_text(size = 20, color = "white", face = "bold", hjust = 0.5),  # Center title #
    plot.subtitle = element_text(size = 16, color = "white", hjust = 0.5),
    legend.position = "none"
  ) 

ggsave("decision_tree_plot.png", 
       plot = last_plot(), 
       width = 10, 
       height = 9, 
       units = "in", 
       dpi = 300) 

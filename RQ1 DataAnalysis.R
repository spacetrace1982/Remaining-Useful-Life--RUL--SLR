# This is the analysis for RQ1
# RQ1.	What Single ML algorithms have been commonly applied to the C-MAPSS dataset

# Load necessary libraries
library(dplyr)   # Data manipulation
library(ggplot2) # Plotting Graphs
library(readxl)  # Reading Excel files

# Read the data from the excel file from the data extraction phase. Manipulated in excel to extract the necessary columns. 
file_import_RQ1 <- "RQ1DataAnalysis.xlsx"
RQ1_data <- read_excel(file_import_RQ1)

# Check column names (used for reference to ensure correct names are used in script below )
colnames(RQ1_data)

# Exclude NA values and count occurrences for "General" ML methods to clean up the data. 
category_counts <- RQ1_data %>%
  filter(!is.na(General) & General != "NA") %>%
  group_by(General) %>%
  summarise(count = n())

# Create the bar plot for General ML methods
ggplot(category_counts, aes(x = General, y = count)) +
  geom_bar(stat = "identity", fill = "dark gray", color = "black", width = 0.7) +
  labs(
    x = "General broad ML Category",
    y = "Count"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),  # Move X-axis title down
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(size = 14)
  )

# Save the plot as png file to project folder
ggsave("GeneralMLMethods.png", width = 8, height = 6, dpi = 300)

# Exclude NA values and count occurrences for "Algorithm"
category_algorithm <- RQ1_data %>%
  filter(!is.na(Algorithm) & Algorithm != "NA") %>%
  group_by(Algorithm) %>%
  summarise(count = n())

# Create the bar plot for ML algorithms used based on deeper analysis of type
ggplot(category_algorithm, aes(x = Algorithm, y = count)) +
  geom_bar(stat = "identity", fill = "dark gray", color = "black", width = 0.7) +
  labs(
    x = "Algorithm Category",
    y = "Count"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),  # Move X-axis title down
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Save the plot as png file to project folder
ggsave("MLAlgorithms.png", width = 8, height = 6, dpi = 300)


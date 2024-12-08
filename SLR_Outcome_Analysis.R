# Install and Load Necessary Libraries. 
install.packages("ggplot2", dependencies = TRUE) #Graphing / Visuals
install.packages("readxl", dependencies = TRUE) #Reading Excel File
install.packages("dplyr", dependencies = TRUE) #Count function

library(ggplot2)
library(readxl)
library(dplyr)

# Imports the Dataset from root folder, ensure file name is correct and location of the file if there is an error.
file_import <- "Article Analysis.xlsx"
SLR_data <- read_excel(file_import)

# Preview the Data- views the headers for assistance in creating the visuals for reference. 
head(SLR_data)


# === Bar Chart for: Articles Per Year ===
yearly_count <- SLR_data %>% #Creates the counts for the categories ready for plotting.
  group_by(Year) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(yearly_count, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "dark gray", color = "black", width = 0.7) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +  # Add labels above bars
  labs(
    x = "Year of Publication",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 16) + 
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(size = 14) 
  )

ggsave("ArticlesPerYear.png", width = 8, height = 6, dpi = 300)


# === Bar Chart for: Articles Per Year Categorized by Hybrid/Non-Hybrid Method ===
# Exploratory- May not use in the the final report.

yearly_count_hybrid <- SLR_data %>%
  group_by(Year, Method) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(yearly_count_hybrid, aes(x = Year, y = Count, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = -0.5, size = 4) + 
  scale_fill_grey(start = 0.3, end = 0.7) +  # Monochrome color palette
  labs(
    x = "Year of Publication",
    y = "Number of Articles",
    fill = "Method"
  ) +
  theme_minimal(base_size = 16) +  # Larger base font size
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),  # Move X-axis title down
    axis.title.y = element_text(face = "bold"),
    legend.position = "top",  # Keep legend on top
    legend.title = element_text(face = "bold")
  )

ggsave("ArticlesPerYearByMethod.png", width = 8, height = 6, dpi = 300)

# === Bar Chart for: Publication Type ===
publication_count <- SLR_data %>% # Counting categories for plotting
  group_by(`Article Type`) %>%
  summarise(Count = n(), .groups = 'drop')

#creating the plot
ggplot(publication_count, aes(x = reorder(`Article Type`, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +  # Neutral gray fill
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +  # Add labels above bars
  labs(
    x = "Article Type",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 16) +  # Larger base font size
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),  # Move X-axis title down
    axis.title.y = element_text(face = "bold")
  )

ggsave("ArticlesType.png", width = 8, height = 6, dpi = 300)



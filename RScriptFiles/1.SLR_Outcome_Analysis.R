# Tracey Harty. Keele University. MSc Computer Science. 

# This is the analysis of the outcomes for the systematic literature review.

# Run to install the packages as needed.
install.packages("ggplot2", dependencies = TRUE)    # Graphing / Visuals
install.packages("readxl", dependencies = TRUE)     # Reading Excel File
install.packages("dplyr", dependencies = TRUE)      # Data manipulation
install.packages("viridis", dependencies = TRUE)    # Improved visual palette

# Load the necessary libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(viridis)


# Read the data
file_import <- "Article Analysis.xlsx"
SLR_data <- read_excel(file_import)

# === Bar Chart: Articles Per Year ===
yearly_count <- SLR_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(yearly_count, aes(x = factor(Year), y = Count)) +
  geom_bar(stat = "identity", fill = "#5b8fa8", color = "black", width = 0.7) +
  geom_text(aes(label = Count), vjust = -0.2, size = 5) +
  labs(x = "Year of Publication", y = "Number of Articles") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave("ArticlesPerYear.png", width = 8, height = 6, dpi = 300)

# === Bar Chart: Articles Per Year Categorized by Method ===
yearly_count_hybrid <- SLR_data %>%
  group_by(Year, Method) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(yearly_count_hybrid, aes(x = factor(Year), y = Count, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = -0.3, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Year of Publication", y = "Number of Articles", fill = "Method") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("ArticlesPerYearByMethod.png", width = 8, height = 6, dpi = 300)

# === Bar Chart: Article Type ===
publication_count <- SLR_data %>%
  group_by(`Article Type`) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(publication_count, aes(x = reorder(`Article Type`, -Count), y = Count, fill = `Article Type`)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(x = "Article Type", y = "Number of Articles") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave("ArticlesType.png", width = 8, height = 6, dpi = 300)

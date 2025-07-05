# Tracey Harty MSc Computer Science, Keele University
# This is the analysis for RQ1
# RQ1. What Single ML algorithms have been commonly applied to the C-MAPSS dataset

# Load libraries required for analysis
install.packages("ggplot2", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("viridis", dependencies = TRUE)  # For colour

library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)

# === Import data ===
file_import_RQ1 <- "RQ1DataAnalysis.xlsx"
RQ1_data <- read_excel(file_import_RQ1)

# === Plot 1: General ML Categories ===
category_counts <- RQ1_data %>%
  filter(!is.na(General) & General != "NA") %>%
  group_by(General) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

category_counts$General <- factor(category_counts$General, levels = category_counts$General)

ggplot(category_counts, aes(x = General, y = count, fill = General)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = count), vjust = -0.2, size = 5) +
  scale_fill_viridis_d(option = "D", guide = FALSE) +
  labs(
    x = "General ML Category",
    y = "Number of Articles"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave("GeneralMLMethods.png", width = 8, height = 6, dpi = 300)

# === Plot 2: Specific ML Algorithms ===
category_algorithm <- RQ1_data %>%
  filter(!is.na(Algorithm) & Algorithm != "NA") %>%
  group_by(Algorithm) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

category_algorithm$Algorithm <- factor(category_algorithm$Algorithm, levels = category_algorithm$Algorithm)

ggplot(category_algorithm, aes(x = Algorithm, y = count, fill = Algorithm)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = count), vjust = -0.2, size = 4.5) +
  scale_fill_viridis_d(option = "C", guide = FALSE) +
  labs(
    x = "Algorithm",
    y = "Number of Articles"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  )

ggsave("MLAlgorithms.png", width = 8, height = 6, dpi = 300)


# Tracey Harty. Keele University. MSc Computer Science. 

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(effsize)

# Load the data
df <- read_excel("Method_Compare_RMSE.xlsx", sheet = "RMSE_Analysis")

# Calculate descriptive statistics for each dataset and method
df_summary <- df %>%
  group_by(DATASET, Method) %>%
  summarise(
    Mean = mean(RMSE, na.rm = TRUE),
    Std_Dev = sd(RMSE, na.rm = TRUE),
    Median = median(RMSE, na.rm = TRUE),
    Min = min(RMSE, na.rm = TRUE),
    Max = max(RMSE, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  )

# Perform Mann-Whitney U test and calculate effect size for each dataset
mann_whitney_results <- df %>%
  group_by(DATASET) %>%
  summarise(
    Mann_Whitney_p = wilcox.test(
      RMSE[Method == "Hybrid"],
      RMSE[Method == "Non-Hybrid"],
      exact = FALSE
    )$p.value,
    Rank_Biserial = as.numeric(cliff.delta(
      RMSE[Method == "Hybrid"],
      RMSE[Method == "Non-Hybrid"]
    )$estimate),
    Rank_Biserial_Lower = as.numeric(cliff.delta(
      RMSE[Method == "Hybrid"],
      RMSE[Method == "Non-Hybrid"]
    )$conf.int[1]),
    Rank_Biserial_Upper = as.numeric(cliff.delta(
      RMSE[Method == "Hybrid"],
      RMSE[Method == "Non-Hybrid"]
    )$conf.int[2]),
    .groups = "drop"
  )

# Merge descriptive statistics and Mann-Whitney results into a single table
combined_results <- df_summary %>%
  left_join(mann_whitney_results, by = "DATASET")

# Export results to a CSV file
write.csv(combined_results, "Analysis_Results.csv", row.names = FALSE)

# Create a box-and-whisker plot
global_plot <- ggplot(df, aes(x = DATASET, y = RMSE, fill = Method)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, color = "black", alpha = 0.9) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black", size = 1.5) +
  labs(
    x = "Dataset",
    y = "RMSE",
    fill = "Method"
  ) +
  scale_fill_manual(values = c("Hybrid" = "#1f77b4", "Non-Hybrid" = "#ff7f0e")) +  # Blue & orange
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))  # Add headroom
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

print(global_plot)
# Save the plot as a PNG image
ggsave("RMSE_Comparison_Boxplot.png", plot = global_plot, width = 8, height = 6, dpi = 300)


# Tracey Harty. Keele University. MSc Computer Science. 

#R script RQ3: This script includes the statistical analysis for the SCORE outcomes 
#of the NASA datasets of Hybrid Vs Non-Hybrid methods. Descriptive Statistics, Mann-Whitney analysis. 


# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(effsize)# The library used for the Mann-Whitney Analysis

# Load the data from the SCORE analysis excel file. 
df <- read_excel("Method_Compare_SCORE.xlsx", sheet = "SCORE_Analysis")

# Calculate descriptive statistics for each dataset and method
df_summary <- df %>% 
  group_by(DATASET, Method) %>%
  summarise(
    Mean = mean(SCORE, na.rm = TRUE),
    Std_Dev = sd(SCORE, na.rm = TRUE),
    Median = median(SCORE, na.rm = TRUE),
    Min = min(SCORE, na.rm = TRUE),
    Max = max(SCORE, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  )

# Perform Mann-Whitney U test and calculate effect size for each dataset
mann_whitney_results <- df %>%
  group_by(DATASET) %>%
  summarise(
    Mann_Whitney_p = wilcox.test(
      SCORE[Method == "Hybrid"],
      SCORE[Method == "Non-Hybrid"],
      exact = FALSE
    )$p.value,
    Rank_Biserial = as.numeric(cliff.delta(
      SCORE[Method == "Hybrid"],
      SCORE[Method == "Non-Hybrid"]
    )$estimate),
    Rank_Biserial_Lower = as.numeric(cliff.delta(
      SCORE[Method == "Hybrid"],
      SCORE[Method == "Non-Hybrid"]
    )$conf.int[1]),
    Rank_Biserial_Upper = as.numeric(cliff.delta(
      SCORE[Method == "Hybrid"],
      SCORE[Method == "Non-Hybrid"]
    )$conf.int[2]),
    .groups = "drop"
  )

# Merge results
combined_results <- df_summary %>%
  left_join(mann_whitney_results, by = "DATASET")

# Export to CSV
write.csv(combined_results, "Analysis_Results_SCORE.csv", row.names = FALSE)

# Boxplot with formatting enhancements
global_plot <- ggplot(df, aes(x = DATASET, y = SCORE, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.15, alpha = 0.6, color = "black", size = 1.5) +
  scale_fill_manual(values = c("Hybrid" = "#0073C2FF", "Non-Hybrid" = "#EFC000FF")) +
  labs(
    x = "Dataset",
    y = "SCORE",
    fill = "Method"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Save and display
ggsave("SCORE_Comparison_Boxplot.png", plot = global_plot, width = 8, height = 6, dpi = 300)
print(global_plot)


# Tracey Harty. Keele University. MSc Computer Science. 

# R script RQx: This script includes the statistical analysis for the RMSE outcomes 
# of the NASA datasets comparing LSTM, CNN, and LSTM-CNN methods.
# Includes Descriptive Statistics and Mann-Whitney analysis.

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(effsize)


# Load the data from the Excel file
df <- read_excel("LSTM_CNN_Comparison.xlsx", sheet = 1)

# Clean and reshape data
df_long <- df %>%
  mutate(Method = trimws(Method)) %>%
  pivot_longer(
    cols = starts_with("RMSE"),
    names_to = "DATASET",
    values_to = "RMSE"
  ) %>%
  mutate(
    DATASET = gsub("RMSE ", "", DATASET),
    DATASET = trimws(DATASET)
  ) %>%
  filter(!is.na(RMSE))  # Removes rows with missing RMSE values

# Calculate descriptive statistics for each dataset and method
df_summary <- df_long %>% 
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

# Perform Mann-Whitney U test and calculate effect size for each dataset and each method pair
method_pairs <- combn(unique(df_long$Method), 2, simplify = FALSE)

mann_whitney_results <- df_long %>%
  group_by(DATASET) %>%
  group_modify(~ {
    results <- lapply(method_pairs, function(pair) {
      group1 <- .x$RMSE[.x$Method == pair[1]]
      group2 <- .x$RMSE[.x$Method == pair[2]]
      if (length(group1) > 0 && length(group2) > 0) {
        p_val <- wilcox.test(group1, group2, exact = FALSE)$p.value
        effect <- cliff.delta(group1, group2)
        data.frame(
          Comparison = paste(pair[1], "vs", pair[2]),
          Mann_Whitney_p = p_val,
          Rank_Biserial = as.numeric(effect$estimate),
          Rank_Biserial_Lower = as.numeric(effect$conf.int[1]),
          Rank_Biserial_Upper = as.numeric(effect$conf.int[2])
        )
      } else {
        NULL
      }
    })
    bind_rows(results)
  }) %>%
  ungroup()

# Merge statistics if desired (e.g., df_summary with test results), or keep separate
write.csv(df_summary, "Descriptive_Stats_CNN_RMSE.csv", row.names = FALSE)
write.csv(mann_whitney_results, "Mann_Whitney_Results_CNN_RMSE.csv", row.names = FALSE)

# Boxplot with formatting enhancements and facet borders
global_plot <- ggplot(df_long, aes(x = Method, y = RMSE, fill = Method)) +
  geom_boxplot(width = 0.6, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.15, alpha = 0.6, color = "black", size = 1.5) +
  facet_wrap(~ DATASET, ncol = 2) +
  scale_fill_manual(
    values = c("LSTM" = "#0073C2FF", "CNN" = "#EFC000FF", "LSTM-CNN" = "#868686FF"),
    drop = FALSE
  ) +
  labs(
    x = "Method",
    y = "RMSE"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.position = "none",  # ← removes the legend
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8)
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))


# Save and display plot
ggsave("RMSE_Comparison_CNN_Boxplot_Tiled.png", plot = global_plot, width = 12, height = 8, dpi = 300)
print(global_plot)


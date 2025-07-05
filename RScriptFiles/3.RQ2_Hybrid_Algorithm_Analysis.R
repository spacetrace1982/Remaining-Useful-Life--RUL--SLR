# Tracey Harty. Keele University. MSc Computer Science. 
# RQ2: Hybrid methodologies applied to C-MAPSS dataset

# Install necessary packages if needed
install.packages("readxl", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("tibble", dependencies = TRUE)
install.packages("viridis", dependencies = TRUE)

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(viridis)

# === Load and clean data ===
data <- read_excel("RQ2 Article Data Extraction.xlsx")
colnames(data) <- trimws(colnames(data))  # Clean headers

# === Plot 1: Count of Classifications ===
classification_counts <- data %>%
  count(Classification) %>%
  arrange(desc(n))

ggplot(classification_counts, aes(x = reorder(Classification, -n), y = n, fill = Classification)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.2, size = 5) +
  scale_fill_viridis_d(option = "D", guide = FALSE) +
  labs(x = "Classification", y = "Number of Articles") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave("HybridClassifications.png", width = 8, height = 6, dpi = 300)

# === Plot 2: Combined Methods Summary ===
combined_methods_summary <- data %>%
  count(`Number Combined Methods`) %>%
  arrange(desc(n))

ggplot(combined_methods_summary, aes(x = factor(`Number Combined Methods`), y = n, fill = factor(`Number Combined Methods`))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.2, size = 5) +
  scale_fill_viridis_d(option = "D", guide = FALSE) +
  labs(x = "Number of Methods Combined", y = "Number of Articles") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )

ggsave("CombinedMethodsCount.png", width = 8, height = 6, dpi = 300)

# === Plot 3: Heatmap of ML Category Co-occurrence ===
file_path <- "RQ2_ML_Data.xlsx"
data <- read_excel(file_path)

ml_categories <- data %>%
  select(`ML Category`) %>%
  filter(!is.na(`ML Category`)) %>%
  mutate(`ML Category` = trimws(`ML Category`)) %>%
  separate_rows(`ML Category`, sep = ",\\s*") %>%
  mutate(id = rep(1:(nrow(data)), times = sapply(strsplit(data$`ML Category`, ",\\s*"), length)))

co_occurrence <- ml_categories %>%
  group_by(id) %>%
  summarise(
    pairs = list(if (n() >= 2) combn(`ML Category`, 2, simplify = FALSE) else NULL),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  mutate(
    ML_Category_1 = sapply(pairs, `[`, 1),
    ML_Category_2 = sapply(pairs, `[`, 2)
  ) %>%
  select(-pairs) %>%
  count(ML_Category_1, ML_Category_2)

co_matrix <- co_occurrence %>%
  pivot_wider(names_from = ML_Category_2, values_from = n, values_fill = 0) %>%
  column_to_rownames("ML_Category_1")

all_categories <- union(rownames(co_matrix), colnames(co_matrix))
row_total <- rowSums(co_matrix[intersect(all_categories, rownames(co_matrix)), , drop = FALSE])
col_total <- colSums(co_matrix[, intersect(all_categories, colnames(co_matrix)), drop = FALSE])
total_freq <- row_total + col_total
ordered_categories <- names(sort(total_freq, decreasing = TRUE))
ordered_categories <- intersect(ordered_categories, rownames(co_matrix))
co_matrix <- co_matrix[ordered_categories, ordered_categories]

heatmap_data <- as.data.frame(as.table(as.matrix(co_matrix)))
colnames(heatmap_data) <- c("Category1", "Category2", "Freq")

heatmap_data <- heatmap_data %>%
  mutate(
    Category1 = factor(Category1, levels = ordered_categories),
    Category2 = factor(Category2, levels = ordered_categories)
  )

p <- ggplot(heatmap_data, aes(Category1, Category2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 3) +
  scale_fill_viridis(option = "H", name = "Co-occurrence") +
  labs(x = "ML Category", y = "ML Category") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10)
  )

print(p)

ggsave("ML_Category_Cooccurrence_Heatmap.png", plot = p, width = 10, height = 8, dpi = 300)


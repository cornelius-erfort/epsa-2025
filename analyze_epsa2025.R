# EPSA 2025 Analysis Script
# Analyze top affiliations and authors

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Read the data
df <- read.csv("epsa2025_abstracts.csv", stringsAsFactors = FALSE)

# Pivot longer for authors and their affiliations
df_long <- df %>%
  pivot_longer(
    cols = matches("^author\\d+$"),
    names_to = "author_num",
    values_to = "author"
  ) %>%
  pivot_longer(
    cols = matches("^author\\d+_affil$"),
    names_to = "affil_num",
    values_to = "author_affil"
  ) %>%
  # Keep only matching author/affil pairs
  filter(
    str_extract(author_num, "\\d+") == str_extract(affil_num, "\\d+")
  ) %>%
  # Remove empty authors
  filter(!is.na(author) & author != "") %>%
  select(-author_num, -affil_num)

# Clean up affiliations (remove extra whitespace, etc.)
df_long$author_affil <- str_trim(df_long$author_affil)
df_long$author_affil[df_long$author_affil == ""] <- NA

# Create Excel file with affiliations for manual correction
if (!require("writexl")) install.packages("writexl")
library(writexl)
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Get all affiliations (not just top 20) for manual correction
all_affiliations <- df_long %>%
  filter(!is.na(author_affil)) %>%
  count(author_affil, sort = TRUE) %>%
  rename(
    "Affiliation" = author_affil,
    "Frequency" = n
  ) %>%
  mutate(
    "Corrected_Affiliation" = "",  # Column for manual corrections
    "Notes" = ""                   # Column for notes
  )

# Save to Excel
write_xlsx(all_affiliations, "epsa2025_affiliations_for_correction.xlsx")

cat("\n=== EXCEL FILE CREATED ===\n")
cat("File: epsa2025_affiliations_for_correction.xlsx\n")
cat("Total unique affiliations:", nrow(all_affiliations), "\n")
cat("You can now manually add corrected affiliations in the 'Corrected_Affiliation' column.\n")

# Read corrected affiliations and apply corrections
corrected_file <- "epsa2025_affiliations_for_correction_coded.xlsx"
if (file.exists(corrected_file)) {
  cat("\n=== APPLYING CORRECTIONS ===\n")
  corrections <- read_xlsx(corrected_file)
  
  # Create a mapping from original to corrected affiliations
  correction_map <- corrections %>%
    filter(!is.na(Corrected_Affiliation) & Corrected_Affiliation != "") %>%
    select(Affiliation, Corrected_Affiliation)
  
  # Apply corrections to df_long
  df_long <- df_long %>%
    left_join(correction_map, by = c("author_affil" = "Affiliation")) %>%
    mutate(
      author_affil = ifelse(!is.na(Corrected_Affiliation), Corrected_Affiliation, author_affil)
    ) %>%
    select(-Corrected_Affiliation)
  
  cat("Applied", nrow(correction_map), "corrections to affiliations.\n")
} else {
  cat("\nNo corrected file found. Using original affiliations.\n")
}

# Top 20 Affiliations
top_affiliations <- df_long %>%
  filter(!is.na(author_affil)) %>%
  count(author_affil, sort = TRUE) %>%
  head(20) %>%
  mutate(author_affil = factor(author_affil, levels = rev(author_affil)))

# Top 20 Authors
top_authors <- df_long %>%
  count(author, sort = TRUE) %>%
  head(20) %>%
  mutate(author = factor(author, levels = rev(author)))

# Create visualizations
p1 <- ggplot(top_affiliations, aes(x = n, y = author_affil)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = n), hjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Top 20 Affiliations at EPSA 2025",
    subtitle = "Number of presentations by institution",
    x = "Number of Presentations",
    y = "Institution"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

p2 <- ggplot(top_authors, aes(x = n, y = author)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  geom_text(aes(label = n), hjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Top 20 Authors at EPSA 2025",
    subtitle = "Number of presentations by researcher",
    x = "Number of Presentations",
    y = "Author"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

# Print results
cat("=== TOP 20 AFFILIATIONS ===\n")
print(top_affiliations)

cat("\n=== TOP 20 AUTHORS ===\n")
print(top_authors)

# Save plots
ggsave("top_affiliations.png", p1, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("top_authors.png", p2, width = 12, height = 8, dpi = 300, bg = "white")

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total presentations:", nrow(df), "\n")
cat("Total unique authors:", n_distinct(df_long$author), "\n")
cat("Total unique affiliations:", n_distinct(df_long$author_affil[!is.na(df_long$author_affil)]), "\n")
cat("Average authors per presentation:", mean(sapply(1:nrow(df), function(i) {
  sum(!is.na(df[i, paste0("author", 1:10)]) & df[i, paste0("author", 1:10)] != "")
})), "\n") 
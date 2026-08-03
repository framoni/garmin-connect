# Cadence Analysis Script in R using ggplot2
# Analyzes average cadence over time filtered by segment incline,
# colored by 30-second pace (min/km) range bins.

user_lib <- path.expand("~/R/x86_64-pc-linux-gnu-library/4.5")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

library(ggplot2)
library(dplyr)
library(readr)

# --- Configuration Parameters ---
csv_file    <- "data/all_segments.csv"
output_img  <- "cadence_analysis.png"

# Incline range filter (in degrees)
min_incline <- -2.0
max_incline <-  2.0

# Pace bin configuration (30-second steps in min/km)
# min pace: 3.0 min/km (3:00), max pace: 8.0 min/km (8:00)
pace_step_sec <- 30
min_pace_min  <- 3.0
max_pace_min  <- 8.0

# --- Helper Functions ---
format_pace <- function(val_min) {
  mins <- floor(val_min)
  secs <- round((val_min - mins) * 60)
  sprintf("%d:%02d", mins, secs)
}

create_pace_bins <- function(pace_vec, min_p = 3.0, max_p = 8.0, step_sec = 30) {
  step_min <- step_sec / 60
  breaks <- seq(min_p, max_p, by = step_min)
  
  labels <- sapply(seq_len(length(breaks) - 1), function(i) {
    paste0(format_pace(breaks[i]), " - ", format_pace(breaks[i + 1]), " min/km")
  })
  
  cut(pace_vec, breaks = breaks, labels = labels, include.lowest = TRUE)
}

# --- Load Data ---
cat("Loading data from:", csv_file, "\n")
if (!file.exists(csv_file)) {
  stop("CSV file not found: ", csv_file)
}

df <- read_csv(csv_file, show_col_types = FALSE)

# Convert date column to POSIXct
df$date <- as.POSIXct(df$date)

# --- Filter Data ---
cat(sprintf("Filtering segments with incline between %.1f° and %.1f°...\n", min_incline, max_incline))
filtered_df <- df %>%
  filter(
    !is.na(incline_deg),
    incline_deg >= min_incline,
    incline_deg <= max_incline,
    avg_cadence > 0,
    avg_pace_min_km > 0
  )

cat(sprintf("Retained %d segments out of %d total.\n", nrow(filtered_df), nrow(df)))

if (nrow(filtered_df) == 0) {
  stop("No segments match the specified incline criteria.")
}

# Add pace range bins
filtered_df <- filtered_df %>%
  mutate(
    pace_bin = create_pace_bins(avg_pace_min_km, min_p = min_pace_min, max_p = max_pace_min, step_sec = pace_step_sec)
  ) %>%
  filter(!is.na(pace_bin))

# --- Generate ggplot Visualization ---
cat("Generating cadence plot...\n")

p <- ggplot(filtered_df, aes(x = date, y = avg_cadence, color = pace_bin)) +
  geom_point(alpha = 0.75, size = 2.5) +
  geom_smooth(aes(group = 1), method = "loess", color = "gray20", linetype = "dashed", se = FALSE, linewidth = 0.8) +
  scale_color_viridis_d(name = "Pace Range", option = "turbo") +
  labs(
    title = "Garmin Running Cadence vs. Date",
    subtitle = sprintf("Filtered for segment incline range: [%.1f°, %.1f°]", min_incline, max_incline),
    x = "Date of Activity",
    y = "Average Cadence (SPM)",
    caption = "Data source: data/all_segments.csv"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray30", size = 11, margin = margin(b = 10)),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Print plot to device if interactive
print(p)

# Save plot to PNG file
ggsave(output_img, plot = p, width = 11, height = 6.5, dpi = 300)
cat("Saved visualization to:", output_img, "\n")

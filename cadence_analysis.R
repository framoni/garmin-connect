# Cadence Analysis

# analyzes average cadence over time filtered by segment incline,
# split into 2x2 facets by fixed pace ranges, colored accordingly

user_lib <- path.expand("~/R/x86_64-pc-linux-gnu-library/4.5")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

library(ggplot2)
library(dplyr)
library(readr)

# configuration parameters
csv_file        <- "data/all_segments.csv"
output_img      <- "cadence_analysis.png"

# incline range filter (in degrees)
min_incline     <- -2.0
max_incline     <-  2.0

# vertical marker date
marker_date     <- as.POSIXct("2026-04-25", tz = "UTC")

# downsampling: max points per panel
max_pts_per_panel <- 200

# number of equidistant time bins for the cadence-average markers
n_time_bins <- 5

# fixed 4-panel pace ranges ---
# each entry: list(label, lo, hi)  (lo inclusive, hi exclusive except last)
pace_panels <- list(
  list(label = "Pace  5:30 – 5:00 min/km", lo = 5.0,  hi = 5.5),
  list(label = "Pace  5:00 – 4:30 min/km", lo = 4.5,  hi = 5.0),
  list(label = "Pace  4:30 – 4:00 min/km", lo = 4.0,  hi = 4.5),
  list(label = "Pace  < 4:00 min/km",       lo = 0.0,  hi = 4.0)
)

# ordered factor levels for correct 2x2 layout (row-major)
panel_levels <- sapply(pace_panels, `[[`, "label")

# load data
cat("Loading data from:", csv_file, "\n")
if (!file.exists(csv_file)) {
  stop("CSV file not found: ", csv_file)
}

df <- read_csv(csv_file, show_col_types = FALSE)
df$date <- as.POSIXct(df$date)

# filter: incline, valid values, year 2026 only ---
cat(sprintf("Filtering segments: incline [%.1f°, %.1f°], year 2026...\n", min_incline, max_incline))
filtered_df <- df %>%
  filter(
    !is.na(incline_deg),
    incline_deg >= min_incline,
    incline_deg <= max_incline,
    avg_cadence  > 0,
    avg_pace_min_km > 0,
    format(date, "%Y") == "2026"
  )

cat(sprintf("Retained %d segments out of %d total.\n", nrow(filtered_df), nrow(df)))

if (nrow(filtered_df) == 0) {
  stop("No segments match the criteria.")
}

# assign pace panel
assign_panel <- function(pace, panels) {
  lbl <- NA_character_
  for (p in panels) {
    if (pace >= p$lo && pace < p$hi) {
      lbl <- p$label
      break
    }
  }
  lbl
}

filtered_df <- filtered_df %>%
  rowwise() %>%
  mutate(pace_panel = assign_panel(avg_pace_min_km, pace_panels)) %>%
  ungroup() %>%
  filter(!is.na(pace_panel)) %>%
  mutate(pace_panel = factor(pace_panel, levels = panel_levels))

# downsample each panel ---
set.seed(42)
plot_df <- filtered_df %>%
  group_by(pace_panel) %>%
  slice_sample(n = max_pts_per_panel, replace = FALSE) %>%
  ungroup()

cat(sprintf("Downsampled to %d total points across 4 panels.\n", nrow(plot_df)))

# --- Build per-panel bin-average markers ---
# For each panel: fit LOESS on the full (non-downsampled) data, divide the
# date range into n_time_bins equal-width bins, compute the mean observed
# cadence per bin, then place the label at the LOESS-predicted y so it sits
# neatly on the trend curve.

build_bin_markers <- function(panel_data, n_bins) {
  if (nrow(panel_data) < 4) return(NULL)

  x_num  <- as.numeric(panel_data$date)
  y      <- panel_data$avg_cadence

  lo_fit <- loess(y ~ x_num, span = 0.75)

  date_min <- min(x_num)
  date_max <- max(x_num)
  breaks   <- seq(date_min, date_max, length.out = n_bins + 1)

  do.call(rbind, lapply(seq_len(n_bins), function(i) {
    in_bin     <- x_num >= breaks[i] & x_num < breaks[i + 1]
    if (sum(in_bin) < 2) return(NULL)
    mid_x      <- (breaks[i] + breaks[i + 1]) / 2
    mean_obs   <- mean(y[in_bin])
    fit_y      <- tryCatch(predict(lo_fit, newdata = data.frame(x_num = mid_x)),
                           error = function(e) mean_obs)
    data.frame(
      mid_date   = as.POSIXct(mid_x, origin = "1970-01-01"),
      mean_cad   = round(mean_obs),
      fit_cad    = fit_y
    )
  }))
}

markers_df <- filtered_df %>%
  group_by(pace_panel) %>%
  group_modify(~ build_bin_markers(.x, n_time_bins)) %>%
  ungroup()

cat(sprintf("Built %d cadence markers across 4 panels.\n", nrow(markers_df)))

# generate 2x2 faceted ggplot
cat("Generating 2x2 cadence plot...\n")

# palette: one color per panel (fast = warm, slow = cool)
panel_colors <- c(
  "Pace  5:30 – 5:00 min/km" = "#5B8FD4",
  "Pace  5:00 – 4:30 min/km" = "#48B37F",
  "Pace  4:30 – 4:00 min/km" = "#F4A62A",
  "Pace  < 4:00 min/km"       = "#E8443A"
)

p <- ggplot(plot_df, aes(x = date, y = avg_cadence, color = pace_panel)) +
  # vertical marker: April 25
  geom_vline(xintercept = marker_date,
             color = "#27AE60", linewidth = 0.9, linetype = "solid") +
  # scatter points (downsampled)
  geom_point(alpha = 0.55, size = 1.8) +
  # LOESS trend per panel
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9,
              linetype = "dashed", color = "gray25") +
  # bin-average markers: filled circle sitting ON the fit line
  geom_point(data = markers_df,
             aes(x = mid_date, y = fit_cad, color = pace_panel),
             size = 5, shape = 21, fill = "white", stroke = 1.5) +
  # bin-average cadence label (observed mean) inside the circle
  geom_text(data = markers_df,
            aes(x = mid_date, y = fit_cad, label = mean_cad),
            color = "gray15", size = 2.8, fontface = "bold") +
  # color scale
  scale_color_manual(values = panel_colors, guide = "none") +
  # split into 2 columns, 2 rows
  facet_wrap(~pace_panel, ncol = 2, scales = "free_y") +
  labs(
    title    = "Running Cadence vs. Date — 2026",
    subtitle = sprintf("Segments with incline in [%.1f°, %.1f°], split by pace range", min_incline, max_incline),
    x        = "Date",
    y        = "Average Cadence (SPM)",
    caption  = "Green line = April 25  •  Dashed = LOESS trend  •  Circled numbers = mean cadence per time bin  •  Data: all_segments.csv"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 15, margin = margin(b = 4)),
    plot.subtitle   = element_text(color = "gray35", size = 10, margin = margin(b = 10)),
    plot.caption    = element_text(color = "gray50", size = 8, hjust = 0),
    strip.text      = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.grid.minor = element_blank(),
    axis.title      = element_text(face = "bold", size = 10),
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 8)
  )

# save plot
ggsave(output_img, plot = p, width = 13, height = 8, dpi = 300)
cat("Saved visualization to:", output_img, "\n")

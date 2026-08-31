# Running Dynamics Analysis (Cadence, Vertical Oscillation, Right Foot Balance)

# Analyzes running metrics over time filtered by segment incline,
# split into 2x2 facets by fixed pace ranges, colored accordingly

user_lib <- path.expand("~/R/x86_64-pc-linux-gnu-library/4.5")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

library(ggplot2)
library(dplyr)
library(readr)

# configuration parameters
csv_file           <- "data/all_segments.csv"
cadence_output_img <- "cadence_analysis.png"
vo_output_img      <- "vertical_oscillation_analysis.png"
balance_output_img <- "right_balance_analysis.png"

# incline range filter (in degrees)
min_incline     <- -2.0
max_incline     <-  2.0

# vertical marker date
marker_date     <- as.POSIXct("2026-04-25", tz = "UTC")

# downsampling: max points per panel
max_pts_per_panel <- 200

# number of equidistant time bins for the average markers
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

# palette: one color per panel (fast = warm, slow = cool)
panel_colors <- c(
  "Pace  5:30 – 5:00 min/km" = "#5B8FD4",
  "Pace  5:00 – 4:30 min/km" = "#48B37F",
  "Pace  4:30 – 4:00 min/km" = "#F4A62A",
  "Pace  < 4:00 min/km"       = "#E8443A"
)

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

# --- Build per-panel bin-average markers ---
# For each panel: fit LOESS on the full (non-downsampled) data, divide the
# date range into n_time_bins equal-width bins, compute the mean observed
# metric per bin, then place the label at the LOESS-predicted y so it sits
# neatly on the trend curve.

build_bin_markers <- function(panel_data, metric_col, n_bins, format_fn) {
  if (nrow(panel_data) < 4) return(NULL)

  x_num  <- as.numeric(panel_data$date)
  y      <- panel_data[[metric_col]]

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
      label_val  = format_fn(mean_obs),
      fit_val    = fit_y
    )
  }))
}

# --- Generic faceted plot generator ---
generate_analysis_plot <- function(
  df,
  metric_col,
  y_label,
  title_text,
  caption_text,
  output_img,
  format_fn,
  valid_filter = function(x) !is.na(x) & x > 0
) {
  cat(sprintf("\n=== Generating plot for: %s ===\n", metric_col))

  # filter: incline, valid values, year 2026 only
  cat(sprintf("Filtering segments: incline [%.1f°, %.1f°], year 2026...\n", min_incline, max_incline))
  filtered_df <- df %>%
    filter(
      !is.na(incline_deg),
      incline_deg >= min_incline,
      incline_deg <= max_incline,
      valid_filter(.data[[metric_col]]),
      avg_pace_min_km > 0,
      format(date, "%Y") == "2026"
    )

  cat(sprintf("Retained %d segments out of %d total.\n", nrow(filtered_df), nrow(df)))

  if (nrow(filtered_df) == 0) {
    warning("No segments match the criteria for ", metric_col)
    return(invisible(NULL))
  }

  filtered_df <- filtered_df %>%
    rowwise() %>%
    mutate(pace_panel = assign_panel(avg_pace_min_km, pace_panels)) %>%
    ungroup() %>%
    filter(!is.na(pace_panel)) %>%
    mutate(pace_panel = factor(pace_panel, levels = panel_levels))

  # downsample each panel
  set.seed(42)
  plot_df <- filtered_df %>%
    group_by(pace_panel) %>%
    slice_sample(n = max_pts_per_panel, replace = FALSE) %>%
    ungroup()

  cat(sprintf("Downsampled to %d total points across panels.\n", nrow(plot_df)))

  # build bin markers
  markers_df <- filtered_df %>%
    group_by(pace_panel) %>%
    group_modify(~ build_bin_markers(.x, metric_col, n_time_bins, format_fn)) %>%
    ungroup()

  cat(sprintf("Built %d summary markers across panels.\n", nrow(markers_df)))

  p <- ggplot(plot_df, aes(x = date, y = .data[[metric_col]], color = pace_panel)) +
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
               aes(x = mid_date, y = fit_val, color = pace_panel),
               size = 5.5, shape = 21, fill = "white", stroke = 1.5) +
    # bin-average label (observed mean) inside the circle
    geom_text(data = markers_df,
              aes(x = mid_date, y = fit_val, label = label_val),
              color = "gray15", size = 2.6, fontface = "bold") +
    # color scale
    scale_color_manual(values = panel_colors, guide = "none") +
    # split into 2 columns, 2 rows
    facet_wrap(~pace_panel, ncol = 2, scales = "free_y") +
    labs(
      title    = title_text,
      subtitle = sprintf("Segments with incline in [%.1f°, %.1f°], split by pace range", min_incline, max_incline),
      x        = "Date",
      y        = y_label,
      caption  = caption_text
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 4)),
      plot.subtitle    = element_text(color = "gray35", size = 10, margin = margin(b = 10)),
      plot.caption     = element_text(color = "gray50", size = 8, hjust = 0),
      strip.text       = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(face = "bold", size = 10),
      axis.text.x      = element_text(angle = 30, hjust = 1, size = 8)
    )

  # save plot
  ggsave(output_img, plot = p, width = 13, height = 8, dpi = 300)
  cat("Saved visualization to:", output_img, "\n")
  invisible(p)
}

# --- Main execution ---
cat("Loading data from:", csv_file, "\n")
if (!file.exists(csv_file)) {
  stop("CSV file not found: ", csv_file)
}

df <- read_csv(csv_file, show_col_types = FALSE)
df$date <- as.POSIXct(df$date)

# 1. Cadence plot
generate_analysis_plot(
  df              = df,
  metric_col      = "avg_cadence",
  y_label         = "Average Cadence (SPM)",
  title_text      = "Running Cadence vs. Date — 2026",
  caption_text    = "Green line = April 25  •  Dashed = LOESS trend  •  Circled numbers = mean cadence per time bin  •  Data: all_segments.csv",
  output_img      = cadence_output_img,
  format_fn       = function(val) as.character(round(val)),
  valid_filter    = function(x) !is.na(x) & x > 0
)

# 2. Vertical Oscillation plot
if ("avg_vertical_oscillation" %in% names(df)) {
  generate_analysis_plot(
    df              = df,
    metric_col      = "avg_vertical_oscillation",
    y_label         = "Average Vertical Oscillation (cm)",
    title_text      = "Vertical Oscillation vs. Date — 2026",
    caption_text    = "Green line = April 25  •  Dashed = LOESS trend  •  Circled numbers = mean vertical oscillation (cm) per time bin  •  Data: all_segments.csv",
    output_img      = vo_output_img,
    format_fn       = function(val) sprintf("%.1f", val),
    valid_filter    = function(x) !is.na(x) & x > 0
  )
} else {
  cat("\nColumn 'avg_vertical_oscillation' not found in CSV. Skipping vertical oscillation plot.\n")
}

# 3. Right Foot Balance plot
if ("avg_ground_contact_balance_right" %in% names(df)) {
  generate_analysis_plot(
    df              = df,
    metric_col      = "avg_ground_contact_balance_right",
    y_label         = "Average Right Foot Balance (%)",
    title_text      = "Right Foot Balance vs. Date — 2026",
    caption_text    = "Green line = April 25  •  Dashed = LOESS trend  •  Circled numbers = mean right foot balance (%) per time bin  •  Data: all_segments.csv",
    output_img      = balance_output_img,
    format_fn       = function(val) sprintf("%.1f", val),
    valid_filter    = function(x) !is.na(x) & x > 0
  )
} else {
  cat("\nColumn 'avg_ground_contact_balance_right' not found in CSV. Skipping right foot balance plot.\n")
}


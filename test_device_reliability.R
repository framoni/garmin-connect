# Device Reliability Analysis: Chest Strap vs. Wrist-Based Running Dynamics
#
# Statistically tests whether wrist-based measurements introduce systematic bias
# and measurement distortion compared to chest strap measurements via:
# 1. Overall Welch's Two-Sample t-tests (Testing H0: mean_strap == mean_watch)
# 2. Variance Ratio tests (F-tests for equal variances)
# 3. Pace-stratified Welch's t-tests (Testing differences across speed panels)
# 4. Concurrent-window Welch's t-tests (Controlling for seasonal fitness improvements)
# 5. Diagnostic density and boxplot visualization (device_comparison_test.png)

user_lib <- path.expand("~/R/x86_64-pc-linux-gnu-library/4.5")
if (dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

library(readr)
library(dplyr)
library(ggplot2)

csv_file   <- "data/all_segments.csv"
output_img <- "device_comparison_test.png"

cat("Loading data from:", csv_file, "\n")
if (!file.exists(csv_file)) {
  stop("CSV file not found: ", csv_file)
}

df <- read_csv(csv_file, show_col_types = FALSE)
df$date <- as.POSIXct(df$date)

# Clean and tag device
df_clean <- df %>%
  filter(
    format(date, "%Y") == "2026",
    !is.na(incline_deg), incline_deg >= -2.0, incline_deg <= 2.0,
    !is.na(avg_pace_min_km), avg_pace_min_km > 0,
    !is.na(avg_vertical_oscillation), avg_vertical_oscillation > 0,
    !is.na(avg_vertical_ratio), avg_vertical_ratio > 0
  ) %>%
  mutate(
    device = ifelse(
      !is.na(avg_ground_contact_balance_right) & avg_ground_contact_balance_right > 0,
      "Chest Strap", "Wrist Only"
    )
  )

n_strap <- sum(df_clean$device == "Chest Strap")
n_watch <- sum(df_clean$device == "Wrist Only")

cat(sprintf("\nDataset summary (2026, incline [-2°, +2°]):\n"))
cat(sprintf("  Total segments: %d\n", nrow(df_clean)))
cat(sprintf("  Chest Strap:    %d (%.1f%%)\n", n_strap, (n_strap / nrow(df_clean)) * 100))
cat(sprintf("  Wrist Only:     %d (%.1f%%)\n", n_watch, (n_watch / nrow(df_clean)) * 100))

# ==============================================================================
# SECTION 1: Overall Welch's Two-Sample t-tests & Variance Ratio Tests
# ==============================================================================
cat("\n====================================================================\n")
cat("SECTION 1: Overall Welch's Two-Sample t-tests & Variance Ratio Tests\n")
cat("====================================================================\n")

run_tests <- function(metric_name, col_name, unit_str) {
  x_s <- df_clean[[col_name]][df_clean$device == "Chest Strap"]
  x_w <- df_clean[[col_name]][df_clean$device == "Wrist Only"]

  tt <- t.test(x_s, x_w)
  vt <- var.test(x_s, x_w)

  diff_val <- tt$estimate[1] - tt$estimate[2]
  pct_diff <- (diff_val / tt$estimate[2]) * 100

  cat(sprintf("\n--- %s (%s) ---\n", metric_name, unit_str))
  cat(sprintf("  Mean (Chest Strap):  %6.2f %s (SD = %.2f)\n", tt$estimate[1], unit_str, sd(x_s)))
  cat(sprintf("  Mean (Wrist Only):   %6.2f %s (SD = %.2f)\n", tt$estimate[2], unit_str, sd(x_w)))
  cat(sprintf("  Difference:          %+6.2f %s (%+.1f%%)\n", diff_val, unit_str, pct_diff))
  cat(sprintf("  95%% Confidence Int:  [%.2f, %.2f]\n", tt$conf.int[1], tt$conf.int[2]))
  cat(sprintf("  Welch t-statistic:   t = %.2f (df = %.1f)\n", tt$statistic, tt$parameter))
  cat(sprintf("  p-value (Means):     p = %.2e %s\n", tt$p.value, ifelse(tt$p.value < 0.001, "(*** Highly Significant)", "")))
  cat(sprintf("  Variance Ratio:      F = %.2f (p = %.2e)\n", vt$estimate, vt$p.value))
}

run_tests("Vertical Oscillation", "avg_vertical_oscillation", "cm")
run_tests("Vertical Ratio",        "avg_vertical_ratio",        "%")

# ==============================================================================
# SECTION 2: Pace-Stratified Welch's t-tests
# ==============================================================================
cat("\n====================================================================\n")
cat("SECTION 2: Pace-Stratified Welch's t-tests (Controlling for Speed)\n")
cat("====================================================================\n")

pace_panels <- list(
  list(label = "Pace 5:30 – 5:00 min/km", lo = 5.0, hi = 5.5),
  list(label = "Pace 5:00 – 4:30 min/km", lo = 4.5, hi = 5.0),
  list(label = "Pace 4:30 – 4:00 min/km", lo = 4.0, hi = 4.5),
  list(label = "Pace < 4:00 min/km",       lo = 0.0, hi = 4.0)
)

for (p in pace_panels) {
  sub <- df_clean %>% filter(avg_pace_min_km >= p$lo, avg_pace_min_km < p$hi)
  ns <- sum(sub$device == "Chest Strap")
  nw <- sum(sub$device == "Wrist Only")

  if (ns < 5 || nw < 5) next

  tt_vo <- t.test(avg_vertical_oscillation ~ device, data = sub)
  tt_vr <- t.test(avg_vertical_ratio ~ device, data = sub)

  cat(sprintf("\n[%s]  (Strap: n=%d, Wrist: n=%d)\n", p$label, ns, nw))
  cat(sprintf("  VO: Strap = %.2f cm vs. Wrist = %.2f cm  | Diff = %+.2f cm (t = %.2f, p = %.2e)\n",
              tt_vo$estimate[1], tt_vo$estimate[2], tt_vo$estimate[1] - tt_vo$estimate[2],
              tt_vo$statistic, tt_vo$p.value))
  cat(sprintf("  VR: Strap = %.2f%%  vs. Wrist = %.2f%%   | Diff = %+.2f%%  (t = %.2f, p = %.2e)\n",
              tt_vr$estimate[1], tt_vr$estimate[2], tt_vr$estimate[1] - tt_vr$estimate[2],
              tt_vr$statistic, tt_vr$p.value))
}

# ==============================================================================
# SECTION 3: Concurrent-Window Welch's t-test (Controlling for Seasonality)
# ==============================================================================
cat("\n====================================================================\n")
cat("SECTION 3: Concurrent-Window Welch's t-test (March–May 2026 Overlap)\n")
cat("====================================================================\n")

sub_overlap <- df_clean %>%
  filter(format(date, "%m") %in% c("03", "04", "05"))

ns_o <- sum(sub_overlap$device == "Chest Strap")
nw_o <- sum(sub_overlap$device == "Wrist Only")

tt_vo_o <- t.test(avg_vertical_oscillation ~ device, data = sub_overlap)
tt_vr_o <- t.test(avg_vertical_ratio ~ device, data = sub_overlap)

cat(sprintf("Segments recorded in same months (March–May 2026):\n"))
cat(sprintf("  Strap: n = %d, Wrist: n = %d\n", ns_o, nw_o))
cat(sprintf("  VO Difference: %+.2f cm (Strap = %.2f vs. Wrist = %.2f, t = %.2f, p = %.2e)\n",
            tt_vo_o$estimate[1] - tt_vo_o$estimate[2],
            tt_vo_o$estimate[1], tt_vo_o$estimate[2],
            tt_vo_o$statistic, tt_vo_o$p.value))
cat(sprintf("  VR Difference: %+.2f%%  (Strap = %.2f%% vs. Wrist = %.2f%%,  t = %.2f, p = %.2e)\n",
            tt_vr_o$estimate[1] - tt_vr_o$estimate[2],
            tt_vr_o$estimate[1], tt_vr_o$estimate[2],
            tt_vr_o$statistic, tt_vr_o$p.value))

# ==============================================================================
# SECTION 4: Diagnostic Visualization Plot
# ==============================================================================
cat("\nGenerating diagnostic visualization plot:", output_img, "...\n")

df_plot <- df_clean %>%
  rowwise() %>%
  mutate(pace_panel = {
    lbl <- NA_character_
    for (p in pace_panels) {
      if (avg_pace_min_km >= p$lo && avg_pace_min_km < p$hi) {
        lbl <- p$label
        break
      }
    }
    lbl
  }) %>%
  ungroup() %>%
  filter(!is.na(pace_panel)) %>%
  mutate(
    pace_panel = factor(pace_panel, levels = sapply(pace_panels, `[[`, "label")),
    device = factor(device, levels = c("Chest Strap", "Wrist Only"))
  )

p1 <- ggplot(df_plot, aes(x = avg_vertical_oscillation, fill = device, color = device)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  scale_fill_manual(values = c("Chest Strap" = "#27AE60", "Wrist Only" = "#E74C3C")) +
  scale_color_manual(values = c("Chest Strap" = "#27AE60", "Wrist Only" = "#E74C3C")) +
  labs(
    title = "Vertical Oscillation Distribution by Device",
    subtitle = "Chest Strap captures true variance (mean = 11.8 cm); Wrist is clamped (mean = 10.4 cm)",
    x = "Vertical Oscillation (cm)",
    y = "Density",
    fill = "Device", color = "Device"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

p2 <- ggplot(df_plot, aes(x = pace_panel, y = avg_vertical_ratio, fill = device)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.3, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("Chest Strap" = "#27AE60", "Wrist Only" = "#E74C3C")) +
  labs(
    title = "Vertical Ratio across Pace Ranges",
    subtitle = "Statistically significant baseline gap across all speeds (all p < 1e-45)",
    x = "Pace Range",
    y = "Vertical Ratio (%)",
    fill = "Device"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "top"
  )

png(output_img, width = 13, height = 6, units = "in", res = 300)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
print(p1, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()
cat("Diagnostic plot saved to:", output_img, "\n")

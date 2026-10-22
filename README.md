# Garmin Connect

Better analytics from Garmin activities

## Setup

You need a Garmin Connect account where your activities are stored. Create a  `.env ` file in the root project directory
with your credentials:

```
GARMIN_CONNECT_EMAIL=  
GARMIN_CONNECT_PASSWORD=
```

Then create a virtual environment managed by uv

`uv sync`

## Running Dynamics Analysis

`cadence_analysis.R` generates 2x2 faceted visualizations across pace ranges for segments filtered by incline:

- `cadence_analysis.png`: Study on how average running cadence evolves over time
- `vertical_oscillation_analysis.png`: Vertical oscillation (bounce in cm) trends over time
- `right_balance_analysis.png`: Ground contact balance percentage for the right foot over time
- `vertical_ratio_analysis.png`: Vertical ratio (bounce relative to stride length %) tracking running economy and forward efficiency over time

Set `strap_only <- TRUE` in [cadence_analysis.R](file:///home/framoni/Projects/garmin-connect/cadence_analysis.R) (or override it via R environment) to filter out runs recorded with only the watch and restrict the analysis strictly to runs recorded with an HR chest strap / RD pod.

### Device Reliability & Hypothesis Testing

`test_device_reliability.R` performs Welch's Two-Sample t-tests, variance ratio F-tests, and pace-stratified comparisons between Chest Strap and Wrist-Only recordings, generating `device_comparison_test.png`. Run with:

```bash
Rscript test_device_reliability.R
```
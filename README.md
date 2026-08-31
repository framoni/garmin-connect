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
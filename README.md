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

## Cadence analysis

`cadence_analysis.R` a study on if and how the average running cadence evolves over time as a function of average
incline and pace
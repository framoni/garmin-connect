import csv
import math
import os
from typing import Any

from garminconnect import Garmin

from pull_activities import email, get_runs, password


def parse_metrics(details: dict) -> list[dict]:
    """Parses detailed metrics from Garmin activity details JSON."""
    descriptors = details.get("metricDescriptors", [])
    metrics_data = details.get("activityDetailMetrics", [])

    # Map metric keys to their indices in the 'metrics' value list
    key_to_index = {d["key"]: d["metricsIndex"] for d in descriptors}

    parsed = []
    for entry in metrics_data:
        vals = entry.get("metrics", [])
        if not vals:
            continue

        point = {}
        for key, idx in key_to_index.items():
            if idx < len(vals):
                point[key] = vals[idx]

        # We need at least distance and elevation to define a segment
        if "sumDistance" in point and point["sumDistance"] is not None:
            parsed.append(point)
    return parsed


def extract_segments_from_activity(
    points: list[dict], segment_len: float, tolerance: float
):
    """
    Finds non-overlapping segments of a specific distance where the incline
    is almost constant.
    """
    segments = []
    n = len(points)
    i = 0

    # Heuristic for "almost constant": vertical deviation from a linear chord
    # max_dev relates the incline tolerance to allowed vertical wiggle.
    max_dev = tolerance * segment_len / 2

    while i < n:
        # Find the next point that completes the segment distance
        j = i + 1
        while (
            j < n and points[j]["sumDistance"] - points[i]["sumDistance"] < segment_len
        ):
            j += 1

        if j >= n:
            break

        d0, d1 = points[i]["sumDistance"], points[j]["sumDistance"]
        e0, e1 = points[i].get("directElevation"), points[j].get("directElevation")

        if e0 is None or e1 is None:
            i += 1
            continue

        actual_dist = d1 - d0
        # Skip if there's a significant data gap (e.g., >30% of segment length)
        if actual_dist > segment_len * 1.3:
            i += 1
            continue

        incline = (e1 - e0) / actual_dist

        # Check constancy by ensuring intermediate points don't deviate too far from the chord
        is_constant = True
        for k in range(i + 1, j):
            dk = points[k]["sumDistance"]
            ek = points[k].get("directElevation")
            if ek is None:
                continue

            # Expected elevation if incline was perfectly constant
            expected_ek = e0 + incline * (dk - d0)
            if abs(ek - expected_ek) > max_dev:
                is_constant = False
                break

        if is_constant:
            # Calculate averages for the segment
            seg_points = points[i : j + 1]
            cadences = []
            for p in seg_points:
                if p.get("directDoubleCadence") is not None:
                    cadences.append(p["directDoubleCadence"])
                elif p.get("directRunCadence") is not None:
                    cadences.append(p["directRunCadence"] * 2)
                elif p.get("directCadence") is not None:
                    cadences.append(p["directCadence"])

            hrs = [
                p["directHeartRate"]
                for p in seg_points
                if p.get("directHeartRate") is not None
            ]
            speeds = [
                p["directSpeed"]
                for p in seg_points
                if p.get("directSpeed") is not None and p["directSpeed"] > 0
            ]

            avg_cadence = round(sum(cadences) / len(cadences)) if cadences else 0
            avg_hr = round(sum(hrs) / len(hrs)) if hrs else 0
            avg_speed = sum(speeds) / len(speeds) if speeds else 0

            # Pace in min/km: (1000 / avg_speed) / 60
            avg_pace = (1000 / 60) / avg_speed if avg_speed > 0 else 0

            incline_deg = round(math.degrees(math.atan(incline)), 2)

            segments.append(
                {
                    "distance": round(actual_dist),
                    "incline": incline_deg,
                    "avg_cadence": avg_cadence,
                    "avg_hr": avg_hr,
                    "avg_pace": avg_pace,
                }
            )
            # Non-overlapping: jump to the end of the current segment
            i = j
        else:
            i += 1

    return segments


def main(
    activities_list: list[dict] | None = None,
    segment_dist: float = 100.0,
    tolerance: float = 0.005,
):
    """
    Main entry point to process activities and extract segments.
    Returns a list of tuples: (date, distance, incline, avg_cadence, avg_hr, avg_pace)
    """
    if not email or not password:
        print("Error: Garmin credentials not found. Check your .env file.")
        return []

    # If no list provided, fetch the most recent ones
    if activities_list is None:
        print("Fetching activities from Garmin Connect...")
        activities_list = get_runs(N=10)

    if not activities_list:
        print("No activities to process.")
        return []

    client = Garmin(email, password)
    print("Logging in to get detailed metrics...")
    client.login()

    all_segments = []
    for run in activities_list:
        activity_id = run.get("activityId")
        date = run.get("startTimeLocal")
        if not activity_id:
            continue

        print(f"Processing activity {activity_id} ({date})...")
        try:
            details = client.get_activity_details(activity_id)
            points = parse_metrics(details)
            segments = extract_segments_from_activity(points, segment_dist, tolerance)

            activity_segments = []
            for s in segments:
                res = (
                    date,
                    s["distance"],
                    s["incline"],
                    s["avg_cadence"],
                    s["avg_hr"],
                    s["avg_pace"],
                )
                all_segments.append(res)
                activity_segments.append(res)

            if activity_segments:
                save_to_csv(activity_segments, f"activity_{activity_id}_segments.csv")

            print(f"  Found {len(segments)} segments.")
        except Exception as e:
            print(f"  Error processing activity {activity_id}: {e}")

    if all_segments:
        save_to_csv(all_segments, "all_segments.csv")

    return all_segments


def save_to_csv(data: list[tuple], filename: str):
    """Saves the segments to a CSV file in the data/ folder."""
    data_dir = "data"
    if not os.path.exists(data_dir):
        os.makedirs(data_dir)

    filepath = os.path.join(data_dir, filename)
    header = [
        "date",
        "distance_m",
        "incline_deg",
        "avg_cadence",
        "avg_hr",
        "avg_pace_min_km",
    ]

    with open(filepath, mode="w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(header)
        writer.writerows(data)
    print(f"  Saved to {filepath}")


if __name__ == "__main__":
    # Parameters: 100m segments, 0.5% incline tolerance
    results = main(segment_dist=100.0, tolerance=0.005)

    if results:
        print("\n" + "=" * 85)
        print(
            f"{'Date':<20} | {'Dist':<7} | {'Incline':<8} | {'Cadence':<8} | {'HR':<6} | {'Pace (min/km)':<12}"
        )
        print("-" * 85)
        for r in results:
            date, dist, incl, cad, hr, pace = r
            print(
                f"{date[:19]:<20} | {dist:>6d}m | {incl:>6.2f}° | {cad:>8.1f} | {hr:>6d} | {pace:>12.2f}"
            )

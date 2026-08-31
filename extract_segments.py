import csv
import math
import os
from typing import Any

from garminconnect import Garmin

from pull_activities import email, get_runs, password

CSV_HEADER = [
    "date",
    "distance_m",
    "incline_deg",
    "avg_cadence",
    "avg_hr",
    "avg_pace_min_km",
    "avg_vertical_oscillation",
    "avg_ground_contact_balance_left",
    "avg_ground_contact_balance_right",
]


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
) -> list[dict[str, Any]]:
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

            vertical_oscs = [
                p["directVerticalOscillation"]
                if p.get("directVerticalOscillation") is not None
                else p["verticalOscillation"]
                for p in seg_points
                if p.get("directVerticalOscillation") is not None
                or p.get("verticalOscillation") is not None
            ]

            left_balances = []
            right_balances = []
            for p in seg_points:
                if p.get("directGroundContactBalanceLeft") is not None:
                    l_val = p["directGroundContactBalanceLeft"]
                    left_balances.append(l_val)
                    right_balances.append(100.0 - l_val)
                elif p.get("directGroundContactBalanceRight") is not None:
                    r_val = p["directGroundContactBalanceRight"]
                    right_balances.append(r_val)
                    left_balances.append(100.0 - r_val)
                elif p.get("directGroundContactBalance") is not None:
                    l_val = p["directGroundContactBalance"]
                    left_balances.append(l_val)
                    right_balances.append(100.0 - l_val)

            avg_cadence = round(sum(cadences) / len(cadences)) if cadences else 0
            avg_hr = round(sum(hrs) / len(hrs)) if hrs else 0
            avg_speed = sum(speeds) / len(speeds) if speeds else 0

            # Pace in min/km: (1000 / avg_speed) / 60
            avg_pace = (1000 / 60) / avg_speed if avg_speed > 0 else 0

            incline_deg = round(math.degrees(math.atan(incline)), 2)

            avg_vo = (
                round(sum(vertical_oscs) / len(vertical_oscs), 2)
                if vertical_oscs
                else None
            )
            avg_bal_l = (
                round(sum(left_balances) / len(left_balances), 2)
                if left_balances
                else None
            )
            avg_bal_r = (
                round(sum(right_balances) / len(right_balances), 2)
                if right_balances
                else None
            )

            segments.append(
                {
                    "distance": round(actual_dist),
                    "incline": incline_deg,
                    "avg_cadence": avg_cadence,
                    "avg_hr": avg_hr,
                    "avg_pace": avg_pace,
                    "avg_vertical_oscillation": avg_vo,
                    "avg_ground_contact_balance_left": avg_bal_l,
                    "avg_ground_contact_balance_right": avg_bal_r,
                }
            )
            # Non-overlapping: jump to the end of the current segment
            i = j
        else:
            i += 1

    return segments


def load_segments_from_csv(filename: str) -> list[tuple] | None:
    """Loads segments from an existing CSV file if it contains up-to-date columns."""
    data_dir = "data"
    filepath = os.path.join(data_dir, filename)
    if not os.path.exists(filepath):
        return None

    try:
        with open(filepath, mode="r", newline="") as f:
            reader = csv.reader(f)
            header = next(reader, None)
            if not header:
                return None

            required_cols = {
                "date",
                "distance_m",
                "incline_deg",
                "avg_cadence",
                "avg_hr",
                "avg_pace_min_km",
                "avg_vertical_oscillation",
                "avg_ground_contact_balance_left",
                "avg_ground_contact_balance_right",
            }
            if not required_cols.issubset(set(header)):
                return None

            col_indices = {col: idx for idx, col in enumerate(header)}
            rows = []
            for row in reader:
                if not row or len(row) < len(required_cols):
                    continue
                date = row[col_indices["date"]]
                dist = (
                    int(row[col_indices["distance_m"]])
                    if row[col_indices["distance_m"]]
                    else 0
                )
                incl = (
                    float(row[col_indices["incline_deg"]])
                    if row[col_indices["incline_deg"]]
                    else 0.0
                )
                cad = (
                    float(row[col_indices["avg_cadence"]])
                    if row[col_indices["avg_cadence"]]
                    else 0.0
                )
                hr = (
                    int(row[col_indices["avg_hr"]]) if row[col_indices["avg_hr"]] else 0
                )
                pace = (
                    float(row[col_indices["avg_pace_min_km"]])
                    if row[col_indices["avg_pace_min_km"]]
                    else 0.0
                )

                vo_raw = row[col_indices["avg_vertical_oscillation"]]
                vo = float(vo_raw) if vo_raw != "" else None

                bal_l_raw = row[col_indices["avg_ground_contact_balance_left"]]
                bal_l = float(bal_l_raw) if bal_l_raw != "" else None

                bal_r_raw = row[col_indices["avg_ground_contact_balance_right"]]
                bal_r = float(bal_r_raw) if bal_r_raw != "" else None

                rows.append((date, dist, incl, cad, hr, pace, vo, bal_l, bal_r))
            return rows
    except Exception as e:
        print(f"  Warning: failed to read cached {filepath}: {e}")
        return None


def save_to_csv(data: list[tuple], filename: str):
    """Saves the segments to a CSV file in the data/ folder."""
    data_dir = "data"
    if not os.path.exists(data_dir):
        os.makedirs(data_dir)

    filepath = os.path.join(data_dir, filename)

    with open(filepath, mode="w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(CSV_HEADER)
        for r in data:
            formatted_row = [
                r[0],
                r[1],
                r[2],
                r[3],
                r[4],
                r[5],
                r[6] if r[6] is not None else "",
                r[7] if r[7] is not None else "",
                r[8] if r[8] is not None else "",
            ]
            writer.writerow(formatted_row)
    print(f"  Saved to {filepath}")


def main(
    activities_list: list[dict] | None = None,
    segment_dist: float = 100.0,
    tolerance: float = 0.005,
    force_refresh: bool = False,
):
    """
    Main entry point to process activities and extract segments.
    Returns a list of tuples:
    (date, distance, incline, avg_cadence, avg_hr, avg_pace, avg_vertical_oscillation, avg_bal_l, avg_bal_r)
    """
    if not email or not password:
        print("Error: Garmin credentials not found. Check your .env file.")
        return []

    # If no list provided, fetch the most recent ones
    if activities_list is None:
        print("Fetching activities from Garmin Connect...")
        activities_list = get_runs(N=500)

    if not activities_list:
        print("No activities to process.")
        return []

    client: Garmin | None = None
    all_segments = []
    total_activities = len(activities_list)

    for idx, run in enumerate(activities_list, start=1):
        activity_id = run.get("activityId")
        date = run.get("startTimeLocal")
        if not activity_id:
            continue

        filename = f"activity_{activity_id}_segments.csv"

        if not force_refresh:
            cached_segments = load_segments_from_csv(filename)
            if cached_segments is not None:
                print(
                    f"[{idx}/{total_activities}] Activity {activity_id} ({date}) already processed ({len(cached_segments)} segments)."
                )
                all_segments.extend(cached_segments)
                continue

        print(f"[{idx}/{total_activities}] Pulling activity {activity_id} ({date})...")
        if client is None:
            client = Garmin(email, password)
            print("Logging in to get detailed metrics...")
            client.login()

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
                    s["avg_vertical_oscillation"],
                    s["avg_ground_contact_balance_left"],
                    s["avg_ground_contact_balance_right"],
                )
                all_segments.append(res)
                activity_segments.append(res)

            save_to_csv(activity_segments, filename)
            print(f"  Found {len(segments)} segments.")
        except Exception as e:
            print(f"  Error processing activity {activity_id}: {e}")

    if all_segments:
        save_to_csv(all_segments, "all_segments.csv")

    return all_segments


if __name__ == "__main__":
    # Parameters: 100m segments, 0.5% incline tolerance
    results = main(segment_dist=100.0, tolerance=0.005)

    if results:
        print("\n" + "=" * 125)
        print(
            f"{'Date':<20} | {'Dist':<7} | {'Incline':<8} | {'Cadence':<8} | {'HR':<6} | {'Pace (min/km)':<14} | {'Vert Osc':<9} | {'L Bal':<7} | {'R Bal':<7}"
        )
        print("-" * 125)
        for r in results[:20]:
            date, dist, incl, cad, hr, pace, vo, bal_l, bal_r = r
            vo_str = f"{vo:.2f} cm" if vo is not None else "N/A"
            bal_l_str = f"{bal_l:.1f}%" if bal_l is not None else "N/A"
            bal_r_str = f"{bal_r:.1f}%" if bal_r is not None else "N/A"
            print(
                f"{str(date)[:19]:<20} | {dist:>6d}m | {incl:>6.2f}° | {cad:>8.1f} | {hr:>6d} | {pace:>14.2f} | {vo_str:>9} | {bal_l_str:>7} | {bal_r_str:>7}"
            )

import os
from typing import Any

from dotenv import load_dotenv
from garminconnect import Garmin

load_dotenv()

email: str | None = os.getenv(key="GARMIN_CONNECT_EMAIL")
password: str | None = os.getenv(key="GARMIN_CONNECT_PASSWORD")


def get_runs(N: int = 500):
    """Retrieves the most recent N running activities."""
    try:
        client = Garmin(email, password)

        print("Logging in to Garmin Connect...")
        client.login()

        activities: dict[str, Any] | list[Any] = client.get_activities(start=0, limit=N)
        runs: list[dict | Any] = [
            a
            for a in activities
            if "running" in a["activityType"]["typeKey"]  # ty:ignore[invalid-argument-type]
        ]

        print(f"Found {len(runs)} running activities.")

        for run in runs:
            date: str = run["startTimeLocal"]
            name: str = run["activityName"]
            # distance is in meters by default
            distance_km: float = run["distance"] / 1000

            print(f"[{date}] {name} - {distance_km:.2f} km")

        return runs

    except Exception as e:
        print(f"An error occurred: {e}")


if __name__ == "__main__":
    run_data: list[dict | Any] | None = get_runs()

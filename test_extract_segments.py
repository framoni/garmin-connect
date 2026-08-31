import math
import unittest

from extract_segments import extract_segments_from_activity, parse_metrics


class TestExtractSegments(unittest.TestCase):
    def test_parse_metrics_and_segment_extraction(self):
        # Mock activity detail metrics response from Garmin
        mock_details = {
            "metricDescriptors": [
                {"key": "sumDistance", "metricsIndex": 0},
                {"key": "directElevation", "metricsIndex": 1},
                {"key": "directDoubleCadence", "metricsIndex": 2},
                {"key": "directHeartRate", "metricsIndex": 3},
                {"key": "directSpeed", "metricsIndex": 4},
            ],
            "activityDetailMetrics": [
                {"metrics": [0.0, 100.0, 160.0, 135.2, 3.0]},
                {"metrics": [50.0, 101.0, 162.0, 136.6, 3.0]},
                {"metrics": [105.83, 102.0, 158.0, 137.4, 3.0]},
            ],
        }

        points = parse_metrics(mock_details)
        self.assertEqual(len(points), 3)

        segments = extract_segments_from_activity(
            points, segment_len=100.0, tolerance=0.01
        )
        self.assertEqual(len(segments), 1)

        seg = segments[0]

        # 1. Distance should be rounded to nearest integer (105.83 -> 106)
        self.assertIsInstance(seg["distance"], int)
        self.assertEqual(seg["distance"], 106)

        # 2. Avg HR should be rounded to nearest integer (mean of [135.2, 136.6, 137.4] = 136.4 -> 136)
        self.assertIsInstance(seg["avg_hr"], int)
        self.assertEqual(seg["avg_hr"], 136)

        # 3. Cadence should be non-zero (mean of [160, 162, 158] = 160.0)
        self.assertGreater(seg["avg_cadence"], 0)
        self.assertAlmostEqual(seg["avg_cadence"], 160.0, places=2)

        # 4. Incline should be in degrees unit of measure (grade = (102 - 100)/105.83 = 0.018898, deg ≈ 1.0826°)
        expected_grade = (102.0 - 100.0) / 105.83
        expected_deg = math.degrees(math.atan(expected_grade))
        self.assertAlmostEqual(seg["incline"], expected_deg, places=2)

    def test_direct_run_cadence_fallback(self):
        # Test fallback when only directRunCadence (single leg) is provided
        mock_details = {
            "metricDescriptors": [
                {"key": "sumDistance", "metricsIndex": 0},
                {"key": "directElevation", "metricsIndex": 1},
                {"key": "directRunCadence", "metricsIndex": 2},
                {"key": "directHeartRate", "metricsIndex": 3},
                {"key": "directSpeed", "metricsIndex": 4},
            ],
            "activityDetailMetrics": [
                {"metrics": [0.0, 50.0, 80.0, 140.0, 3.0]},
                {"metrics": [100.0, 50.0, 82.0, 140.0, 3.0]},
            ],
        }

        points = parse_metrics(mock_details)
        segments = extract_segments_from_activity(
            points, segment_len=100.0, tolerance=0.01
        )
        self.assertEqual(len(segments), 1)

        # directRunCadence single-leg avg = 81 -> total SPM = 162.0
        self.assertEqual(segments[0]["avg_cadence"], 162.0)

    def test_vertical_oscillation_and_balance_extraction(self):
        # Mock activity detail metrics with running dynamics
        mock_details = {
            "metricDescriptors": [
                {"key": "sumDistance", "metricsIndex": 0},
                {"key": "directElevation", "metricsIndex": 1},
                {"key": "directDoubleCadence", "metricsIndex": 2},
                {"key": "directHeartRate", "metricsIndex": 3},
                {"key": "directSpeed", "metricsIndex": 4},
                {"key": "directVerticalOscillation", "metricsIndex": 5},
                {"key": "directGroundContactBalanceLeft", "metricsIndex": 6},
            ],
            "activityDetailMetrics": [
                {"metrics": [0.0, 100.0, 160.0, 140.0, 3.0, 9.5, 49.0]},
                {"metrics": [50.0, 100.0, 162.0, 142.0, 3.0, 10.0, 48.5]},
                {"metrics": [100.0, 100.0, 164.0, 144.0, 3.0, 10.5, 49.5]},
            ],
        }

        points = parse_metrics(mock_details)
        segments = extract_segments_from_activity(
            points, segment_len=100.0, tolerance=0.01
        )
        self.assertEqual(len(segments), 1)
        seg = segments[0]

        # Vertical Oscillation avg = (9.5 + 10.0 + 10.5) / 3 = 10.0
        self.assertAlmostEqual(seg["avg_vertical_oscillation"], 10.0, places=2)
        # Left Balance avg = (49.0 + 48.5 + 49.5) / 3 = 49.0%
        self.assertAlmostEqual(seg["avg_ground_contact_balance_left"], 49.0, places=2)
        # Right Balance avg = (51.0 + 51.5 + 50.5) / 3 = 51.0%
        self.assertAlmostEqual(seg["avg_ground_contact_balance_right"], 51.0, places=2)

    def test_missing_dynamics_returns_none(self):
        # Mock activity without running dynamics metrics
        mock_details = {
            "metricDescriptors": [
                {"key": "sumDistance", "metricsIndex": 0},
                {"key": "directElevation", "metricsIndex": 1},
                {"key": "directDoubleCadence", "metricsIndex": 2},
                {"key": "directHeartRate", "metricsIndex": 3},
                {"key": "directSpeed", "metricsIndex": 4},
            ],
            "activityDetailMetrics": [
                {"metrics": [0.0, 100.0, 160.0, 140.0, 3.0]},
                {"metrics": [100.0, 100.0, 160.0, 140.0, 3.0]},
            ],
        }

        points = parse_metrics(mock_details)
        segments = extract_segments_from_activity(
            points, segment_len=100.0, tolerance=0.01
        )
        self.assertEqual(len(segments), 1)
        seg = segments[0]

        self.assertIsNone(seg["avg_vertical_oscillation"])
        self.assertIsNone(seg["avg_ground_contact_balance_left"])
        self.assertIsNone(seg["avg_ground_contact_balance_right"])


if __name__ == "__main__":
    unittest.main()

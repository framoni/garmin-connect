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
        self.assertAlmostEqual(seg["incline"], expected_deg, places=4)

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


if __name__ == "__main__":
    unittest.main()

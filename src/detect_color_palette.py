from pathlib import Path

import cv2
import numpy as np



IMAGE_PATH = Path("Images") / "PodImages" / "PI_227393_85i_pod122.jpg"


def _require_mcc():
	if not hasattr(cv2, "mcc"):
		raise RuntimeError(
			"This OpenCV build does not include cv2.mcc. "
			"Install dependencies with: pip install -r requirements.txt"
		)
	return cv2.mcc

def detect_colorchecker(image: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
	mcc = _require_mcc()
	detector = mcc.CCheckerDetector_create()

	if not detector.process(image, mcc.MCC24):
		raise RuntimeError(
			"OpenCV MCC could not confidently detect a ColorChecker Classic "
			"in this image."
		)

	checker = detector.getBestColorChecker()
	if checker is None:
		raise RuntimeError("OpenCV detected no usable ColorChecker Classic chart.")

	try:
		chart_data = np.asarray(checker.getChartsRGB(), dtype=np.float32)
	except cv2.error as error:
		raise RuntimeError(
			"OpenCV MCC detected a chart, but failed while extracting its RGB values:\n"
			f"{error}"
		) from error

	if chart_data.ndim != 2 or chart_data.shape[0] != 72 or chart_data.shape[1] < 5:
		raise RuntimeError(
		f"OpenCV returned unexpected RGB data with shape {chart_data.shape}."
		)
	values = chart_data[:, [1, 3, 4]].reshape(24, 3, 3).mean(axis=1)

	box = np.asarray(checker.getBox(), dtype=np.float32).reshape(-1, 2)
	return values, box


def get_reference_colorchecker() -> tuple[str, np.ndarray, np.ndarray, np.ndarray]:
	"""Return reference xyY, sRGB, and Lab values for the ColorChecker."""
	try:
		import colour
	except ImportError as error:
		raise RuntimeError(
			"colour-science is required for ColorChecker reference values. "
			"Install dependencies with: pip install -r requirements.txt"
		) from error

	checkers = getattr(colour.characterisation, "COLORCHECKERS", None)
	if checkers is None:
		from colour.characterisation import CCS_COLOURCHECKERS

		checkers = CCS_COLOURCHECKERS

	checker_name = "ColorChecker24 - After November 2014"
	if checker_name not in checkers:
		checker_name = "ColorChecker24 - After Nov 2014"
	if checker_name not in checkers:
		raise RuntimeError("The installed colour-science version has no ColorChecker24 reference dataset.")

	checker = checkers[checker_name]
	xyy_values = np.asarray(list(checker.data.values()), dtype=np.float32)
	illuminant = np.asarray(checker.illuminant)
	xyz_values = colour.xyY_to_XYZ(xyy_values)
	srgb_values = colour.XYZ_to_sRGB(xyz_values, illuminant=illuminant) * 255
	lab_values = colour.XYZ_to_Lab(xyz_values, illuminant=illuminant)
	return (
		checker_name,
		xyy_values,
		srgb_values,
		lab_values,
	)



def display_overlay(image: np.ndarray, box: np.ndarray) -> None:
	max_display_width = 1600
	max_display_height = 900
	height, width = image.shape[:2]
	scale = min(max_display_width / width, max_display_height / height, 1.0)
	overlay = cv2.resize(
		image,
		None,
		fx=scale,
		fy=scale,
		interpolation=cv2.INTER_AREA,
	)
	corners = np.round(box * scale).astype(np.int32)
	cv2.polylines(overlay, [corners], True, (0, 255, 0), 3)

	for patch_number, point in enumerate(corners, start=1):
		x, y = int(point[0]), int(point[1])
		cv2.putText(
			overlay,
			str(patch_number),
			(x, y),
				cv2.FONT_HERSHEY_SIMPLEX,
				0.7,
			(0, 0, 255),
			2,
			cv2.LINE_AA,
		)
	cv2.namedWindow("ColorChecker Detection", cv2.WINDOW_NORMAL)
	cv2.imshow("ColorChecker Detection", overlay)
	cv2.waitKey(0)
	cv2.destroyAllWindows()


def main():
	image = cv2.imread(str(IMAGE_PATH), cv2.IMREAD_COLOR)
	print(f"The image path is {IMAGE_PATH}")
	if image is None:
		raise FileNotFoundError(f"Could not read image at {IMAGE_PATH}")

	values, box = detect_colorchecker(image)

	print("Detected ColorChecker Classic: 24 patches")
	print("Here are the RGB values for each patch:")
	for index, rgb in enumerate(values, start=1):
		rgb_values = [round(float(channel), 3) for channel in rgb]
		print(f"  Patch {index}: {rgb_values}")

	checker_name, reference_xyy, reference_srgb, reference_lab = get_reference_colorchecker()
	print(f"\nReference values ({checker_name}, sRGB 0-255):")
	for index, reference in enumerate(reference_srgb, start=1):
		print(f"  Patch {index}: {[round(float(channel), 3) for channel in reference]}")

	print("\nReference values (CIE Lab):")
	for index, reference in enumerate(reference_lab, start=1):
		print(f"  Patch {index}: {[round(float(channel), 3) for channel in reference]}")

	display_overlay(image, box)


if __name__ == "__main__":
	try:
		main()
	except (FileNotFoundError, OSError, RuntimeError) as error:
		raise SystemExit(f"Error: {error}") from error

# AlfalfaCV

AlfalfaCV is an R Shiny application for measuring alfalfa morphology from USDA accession images. Each accession is identified from the filename (`PI_` or `W6_`).

Seed analysis is implemented. Pods, leaves, and whole plants are listed in the sidebar and are not available yet.

## Layout

The window has four regions:

1. **Workflow sidebar.** Seeds, Pods, Leaves, and Whole Plants. A panel control next to the title collapses the sidebar without changing the selected workflow or any analysis state. Debug Mode sits at the bottom of the sidebar and defaults to off.
2. **Input panel.** Upload and Run Seed Detection. Technical metadata and detection parameters appear only in Debug Mode.
3. **Image workspace.** One vertical column: the full image, the ROI preview, then the ruler calibration preview.
4. **Results.** Seed detection, individual measurements, and a statistical-summary section. The summary section is reserved and does not compute statistics yet.

The input, image, and results columns scroll independently. The image column scrolls as one column. Images keep their aspect ratio.

## Installation

R 4.2 or newer, and ImageMagick (used by the `magick` package).

```bash
brew install imagemagick
```

```r
install.packages(c("shiny", "bslib", "magick", "mmand", "testthat", "png"))
```

| Package | Role |
|---|---|
| `shiny` | Application |
| `bslib` | Layout and theme |
| `magick` | Load, resize, and crop images |
| `mmand` | Segmentation and connected components |
| `testthat` | Automated tests |
| `png` | Synthetic test fixtures |

## Run

From the project root:

```bash
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

Uploaded images stay in the session. They are not written into the project.

## Accession names

```text
PI_<digits>[optional_suffix].jpg|jpeg|png
W6_<digits>[optional_suffix].jpg|jpeg|png
```

Example: `PI_227393_85i_120.jpg` is accession `PI_227393`. Parsing uses the basename and is case-insensitive. A name that does not match is still accepted for preview; the accession field stays empty.

## Seed workflow

1. Choose **Seeds** and upload a JPEG or PNG. The progress bar stays visible while the file uploads and the image is prepared. It clears when that finishes. A successful upload shows **Ready — Image uploaded**. A failed upload shows an error instead of a stuck progress bar. The same control replaces the current image.
2. On the full image, drag to draw a region of interest. Drag the box to move it, or drag a handle to resize it. Coordinates are stored in original-image pixels. Right-click inside the region and choose **Reset ROI** to restore the default region. Right-clicking outside the region does not reset it.
3. Open the ruler preview below the ROI preview. Choose **Calibrate**, click the first mark, then click a second mark to its right. The guide stays horizontal. The reference distance is fixed at 10 mm. Confirm, retry, or cancel. Confirm stores `pixels_per_mm = (x2 - x1) / 10`. **Recalibrate** starts a new selection and keeps the previous confirmed scale until the new one is confirmed. Cancel and retry do not replace a confirmed scale.
4. Choose **Run Seed Detection**. Pixel measurements are available before calibration. Millimetre measurements appear only after a confirmed 10 mm scale for the current image.
5. Read detection status and the individual measurements table in the results column.

A tilted ruler is not deskewed. The scale uses the horizontal pixel distance only, so align the ruler horizontally before calibrating.

Changing the ROI clears detections and measurements that depended on it. Changing a confirmed scale clears physical-unit measurements that used the previous scale. Uploading a different image resets that image’s ROI and calibration. Debug Mode and collapsing the sidebar do not change analysis settings.

## Debug Mode

When Debug Mode is on, the input panel shows:

- Filename, accession, dimensions, and upload status
- ROI coordinates and dimensions
- Detection sensitivity (default 0.05), minimum component area, and maximum component area as a fraction of the ROI (default 0.24)
- Calibration method, the two reference points, and pixels per millimetre

The minimum-area field starts at 200 only until an image and ROI exist. After upload it is replaced by the size suggested for that ROI. That 200 is not the scientific default (`DEFAULT_DETECTION_PARAMS$min_area` is unset until then).

Detection parameters used in a run are stored with the detection result.

## Project structure

```text
AlfalfaCV/
├── app.R
├── R/
│   ├── image_io.R           # Upload checks and accession parsing
│   ├── roi.R                # ROI coordinates and the default region
│   ├── image_processing.R   # Previews and crops
│   ├── seed_detection.R     # Segmentation
│   ├── ruler_calibration.R  # Horizontal 10 mm scale
│   └── morphology.R         # Shape measurements and unit conversion
├── www/                     # Layout, ROI interaction, calibration clicks
├── tests/testthat/
└── scripts/create_synthetic_fixture.R
```

## Tests

```bash
Rscript tests/testthat.R
```

Tests use synthetic images, not confidential USDA photographs.

## Limitations

- Pods, leaves, and whole plants are not implemented.
- The statistical summary panel does not calculate summaries yet.
- There is no manual seed-boundary editing and no CSV export.
- A tilted ruler is rejected rather than corrected. The scale is the horizontal distance only.
- Perimeter is a radial boundary estimate and is approximate for strongly concave shapes.
- Very large ROIs are segmented at original resolution, so detection can be slow.
- The ROI preview is limited to about 420 pixels tall. The whole crop is scaled to fit that height and the column width; it is not cropped.

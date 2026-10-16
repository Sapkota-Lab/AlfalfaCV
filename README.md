# AlfalfaCV

AlfalfaCV is an R Shiny research application for analyzing morphological characteristics of alfalfa breeds using computer vision. Each breed is identified by an accession identifier (`PI_` or `W6_`).

**Current milestone:** USDA Display Draft  
**Current phase:** Phase 1B — Computer Vision Foundation (ROI + seed detection)

> Physical measurements, calibration, and shape classification are **not** implemented in Phase 1B.

## Scope

Phase 1B adds seed computer-vision foundations on top of Phase 1A upload/display:

- Automatic seed ROI initialization
- Interactive ROI adjustment (brush)
- Downscaled previews + original-resolution ROI crops
- Background-aware seed segmentation (no fixed hue, no ML)
- Connected-component candidate detection
- Numbered detection overlays and counts
- Adjustable sensitivity / area filters

## Project structure

```text
AlfalfaCV/
├── app.R                      # Shiny orchestration
├── R/
│   ├── image_io.R             # Upload validation / header metadata
│   ├── roi.R                  # ROI defaults, validation, coordinates
│   ├── image_processing.R     # magick previews and ROI crops
│   └── seed_detection.R       # Segmentation and detections
├── tests/
│   ├── fixtures/PI_100001.png # Synthetic UI fixture
│   └── testthat/
├── scripts/create_synthetic_fixture.R
└── README.md
```

A legacy Python prototype may also exist in the repository; it is unused by this Shiny app.

## Installation

1. Install [R](https://cran.r-project.org/) (4.2+ recommended).
2. Install system ImageMagick (required by the `magick` package), e.g. on macOS:

```bash
brew install imagemagick
```

3. Install R packages:

```r
install.packages(c("shiny", "bslib", "magick", "mmand", "testthat"))
```

Optional (fixture generation): `install.packages("png")`

### Required R packages

| Package | Purpose |
|---|---|
| `shiny` | Application framework |
| `bslib` | UI layout/theme |
| `magick` | Efficient image load, resize, crop |
| `mmand` | Morphology + connected components |
| `testthat` | Automated tests |

## Launch

From the project root:

```r
shiny::runApp(".")
```

Or:

```bash
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

Uploaded images remain session-scoped and are not saved permanently into the project.

## Filename convention

```text
PI_<digits>[optional_suffix].jpg|jpeg|png
W6_<digits>[optional_suffix].jpg|jpeg|png
```

Examples: `PI_227393_85i_120.jpg` → `PI_227393`

## Typical workflow

1. Upload a seed image
2. Inspect the default ROI (cyan rectangle)
3. Optionally enable **Adjust ROI** and brush a new rectangle; or **Reset ROI**
4. Adjust detection sensitivity / area filters if needed
5. Click **Run Seed Detection**
6. Inspect numbered overlays in the ROI preview and the detection count

## Segmentation notes

Detection uses a dark-background-aware color-distance **and** luminance threshold, light morphological opening, connected-component labeling, and contrast filtering. It does **not** assume yellow seeds. Results are seed *candidates*, not scientifically validated seeds.

Default controls are conservative (sensitivity `0.05`, max area fraction `0.24`, ROI-aware minimum area). Images are loaded with EXIF orientation applied. Previews are drawn with top-left origin and y-down coordinates so ROI overlays match crops.

## Tests

```bash
Rscript tests/testthat.R
```

## Current limitations

- No ruler/palette calibration
- No physical measurements or statistics
- No manual boundary correction
- Very large ROIs still allocate full ROI pixel arrays for segmentation
- Browser display of huge originals may remain slow
- Detection quality depends on ROI placement and parameters

## Planned next phases

- Phase 1C: calibration and morphological measurements
- Later: leaves, pods, whole plants

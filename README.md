# ColossalCode
Code related to the publication: TBA

- Three R scripts:
  - RandomDraw.R performs random draws to take vertebral position uncertainty into account.
  - coreProps.R extrapolates the area sampled with the core drills to the whole section of centrum (approximated as an ellipse)
  - PGLSs.R runs the regressions and draws the plot used for Fig. 4
- ImageJ macro (global compactness measurement along the core drill): V0.1. Takes a single ROI in entry, and divide it in 10 sub-ROIs along the y-axis. Then standard measurements are taken for each sub-ROI. This was performed to extrapolate the area sampled with the core drills (see coreProps.R).


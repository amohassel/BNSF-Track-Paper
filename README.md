# Track Defect Degradation & Inspection Cost Models

R scripts for modeling rail track defect growth (DIP, surface, cross-level) against cumulative tonnage, and for choosing inspection intervals that minimize expected cost.

## Layout

| Folder | Contents |
|---|---|
| `R/01_data_prep/` | Import training data, clean defects, assign tonnage, build inspection tables, split train/test (scripts 01–03, 05, 06) |
| `R/02_linear_models/` | Linear and Bayesian linear models of defect growth (04) |
| `R/03_weibull_models/` | Weibull hazard models and exceedance probabilities by defect type (07, 08) |
| `R/04_classification_models/` | Logistic regression (09) and GBM (11) |
| `R/05_gamma_models/` | Gamma-process degradation models for DIP, surface and cross-level (10); `variants/` holds earlier DIP versions |
| `R/06_cost_optimization/` | Inspection-interval cost functions and grid search (12, Lamar conference) |
| `results/` | Inspection-interval figure (cost tables are excluded for now) |

Script numbers give the run order.

## Data

The input data is not included. Scripts expect this layout under the data root:

- `Input/` – raw training, tonnage and inspection-run CSVs
- `90/Intermediate_Data/` – files produced by the data-prep scripts (e.g. `DIP_train.csv`, `DIP_test.csv`)
- `90/Output/` – plots and tables written by the models

## Running

Each script sets its working directory from `data_dir`, which reads the `BNSF_DIR` environment variable (default: the current folder). Point it at the data root, the folder that holds `Input/` and the `90/` subfolder with `Intermediate_Data/` and `Output/`:

```r
Sys.setenv(BNSF_DIR = "/path/to/BNSF")
```

or edit the `data_dir` line at the top of a script.

Packages used: `dplyr`, `tidyr`, `purrr`, `lubridate`, `readxl`, `ggplot2`, `stargazer`, `stats4`, `pracma`, `zipfR`, `cubature`, `fitdistrplus`, `caret`, `ROCR`, `Metrics`, `rjags`, `BaTFLED3D`, `foreach`, `doParallel`, `tictoc`.

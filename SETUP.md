# Smile project — updated pipeline

Everything has been rewritten for the new setup: raw data on the research
drive, report in its own folder.

## Folder layout assumed

```
/Volumes/PRJ-smile/
└── raw_data_all/              ← raw FaceReader exports (research drive)
    ├── Chinese Fake Smiles/
    ├── Chinese Genuine Smiles/
    ├── European Fake Smiles/
    └── European Genuine Smiles/

Smile_Project/                 ← local
├── r_scripts/
│   ├── 00_config.R            ← paths and decisions live HERE only
│   ├── 00a_inspect_raw_data.R
│   ├── 01_extract_facereader.R
│   ├── 02_analysis.R
│   └── 03_power_simulation.R
├── results_outputs/           ← everything generated
└── report/
    ├── _quarto.yml
    ├── report.qmd
    ├── styles.css
    ├── images/
    └── _site/
```

## What to do

Replace all five files in `r_scripts/`. Delete the old `00_inspect_raw_data.R`,
`00b_diagnose_duplicates.R` and `00c_verify_duplicates.R` — they are merged
into `00a_inspect_raw_data.R`.

Then run, from the `Smile_Project` folder:

```r
source("r_scripts/00a_inspect_raw_data.R")   # read-only check, ~1 min
source("r_scripts/01_extract_facereader.R")  # parse + dedupe, ~2 min
source("r_scripts/02_analysis.R")            # models, ~1 min
source("r_scripts/03_power_simulation.R")    # slow, run once
```

## One change needed in report.qmd

The report sits one folder deeper now, so its path to the outputs must
change. In the setup chunk:

```r
OUT <- "../results_outputs"      # was "results_outputs"
```

Also update the output filenames, which have shifted:

| Old | New |
|---|---|
| `09_confirmatory_results.csv` | `09_primary_result.csv` |
| `10_exploratory_results.csv` | `10_secondary_results.csv` |
| — | `11_exploratory_results.csv` |
| `11_analysis_log.txt` | `12_analysis_log.txt` |
| `12_power_simulation.csv` | `13_power_simulation.csv` |

Render from inside `report/` as usual — `_quarto.yml` is found there.

## The one file you will ever need to edit

`00_config.R` holds the drive path, the coding rules, and the analysis
decisions. Nothing else hard-codes a path. If the drive letter changes, or
you want to work from a local copy, change the single `RAW_DIR` line.

## Only two scripts need the drive

`00a` and `01` read the research drive. `02`, `03` and the report work
entirely from local `results_outputs/`, so you can analyse on a train.

If the drive is not mounted, `01` stops with instructions rather than a
confusing empty result.

## Decisions now written into the code

From the meeting of 11 August, recorded in `00_config.R` and echoed into
the analysis log every run:

| Decision | Setting |
|---|---|
| No stimulus random effect | `USE_STIMULUS_RANDOM_EFFECT <- FALSE` |
| Keep smile-less clips | `EXCLUDE_SMILELESS_CLIPS <- FALSE` |
| Primary outcome | `PRIMARY_OUTCOME <- "duchenne_prop"` |
| Peak intensity demoted | listed under `SECONDARY_OUTCOMES` |

Each is annotated in the config with the reason, so the rationale travels
with the code rather than living only in a meeting transcript.

## New outputs

| File | Contents |
|---|---|
| `00a_inspection_report.txt` | Pre-flight check on the raw data |
| `00a_duplicate_pairs.csv` | Every duplicate group found |
| `01b_duplicates_dropped.csv` | Which copy was dropped, which kept |
| `09_primary_result.csv` | The single primary outcome |
| `10_secondary_results.csv` | Secondary set, FDR corrected |
| `11_exploratory_results.csv` | Remaining action units, FDR corrected |
| `12_analysis_log.txt` | Full audit trail, including the decisions |
| `13_power_simulation.csv` | Power grid |

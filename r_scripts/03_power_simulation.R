# =============================================================================
# 03_power_simulation.R
#
# Purpose : Simulation-based power analysis for the genuine-vs-fake design,
#           using variance components estimated from the pilot data.
# Output  : results_outputs/12_power_simulation.csv
#
# SLOW - roughly 5-15 minutes depending on the grid. Run it once; the analysis
# and the report both just read the saved CSV. Re-run only if the variance
# components change materially once the full data arrives.
# =============================================================================

suppressMessages(library(nlme))
set.seed(2026)

# --- Parameters estimated from the pilot data (AU06_pk, genuine smiles) ------
# Re-estimate these from 07_analysis_dataset.csv once the full data lands.
SD_SMILER <- 1.04    # between-smiler SD
SD_RESID  <- 0.885   # residual (within-smiler, between-smile) SD

SMILES_PER_TYPE <- 3:6    # each smiler contributes this many of each type
INTERACTION_FRAC <- 0.5   # interaction size as a fraction of the main effect
ALPHA <- 0.05

GRID <- expand.grid(k = c(10, 20, 30), d = c(0.3, 0.5, 0.8))
NREP <- 500

find_project_root <- function() {
  p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 1:4) {
    if (dir.exists(file.path(p, "results_outputs"))) return(p)
    p <- dirname(p)
  }
  stop("Could not locate 'smile_project'.", call. = FALSE)
}
out_dir <- file.path(find_project_root(), "results_outputs")

# --- Simulate one dataset ----------------------------------------------------
sim_once <- function(k, d, inter_frac) {
  rows <- list()
  for (cul in c("A", "B")) {
    eff <- if (cul == "A") d else d * inter_frac
    for (s in seq_len(k)) {
      sid <- paste0(cul, s)
      u   <- rnorm(1, 0, SD_SMILER)          # this smiler's expressiveness
      for (ty in c("Gen", "Fake")) {
        n <- sample(SMILES_PER_TYPE, 1)      # unbalanced, as in the real data
        rows[[length(rows) + 1]] <- data.frame(
          sid = sid, cul = cul, ty = ty,
          y = u + (if (ty == "Gen") eff * SD_RESID else 0) + rnorm(n, 0, SD_RESID),
          stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, rows)
}

test_once <- function(dat, term) {
  m <- try(lme(y ~ ty * cul, random = ~ 1 | sid, data = dat, method = "ML"),
           silent = TRUE)
  if (inherits(m, "try-error")) return(NA_real_)
  a <- try(anova(m), silent = TRUE)
  if (inherits(a, "try-error")) return(NA_real_)
  a[["p-value"]][rownames(a) == term]
}

# --- Run the grid ------------------------------------------------------------
res <- list()
for (i in seq_len(nrow(GRID))) {
  k <- GRID$k[i]; d <- GRID$d[i]
  message(sprintf("k = %d, d = %.1f  (%d of %d)", k, d, i, nrow(GRID)))
  pm <- pin <- numeric(NREP)
  for (r in seq_len(NREP)) {
    dat    <- sim_once(k, d, INTERACTION_FRAC)
    pm[r]  <- test_once(dat, "ty")
    pin[r] <- test_once(dat, "ty:cul")
  }
  res[[i]] <- data.frame(
    smilers_per_culture = k,
    effect_d            = d,
    power_main          = round(mean(pm  < ALPHA, na.rm = TRUE), 3),
    power_interaction   = round(mean(pin < ALPHA, na.rm = TRUE), 3),
    reps                = NREP)
}

out <- do.call(rbind, res)
out <- out[order(out$effect_d, out$smilers_per_culture), ]
write.csv(out, file.path(out_dir, "12_power_simulation.csv"), row.names = FALSE)

print(out, row.names = FALSE)
message("\nMonte Carlo SE is about ", sprintf("%.1f", 100 * sqrt(.25 / NREP)),
        " percentage points; small non-monotonicities are simulation noise.")
message("Wrote 12_power_simulation.csv")

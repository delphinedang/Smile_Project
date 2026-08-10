# =============================================================================
# 02_analysis.R
#
# Project : Genuine vs fake smiles (Ruffman / Kang collaboration)
# Purpose : Implements the pre-specified analysis framework.
#           Run AFTER 01_extract_facereader.R.
#
# Input   : smile_project/results_outputs/02_frames_wide.csv
#           smile_project/results_outputs/03_clip_summary_long.csv
# Output  : smile_project/results_outputs/07_* onwards
#
# DESIGN NOTE
#   The script checks design coverage before modelling. With only one
#   smile type present it runs in PILOT mode: derived variables,
#   descriptives and assumption checks only, with the inferential models
#   reported as BLOCKED. When the remaining three cells arrive it switches
#   to FULL mode automatically. Nothing needs editing in between.
#
# PACKAGES
#   Uses lme4 if installed, otherwise nlme (which ships with R).
#   No other packages required.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. CONFIGURATION
# -----------------------------------------------------------------------------

# Confirmatory outcomes - pre-specified, NOT corrected for multiplicity,
# because they are a small hypothesis-driven set agreed in advance.
CONFIRMATORY <- c("AU06_pk", "AU12_pk", "duchenne_prop", "Happy_pk")

# Exploratory family - every remaining AU. FDR corrected.
ALPHA     <- 0.05
FDR_ALPHA <- 0.05

# Minimum cell size before a model is attempted
MIN_PER_CELL   <- 5
MIN_SMILERS    <- 6

# Outcomes that need clip duration as a covariate.
#
# REVISED after the pilot analysis. Cumulative outcomes obviously depend on
# clip length. But PEAK outcomes do too, and less obviously: the maximum of a
# longer series is stochastically larger, so peak intensity rises with clip
# duration purely as a sampling artefact. In the pilot data peak AU12 rose with
# duration (beta = 0.26, 95% CI [0.08, 0.44]), and that association vanished
# when every clip was truncated to a common window (beta = 0.03, p = .76).
#
# If clip lengths differ between genuine and performed smiles, an uncorrected
# peak comparison is biased. Proportions (_prop) and duchenne_prop are
# scale-free and are the only outcomes genuinely exempt.
NEEDS_DURATION_COVARIATE <- c("_pk", "_durtot", "_durmax", "_auc", "_neps", "_tpk")


# -----------------------------------------------------------------------------
# 1. PATHS
# -----------------------------------------------------------------------------

find_project_root <- function() {
  p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 1:4) {
    if (dir.exists(file.path(p, "results_outputs"))) return(p)
    p <- dirname(p)
  }
  stop("Could not locate the 'smile_project' folder. Set project_root manually.",
       call. = FALSE)
}

project_root <- find_project_root()
# project_root <- "/Users/delphine/Documents/Research projects /smile_project"

out_dir <- file.path(project_root, "results_outputs")
fig_dir <- file.path(out_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

message("Project root : ", project_root)


# -----------------------------------------------------------------------------
# 2. LOAD
# -----------------------------------------------------------------------------

f_long   <- file.path(out_dir, "03_clip_summary_long.csv")
f_frames <- file.path(out_dir, "02_frames_wide.csv")
for (f in c(f_long, f_frames))
  if (!file.exists(f)) stop("Missing ", basename(f), ". Run 01_extract_facereader.R first.",
                            call. = FALSE)

clip_long <- read.csv(f_long,   stringsAsFactors = FALSE)
frames    <- read.csv(f_frames, stringsAsFactors = FALSE)

message("Loaded ", length(unique(clip_long$clip_id)), " clips.")


# -----------------------------------------------------------------------------
# 3. DERIVED VARIABLES
# -----------------------------------------------------------------------------
# These are the measures that speak directly to the Duchenne hypothesis but
# cannot be computed clip-by-clip without the frame-level data.

message("Computing derived variables from frame-level data...")

derive_clip <- function(d) {
  au12 <- d$AU12; au06 <- d$AU06
  on12 <- !is.na(au12) & au12 >= 1
  on06 <- !is.na(au06) & au06 >= 1
  dt   <- if (nrow(d) > 1) median(diff(d$time_s), na.rm = TRUE) else NA_real_

  # Duchenne co-activation: of the time the smile (AU12) was on, what
  # proportion also had cheek raise (AU06)? Undefined if AU12 never fires.
  duch <- if (sum(on12) > 0) sum(on12 & on06) / sum(on12) else NA_real_

  # Rise rate of the smile: only meaningful when we actually observed the
  # onset, i.e. AU12 was NOT already active in frame 1.
  rise <- NA_real_
  if (sum(on12) > 0 && !on12[1]) {
    onset_i <- which(on12)[1]
    peak_v  <- max(au12, na.rm = TRUE)
    peak_i  <- which(!is.na(au12) & au12 == peak_v)[1]
    if (peak_i > onset_i) rise <- peak_v / ((peak_i - onset_i) * dt)
  }

  # Smoothness: mean absolute frame-to-frame change in AU12. Posed smiles
  # are reported to be more irregular / steppier.
  sm <- if (sum(!is.na(au12)) > 1) mean(abs(diff(au12[!is.na(au12)]))) else NA_real_

  # Apex count: number of distinct local maxima at the peak value
  r <- rle(on12)
  data.frame(
    clip_id        = d$clip_id[1],
    duchenne_prop  = duch,
    au12_rise_rate = rise,
    au12_smooth    = sm,
    au12_n_bouts   = sum(r$values),
    stringsAsFactors = FALSE
  )
}

derived <- do.call(rbind, lapply(split(frames, frames$clip_id), derive_clip))


# -----------------------------------------------------------------------------
# 4. BUILD THE ANALYSIS TABLE (one row per clip)
# -----------------------------------------------------------------------------

id_cols <- c("clip_id", "smiler_id", "culture", "smile_type", "label_conflict",
             "stimulus", "stimulus_num", "take_num", "clip_duration_s", "n_frames")

# Take the FIRST row per clip rather than unique() across all id columns.
# unique() would silently emit several rows for a clip if any identifier
# disagreed between that clip's channel rows - which would inflate n and
# corrupt every model downstream. Take one row per clip, then verify.
dat <- clip_long[!duplicated(clip_long$clip_id), id_cols]

incons <- names(which(sapply(id_cols[-1], function(cl)
  any(tapply(clip_long[[cl]], clip_long$clip_id,
             function(x) length(unique(x[!is.na(x)])) > 1)))))
if (length(incons))
  stop("Identifier column(s) disagree between channel rows of the same clip: ",
       paste(incons, collapse = ", "),
       "\n  The extraction output is corrupt - re-run 01_extract_facereader.R.",
       call. = FALSE)

stopifnot(nrow(dat) == length(unique(clip_long$clip_id)))

# Spread the per-channel metrics into columns
key_metrics <- c(pk = "peak", mn = "mean_intensity", prop = "prop_active",
                 durmax = "dur_longest_s", durtot = "dur_total_s",
                 tpk = "t_peak_s", auc = "auc", neps = "n_episodes",
                 cens0 = "censored_start", cens1 = "censored_end")

for (ch in unique(clip_long$channel)) {
  sub <- clip_long[clip_long$channel == ch, ]
  idx <- match(dat$clip_id, sub$clip_id)
  for (sn in names(key_metrics))
    dat[[paste0(ch, "_", sn)]] <- sub[[key_metrics[[sn]]]][idx]
}

dat <- merge(dat, derived, by = "clip_id", all.x = TRUE)

# EXCLUSIONS - applied here, once, and logged.
n0 <- nrow(dat)
dat$exclude_reason <- NA_character_
dat$exclude_reason[dat$label_conflict == 1] <- "stimulus label conflict"
dat$exclude_reason[is.na(dat$smile_type)]   <- "smile type unresolved"

analysis <- dat[is.na(dat$exclude_reason), ]
excluded <- dat[!is.na(dat$exclude_reason), ]

analysis$smile_type <- factor(analysis$smile_type, levels = c("Fake", "Genuine"))
analysis$culture    <- factor(analysis$culture)
analysis$smiler_id  <- factor(analysis$smiler_id)
analysis$stimulus   <- factor(analysis$stimulus)

message("Analysis set: ", nrow(analysis), " clips (", nrow(excluded), " excluded).")


# -----------------------------------------------------------------------------
# 5. DESIGN COVERAGE GATE
# -----------------------------------------------------------------------------

cell_n     <- table(analysis$culture, analysis$smile_type)
smilers_by <- tapply(as.character(analysis$smiler_id), analysis$smile_type,
                     function(x) length(unique(x)))

n_types    <- sum(colSums(cell_n) > 0)
n_cultures <- sum(rowSums(cell_n) > 0)
cells_ok   <- sum(cell_n >= MIN_PER_CELL)

MODE <- if (n_types >= 2 && min(colSums(cell_n)[colSums(cell_n) > 0]) >= MIN_PER_CELL &&
            all(smilers_by[!is.na(smilers_by)] >= MIN_SMILERS)) {
  if (n_cultures >= 2 && cells_ok >= 4) "FULL" else "PARTIAL"
} else "PILOT"

message("Analysis mode: ", MODE)


# -----------------------------------------------------------------------------
# 6. DESCRIPTIVES
# -----------------------------------------------------------------------------

outcome_cols <- c(CONFIRMATORY,
                  grep("^AU[0-9]+_pk$",   names(analysis), value = TRUE),
                  grep("^AU[0-9]+_prop$", names(analysis), value = TRUE),
                  "au12_rise_rate", "au12_smooth", "au12_n_bouts")
outcome_cols <- unique(outcome_cols[outcome_cols %in% names(analysis)])

describe <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(c(n = 0, mean = NA, sd = NA, median = NA, min = NA, max = NA))
  c(n = length(v), mean = mean(v), sd = sd(v),
    median = median(v), min = min(v), max = max(v))
}

desc_rows <- list()
for (oc in outcome_cols) {
  for (ty in levels(analysis$smile_type)) {
    for (cu in levels(analysis$culture)) {
      sub <- analysis[analysis$smile_type == ty & analysis$culture == cu, ]
      s <- describe(sub[[oc]])
      desc_rows[[length(desc_rows) + 1]] <- data.frame(
        outcome = oc, culture = cu, smile_type = ty,
        n = s[["n"]], mean = s[["mean"]], sd = s[["sd"]],
        median = s[["median"]], min = s[["min"]], max = s[["max"]],
        stringsAsFactors = FALSE)
    }
  }
}
descriptives <- do.call(rbind, desc_rows)
descriptives[, 5:9] <- round(descriptives[, 5:9], 4)


# -----------------------------------------------------------------------------
# 7. MODEL ENGINE
# -----------------------------------------------------------------------------
# Mixed model with crossed random intercepts for smiler and stimulus.
# smile_type is within-smiler; culture is between-smiler.

USE_LME4 <- requireNamespace("lme4", quietly = TRUE)
ENGINE   <- if (USE_LME4) "lme4::lmer" else "nlme::lme"
message("Model engine: ", ENGINE)

fit_model <- function(outcome, data, include_culture) {

  y <- data[[outcome]]
  if (sum(!is.na(y)) < 10 || length(unique(y[!is.na(y)])) < 3)
    return(list(ok = FALSE, note = "too few valid or distinct values"))

  needs_dur <- any(sapply(NEEDS_DURATION_COVARIATE, function(p) grepl(p, outcome)))
  terms <- if (include_culture) "smile_type * culture" else "smile_type"
  if (needs_dur) terms <- paste(terms, "+ scale(clip_duration_s)")

  res <- try({
    if (USE_LME4) {
      f  <- as.formula(paste(outcome, "~", terms, "+ (1|smiler_id) + (1|stimulus)"))
      m  <- lme4::lmer(f, data = data, REML = TRUE)
      m0 <- lme4::lmer(update(f, . ~ . - smile_type), data = data, REML = FALSE)
      m1 <- lme4::lmer(f, data = data, REML = FALSE)
      lrt <- anova(m0, m1)
      list(coefs = summary(m)$coefficients,
           chisq = lrt$Chisq[2], df = lrt$Df[2], p = lrt$`Pr(>Chisq)`[2])
    } else {
      f <- as.formula(paste(outcome, "~", terms))
      m <- nlme::lme(f, random = ~ 1 | smiler_id, data = data,
                     na.action = na.omit, method = "REML")
      at <- anova(m)
      list(coefs = summary(m)$tTable,
           chisq = at[["F-value"]][2] , df = at[["denDF"]][2],
           p = at[["p-value"]][2])
    }
  }, silent = TRUE)

  if (inherits(res, "try-error"))
    return(list(ok = FALSE, note = paste("model failed:", trimws(as.character(res)[1]))))

  # Effect size: Hedges' g on raw clip values, as a readable companion
  g <- NA_real_
  a <- y[data$smile_type == "Genuine"]; b <- y[data$smile_type == "Fake"]
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (length(a) > 1 && length(b) > 1) {
    sp <- sqrt(((length(a)-1)*var(a) + (length(b)-1)*var(b)) / (length(a)+length(b)-2))
    if (is.finite(sp) && sp > 0) {
      d <- (mean(a) - mean(b)) / sp
      J <- 1 - 3 / (4 * (length(a) + length(b)) - 9)
      g <- d * J
    }
  }

  list(ok = TRUE, p = res$p, stat = res$chisq, df = res$df,
       hedges_g = g, n = sum(!is.na(y)),
       n_genuine = length(a), n_fake = length(b),
       covariate = needs_dur, note = "")
}

run_family <- function(outcomes, data, include_culture, label) {
  rows <- lapply(outcomes, function(oc) {
    r <- fit_model(oc, data, include_culture)
    data.frame(
      family = label, outcome = oc,
      n = if (isTRUE(r$ok)) r$n else NA_integer_,
      n_genuine = if (isTRUE(r$ok)) r$n_genuine else NA_integer_,
      n_fake    = if (isTRUE(r$ok)) r$n_fake else NA_integer_,
      statistic = if (isTRUE(r$ok)) round(r$stat, 4) else NA_real_,
      df        = if (isTRUE(r$ok)) round(r$df, 2) else NA_real_,
      p_raw     = if (isTRUE(r$ok)) r$p else NA_real_,
      hedges_g  = if (isTRUE(r$ok)) round(r$hedges_g, 4) else NA_real_,
      duration_covariate = if (isTRUE(r$ok)) r$covariate else NA,
      status = if (isTRUE(r$ok)) "fitted" else "not fitted",
      note   = if (isTRUE(r$ok)) "" else r$note,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}


# -----------------------------------------------------------------------------
# 8. RUN (or record as blocked)
# -----------------------------------------------------------------------------

confirm_res <- explore_res <- NULL

if (MODE %in% c("FULL", "PARTIAL")) {

  inc_cul <- (MODE == "FULL")

  confirm_res <- run_family(intersect(CONFIRMATORY, names(analysis)),
                            analysis, inc_cul, "confirmatory")
  confirm_res$p_adj  <- confirm_res$p_raw          # not corrected, by design
  confirm_res$method <- "uncorrected (pre-specified)"

  expl <- setdiff(grep("^AU[0-9]+_(pk|prop)$", names(analysis), value = TRUE),
                  CONFIRMATORY)
  explore_res <- run_family(expl, analysis, inc_cul, "exploratory")
  explore_res$p_adj  <- p.adjust(explore_res$p_raw, method = "BH")
  explore_res$method <- "Benjamini-Hochberg FDR"

} else {
  message("PILOT mode: inferential models skipped (insufficient design coverage).")
}


# -----------------------------------------------------------------------------
# 9. PLOTS
# -----------------------------------------------------------------------------

NAVY <- "#1E3A8A"; AMBER <- "#D97706"; RED <- "#BE123C"; GREY <- "#CBD5E1"

# --- Fig 1: raster of every clip's AU12 / AU06 time course ------------------
# One row per clip, time normalised to clip length, shade = intensity.
# Overplotting 90+ ordinal step functions as lines is unreadable; a raster
# shows every clip legibly and makes two things visible at once: bands that
# run to the right-hand edge are right-censored, and blank rows are clips
# with no activation at all.
raster_matrix <- function(ch, ids, ncol = 50) {
  t(sapply(ids, function(id) {
    v <- frames[[ch]][frames$clip_id == id]
    if (!length(v)) return(rep(NA_real_, ncol))
    approx(seq(0, 1, length.out = length(v)), v,
           xout = seq(0, 1, length.out = ncol), method = "constant", rule = 2)$y
  }))
}
ord <- analysis$clip_id[order(analysis$smile_type, analysis$AU12_prop)]
pal <- colorRampPalette(c("#F8FAFC", "#BFDBFE", "#60A5FA", "#1E3A8A"))(6)

png(file.path(fig_dir, "fig1_au12_au06_raster.png"),
    width = 1700, height = 1000, res = 155)
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(6, 1))
par(mar = c(4.2, 3.4, 2.6, 1))
for (ch in c("AU12", "AU06")) {
  m <- raster_matrix(ch, ord)
  image(x = seq(0, 1, length.out = ncol(m)), y = seq_len(nrow(m)),
        z = t(m), col = pal, zlim = c(0, 5), axes = FALSE,
        xlab = "Proportion through clip", ylab = "",
        main = paste0(ch, if (ch == "AU12") " - Lip Corner Puller"
                          else " - Cheek Raiser"))
  axis(1); box(col = "grey70")
  mtext("Clips, least to most active (bottom to top)", side = 2, line = 1, cex = .78)
}
par(mar = c(0, 0, 0, 0)); plot.new()
legend("center", legend = c("None", "A", "B", "C", "D", "E"), fill = pal,
       border = "grey70", bty = "n", cex = .95, horiz = TRUE,
       title = "AU intensity grade")
dev.off()
layout(1)

# --- Fig 2: mean trajectory by smile type ----------------------------------
png(file.path(fig_dir, "fig2_mean_trajectory.png"),
    width = 1700, height = 750, res = 155)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.6, 1))
for (ch in c("AU12", "AU06")) {
  plot(NA, xlim = c(0, 1), ylim = c(0, 4), xlab = "Proportion through clip",
       ylab = paste(ch, "intensity"), main = paste(ch, "- mean trajectory"))
  for (ty in levels(analysis$smile_type)) {
    ids <- analysis$clip_id[analysis$smile_type == ty]
    if (length(ids) < 3) next
    m  <- raster_matrix(ch, ids)
    mu <- colMeans(m, na.rm = TRUE)
    se <- apply(m, 2, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    xx <- seq(0, 1, length.out = length(mu))
    col <- if (ty == "Genuine") NAVY else AMBER
    polygon(c(xx, rev(xx)), c(mu - 1.96*se, rev(mu + 1.96*se)),
            col = adjustcolor(col, .18), border = NA)
    lines(xx, mu, col = col, lwd = 2.4)
  }
  legend("topleft", levels(analysis$smile_type), col = c(AMBER, NAVY),
         lwd = 2.4, bty = "n", cex = .85)
}
dev.off()

# --- Fig 3: censoring ------------------------------------------------------
png(file.path(fig_dir, "fig3_censoring.png"), width = 1500, height = 1000, res = 155)
par(mar = c(4.4, 6.5, 2.4, 2))
aus   <- grep("^AU[0-9]+_cens1$", names(analysis), value = TRUE)
rates <- sort(sapply(aus, function(a) mean(analysis[[a]], na.rm = TRUE)))
cols  <- ifelse(names(rates) %in% c("AU12_cens1", "AU06_cens1"), AMBER, NAVY)
barplot(rates * 100, horiz = TRUE, las = 1, xlim = c(0, 100), border = NA,
        names.arg = sub("_cens1", "", names(rates)), cex.names = .75,
        xlab = "% of clips still active in final frame", col = cols,
        main = "Right-censoring by action unit")
abline(v = 50, lty = 2, col = RED)
dev.off()

message("Wrote figures to ", fig_dir)


# -----------------------------------------------------------------------------
# 10. WRITE OUTPUTS
# -----------------------------------------------------------------------------

w <- function(obj, name) {
  write.csv(obj, file.path(out_dir, name), row.names = FALSE, na = "")
  message("  wrote ", name, " (", nrow(obj), " rows)")
}

w(analysis,     "07_analysis_dataset.csv")
w(descriptives, "08_descriptives_by_condition.csv")
if (!is.null(confirm_res)) w(confirm_res, "09_confirmatory_results.csv")
if (!is.null(explore_res)) w(explore_res, "10_exploratory_results.csv")


# -----------------------------------------------------------------------------
# 11. ANALYSIS LOG
# -----------------------------------------------------------------------------

con <- file(file.path(out_dir, "11_analysis_log.txt"), open = "wt")
p <- function(...) cat(..., "\n", sep = "", file = con)

p("SMILE PROJECT - ANALYSIS LOG")
p("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
p("Engine   : ", ENGINE)
p("Mode     : ", MODE)
p(strrep("=", 74))

p("\n1. SAMPLE")
p(strrep("-", 74))
p("  Clips extracted : ", n0)
p("  Excluded        : ", nrow(excluded))
if (nrow(excluded)) for (i in seq_len(nrow(excluded)))
  p("      ", excluded$clip_id[i], " - ", excluded$exclude_reason[i])
p("  Analysed        : ", nrow(analysis))
p("  Smilers         : ", length(unique(analysis$smiler_id)))
p("\n  Cells (rows = culture, cols = smile type):")
for (l in capture.output(print(cell_n))) p("    ", l)

p("\n2. MODE AND WHAT IT MEANS")
p(strrep("-", 74))
if (MODE == "PILOT") {
  p("  PILOT. The design does not yet support the planned models: at least")
  p("  one smile type has fewer than ", MIN_PER_CELL, " clips.")
  p("")
  p("  Completed: derived variables, descriptives, censoring audit, figures.")
  p("  BLOCKED  : all inferential models (genuine vs fake, culture")
  p("             interaction). These need the remaining data cells.")
  p("")
  p("  Nothing about the script needs to change when that data arrives.")
  p("  Re-run 01 then 02 and it will switch to FULL automatically.")
} else if (MODE == "PARTIAL") {
  p("  PARTIAL. Genuine vs fake can be tested, but only one culture is")
  p("  present so the culture main effect and the interaction are omitted.")
} else {
  p("  FULL. All planned models fitted.")
}

p("\n3. MODEL SPECIFICATION")
p(strrep("-", 74))
if (USE_LME4) {
  p("  outcome ~ smile_type * culture + (1 | smiler_id) + (1 | stimulus)")
  p("  Crossed random intercepts, fitted with lme4::lmer.")
} else {
  p("  outcome ~ smile_type * culture, random = ~ 1 | smiler_id")
  p("  Fitted with nlme::lme.")
  p("")
  p("  ** LIMITATION ** lme4 is not installed, so the STIMULUS random")
  p("  intercept was NOT fitted - nlme cannot handle crossed random")
  p("  effects cleanly. Variance due to some videos being funnier than")
  p("  others is therefore unmodelled, which makes the smile_type test")
  p("  slightly anti-conservative.")
  p("  Fix before the final analysis:  install.packages('lme4')")
}
p("")
p("  smile_type  within-smiler   (each person produced both types)")
p("  culture     between-smiler")
p("  smiler_id   random intercept - repeated smiles from one person are")
p("              not independent, and people vary in expressiveness")
p("  stimulus    random intercept - some videos are funnier than others")
p("")
p("  Unit of analysis is the individual smile, NOT the person mean.")
p("  This is deliberate: the number of smiles per person varies, and")
p("  averaging would treat a person with one smile as being as precise")
p("  as a person with eight.")
p("")
p("  Duration covariate added for peak, timing and cumulative outcomes:")
p("  ",
  paste(NEEDS_DURATION_COVARIATE, collapse = " "))

p("\n4. MULTIPLICITY")
p(strrep("-", 74))
p("  Confirmatory (uncorrected, pre-specified): ",
  paste(CONFIRMATORY, collapse = ", "))
p("  Exploratory (Benjamini-Hochberg FDR at ", FDR_ALPHA, "): all remaining AUs")
p("  This split must be fixed BEFORE the full data is seen.")

p("\n5. RESULTS")
p(strrep("-", 74))
if (is.null(confirm_res)) {
  p("  No inferential results - see mode above.")
} else {
  p("  CONFIRMATORY")
  p(sprintf("  %-16s %6s %8s %9s %10s", "outcome", "n", "p", "Hedges g", "status"))
  for (i in seq_len(nrow(confirm_res)))
    p(sprintf("  %-16s %6s %8s %9s %10s",
              confirm_res$outcome[i],
              ifelse(is.na(confirm_res$n[i]), "-", confirm_res$n[i]),
              ifelse(is.na(confirm_res$p_raw[i]), "-", format.pval(confirm_res$p_raw[i], digits = 3)),
              ifelse(is.na(confirm_res$hedges_g[i]), "-", sprintf("%.2f", confirm_res$hedges_g[i])),
              confirm_res$status[i]))
  sig <- explore_res[!is.na(explore_res$p_adj) & explore_res$p_adj < FDR_ALPHA, ]
  p("\n  EXPLORATORY: ", nrow(sig), " of ", nrow(explore_res),
    " outcomes survive FDR at ", FDR_ALPHA)
  if (nrow(sig)) for (i in seq_len(nrow(sig)))
    p(sprintf("    %-16s p_adj = %s  g = %.2f",
              sig$outcome[i], format.pval(sig$p_adj[i], digits = 3), sig$hedges_g[i]))
}

p("\n6. STANDING CAVEATS")
p(strrep("-", 74))
p("  a) AU intensity is ORDINAL (A-E). The models treat it as continuous.")
p("     Defensible with six levels and standard in this literature, but it")
p("     is an approximation - flag it in the write-up.")
p("  b) Duration outcomes are RIGHT-CENSORED. Where an AU is still active")
p("     in the final frame the recorded duration is a lower bound, not a")
p("     true value. Do not report raw mean durations for AU06, AU12, AU25")
p("     or AU43 without saying so.")
p("  c) 8 fps caps all timing resolution at 125 ms.")
p("  d) prop_active is conditional on the clip window and is the")
p("     recommended duration-like outcome while censoring stands.")

p("\n", strrep("=", 74))
p("END OF LOG")
close(con)
message("  wrote 11_analysis_log.txt")

message("\nDone. Mode = ", MODE)

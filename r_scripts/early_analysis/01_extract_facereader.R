# =============================================================================
# 01_extract_facereader.R
#
# Project : Genuine vs fake smiles (Ruffman / Kang collaboration)
# Purpose : Parse raw FaceReader "detailed log" exports into analysis-ready
#           datasets, and produce a data-quality report.
#
# Input   : smile_project/raw_data_email/*.txt   (FaceReader text exports)
# Output  : smile_project/results_outputs/       (csv + txt)
#
# Written in BASE R only - no packages required, so it runs anywhere,
# including on a locked-down university machine.
#
# Run either by:
#   - opening this file in RStudio and clicking Source, or
#   - Rscript r_scripts/01_extract_facereader.R   from the smile_project folder
# =============================================================================


# -----------------------------------------------------------------------------
# 0. CONFIGURATION  - the only bit you should normally need to edit
# -----------------------------------------------------------------------------

# Action units are scored NotActive / A / B / C / D / E.
# We recode these to 0 / 1 / 2 / 3 / 4 / 5.
# AU_ACTIVE_MIN sets what counts as "the muscle is switched on".
#   1 = A and above (FaceReader's own definition of active)
#   2 = B and above (stricter; ignores the weakest traces)
AU_ACTIVE_MIN <- 1

# Emotion channels are continuous 0-1 probabilities.
# EMO_ACTIVE_MIN sets what counts as the emotion being "present".
EMO_ACTIVE_MIN <- 0.5

# Smile-type coding. Videos are labelled HA (humorous stimulus) or SA (sad
# stimulus). Per Ted's description: people smiled genuinely while watching
# humorous videos and posed a smile while watching sad videos.
SMILE_TYPE_MAP <- c(HA = "Genuine", SA = "Fake")

# Culture coding, taken from the first letter of the internal clip ID.
# Only "E" appears in the current batch; W is pre-registered here so the
# script keeps working when the NZ European files arrive.
CULTURE_MAP <- c(E = "Chinese", W = "NZ_European")


# -----------------------------------------------------------------------------
# 1. PATHS
# -----------------------------------------------------------------------------

find_project_root <- function() {
  p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  # Walk up a few levels looking for the raw data folder
  for (i in 1:4) {
    if (dir.exists(file.path(p, "raw_data_email"))) return(p)
    p <- dirname(p)
  }
  stop(
    "Could not locate the 'smile_project' folder.\n",
    "  Set it manually, e.g.:\n",
    "  project_root <- 'C:/Users/you/Documents/smile_project'\n",
    "  and comment out the find_project_root() call below.",
    call. = FALSE
  )
}

project_root <- find_project_root()
# project_root <- "C:/path/to/smile_project"   # <- uncomment and edit if needed

raw_dir <- file.path(project_root, "raw_data_email")
out_dir <- file.path(project_root, "results_outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

message("Project root : ", project_root)
message("Raw data     : ", raw_dir)
message("Outputs      : ", out_dir)


# -----------------------------------------------------------------------------
# 2. HELPERS
# -----------------------------------------------------------------------------

# "00:01:23.375" -> 83.375
parse_video_time <- function(x) {
  parts <- do.call(rbind, strsplit(trimws(x), ":", fixed = TRUE))
  as.numeric(parts[, 1]) * 3600 + as.numeric(parts[, 2]) * 60 + as.numeric(parts[, 3])
}

# Pull a value out of the FaceReader header block, e.g. header_value(lines, "Frame rate")
header_value <- function(lines, key) {
  hit <- grep(paste0("^", key, "\t"), lines)
  if (!length(hit)) return(NA_character_)
  trimws(sub(paste0("^", key, "\t"), "", lines[hit[1]]))
}

# AU letter grades -> ordinal numbers. Anything unrecognised (FIT_FAILED,
# blanks) becomes NA so it can never be silently mistaken for a zero.
recode_au <- function(x) {
  x <- trimws(x)
  out <- rep(NA_real_, length(x))
  out[x == "NotActive"] <- 0
  out[x == "A"] <- 1
  out[x == "B"] <- 2
  out[x == "C"] <- 3
  out[x == "D"] <- 4
  out[x == "E"] <- 5
  out
}

# Emotion columns -> numeric, with FIT_FAILED etc. becoming NA (no warning spam)
recode_emotion <- function(x) {
  suppressWarnings(as.numeric(trimws(x)))
}

# "Action Unit 12 - Lip Corner Puller" -> list(code = "AU12", label = "Lip Corner Puller")
split_au_name <- function(nm) {
  num   <- sub("^Action Unit ([0-9]+).*$", "\\1", nm)
  label <- trimws(sub("^Action Unit [0-9]+ *- *", "", nm))
  list(code = paste0("AU", num), label = label)
}

# Count runs of TRUE, and measure them. NA is treated as not-active for the
# purpose of finding runs, but is excluded from the denominator elsewhere.
episode_stats <- function(active_lgl, dt) {
  a <- active_lgl
  a[is.na(a)] <- FALSE
  if (!any(a)) {
    return(list(n_episodes = 0L, dur_total = 0, dur_longest = 0))
  }
  r <- rle(a)
  lens <- r$lengths[r$values]
  list(
    n_episodes  = length(lens),
    dur_total   = sum(lens) * dt,
    dur_longest = max(lens) * dt
  )
}

# Trapezoidal area under the curve, ignoring NA frames
auc_trapz <- function(v, dt) {
  ok <- !is.na(v)
  if (sum(ok) < 2) return(NA_real_)
  v <- v[ok]
  sum((head(v, -1) + tail(v, -1)) / 2) * dt
}


# -----------------------------------------------------------------------------
# 3. PARSE ONE FILE
# -----------------------------------------------------------------------------

parse_facereader_file <- function(path) {

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("\r$", "", lines)

  fname_field <- header_value(lines, "Filename")
  frame_rate  <- suppressWarnings(as.numeric(header_value(lines, "Frame rate")))
  start_time  <- header_value(lines, "Start time")
  face_model  <- header_value(lines, "Face Model")

  hdr_idx <- grep("^Video Time\t", lines)

  # --- classify the export type -------------------------------------------
  if (!length(hdr_idx)) {
    return(list(status = "no_data_table", meta = NULL, frames = NULL))
  }
  hdr <- strsplit(lines[hdr_idx[1]], "\t", fixed = TRUE)[[1]]
  hdr <- trimws(hdr)

  if (identical(hdr, c("Video Time", "Dominant Expression"))) {
    # This is a "state log" export: dominant expression transitions only.
    # It contains no emotion probabilities and no action units, so it cannot
    # be used for peak intensity or duration. Flagged for re-export.
    return(list(status = "state_log_only", meta = NULL, frames = NULL))
  }

  # --- read the data table -------------------------------------------------
  body <- lines[(hdr_idx[1] + 1):length(lines)]
  body <- body[nzchar(trimws(body))]
  if (!length(body)) {
    return(list(status = "empty_table", meta = NULL, frames = NULL))
  }

  cells <- strsplit(body, "\t", fixed = TRUE)
  ncol_expected <- length(hdr)
  cells <- lapply(cells, function(r) {
    length(r) <- ncol_expected   # pads with NA / truncates
    r
  })
  m <- do.call(rbind, cells)
  colnames(m) <- hdr

  # --- metadata from the internal source filename --------------------------
  # e.g. "Happy 7 CN.mp4_converted_E17HA07-02_noback.mp4"
  src_base   <- basename(gsub("\\\\", "/", fname_field))
  src_folder <- basename(dirname(gsub("\\\\", "/", fname_field)))   # e.g. "C17_noback"

  stimulus_label <- sub("\\.mp4.*$", "", src_base)                  # "Happy 7 CN"
  clip_id <- sub("^.*_converted_([A-Za-z0-9-]+)_noback.*$", "\\1", src_base)
  if (identical(clip_id, src_base)) clip_id <- NA_character_

  culture_code <- stim_code <- NA_character_
  cohort_num <- stim_num <- take_num <- NA_integer_
  if (!is.na(clip_id) && grepl("^[A-Za-z][0-9]+(HA|SA)[0-9]+-[0-9]+$", clip_id)) {
    culture_code <- toupper(sub("^([A-Za-z]).*$", "\\1", clip_id))
    cohort_num   <- as.integer(sub("^[A-Za-z]([0-9]+).*$", "\\1", clip_id))
    stim_code    <- sub("^[A-Za-z][0-9]+(HA|SA).*$", "\\1", clip_id)
    stim_num     <- as.integer(sub("^[A-Za-z][0-9]+(HA|SA)([0-9]+)-.*$", "\\2", clip_id))
    take_num     <- as.integer(sub("^.*-([0-9]+)$", "\\1", clip_id))
  }

  # --- integrity check ------------------------------------------------------
  # The source video name (e.g. "Happy 7 CN") and the clip code (e.g. HA / SA)
  # are two independent records of which stimulus was watched. They should
  # agree. Where they do not, the clip's condition is genuinely ambiguous, so
  # we flag it and refuse to assign a smile_type rather than guessing.
  stim_from_label <- NA_character_
  if (grepl("^Happy", stimulus_label)) stim_from_label <- "HA"
  if (grepl("^Sad",   stimulus_label)) stim_from_label <- "SA"

  label_conflict <- as.integer(
    !is.na(stim_from_label) && !is.na(stim_code) && stim_from_label != stim_code
  )

  smile_type <- if (isTRUE(label_conflict == 1)) NA_character_
                else unname(SMILE_TYPE_MAP[stim_code])

  meta <- data.frame(
    clip_id         = clip_id,
    smiler_id       = if (!is.na(cohort_num)) sprintf("C%02d", cohort_num) else NA_character_,
    culture         = unname(CULTURE_MAP[culture_code]),
    smile_type      = smile_type,
    stim_from_label = stim_from_label,
    stim_from_code  = stim_code,
    label_conflict  = label_conflict,
    stimulus        = stimulus_label,
    stimulus_num    = stim_num,
    take_num        = take_num,
    source_folder   = src_folder,
    export_file     = basename(path),
    face_model      = face_model,
    start_time      = start_time,
    frame_rate      = frame_rate,
    stringsAsFactors = FALSE
  )

  list(status = "ok", meta = meta, matrix = m, header = hdr, frame_rate = frame_rate)
}


# -----------------------------------------------------------------------------
# 4. LOOP OVER ALL FILES
# -----------------------------------------------------------------------------

files <- sort(list.files(raw_dir, pattern = "\\.txt$", full.names = TRUE))
if (!length(files)) stop("No .txt files found in ", raw_dir, call. = FALSE)
message("\nFound ", length(files), " files. Parsing...")

manifest_rows <- list()
frames_list   <- list()
summary_list  <- list()
au_dictionary <- NULL

for (f in files) {

  res <- parse_facereader_file(f)

  if (res$status != "ok") {
    manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
      export_file = basename(f), status = res$status, clip_id = NA_character_,
      smiler_id = NA_character_, culture = NA_character_, smile_type = NA_character_,
      label_conflict = NA_integer_, stim_from_label = NA_character_,
      stim_from_code = NA_character_,
      stimulus = NA_character_, n_frames = NA_integer_, duration_s = NA_real_,
      frame_rate = NA_real_, n_fitfail_cells = NA_integer_,
      stringsAsFactors = FALSE
    )
    next
  }

  m    <- res$matrix
  hdr  <- res$header
  meta <- res$meta
  fps  <- res$frame_rate
  dt   <- 1 / fps

  time_s   <- parse_video_time(m[, "Video Time"])
  n_frames <- nrow(m)

  emo_cols <- hdr[hdr %in% c("Neutral", "Happy", "Sad", "Angry",
                             "Surprised", "Scared", "Disgusted", "Contempt")]
  au_cols  <- hdr[grepl("^Action Unit", hdr)]

  n_fitfail <- sum(m[, c(emo_cols, au_cols), drop = FALSE] == "FIT_FAILED", na.rm = TRUE)

  # --- build the AU dictionary once ---------------------------------------
  if (is.null(au_dictionary)) {
    d <- lapply(au_cols, split_au_name)
    au_dictionary <- data.frame(
      channel      = vapply(d, `[[`, "", "code"),
      channel_type = "action_unit",
      full_label   = vapply(d, `[[`, "", "label"),
      source_column = au_cols,
      stringsAsFactors = FALSE
    )
    au_dictionary <- rbind(
      au_dictionary,
      data.frame(channel = emo_cols, channel_type = "emotion",
                 full_label = emo_cols, source_column = emo_cols,
                 stringsAsFactors = FALSE)
    )
  }

  # --- frame-level wide table ---------------------------------------------
  fr <- data.frame(
    clip_id    = meta$clip_id,
    smiler_id  = meta$smiler_id,
    culture    = meta$culture,
    smile_type = meta$smile_type,
    stimulus   = meta$stimulus,
    frame      = seq_len(n_frames),
    time_s     = time_s,
    stringsAsFactors = FALSE
  )
  for (cn in emo_cols) fr[[cn]] <- recode_emotion(m[, cn])
  for (cn in au_cols) {
    code <- split_au_name(cn)$code
    fr[[code]] <- recode_au(m[, cn])
  }
  frames_list[[length(frames_list) + 1]] <- fr

  # --- per-channel clip summary -------------------------------------------
  channels <- c(
    lapply(emo_cols, function(cn) list(name = cn, type = "emotion",
                                       v = recode_emotion(m[, cn]),
                                       thr = EMO_ACTIVE_MIN)),
    lapply(au_cols, function(cn) list(name = split_au_name(cn)$code, type = "action_unit",
                                      v = recode_au(m[, cn]),
                                      thr = AU_ACTIVE_MIN))
  )

  ch_rows <- lapply(channels, function(ch) {
    v   <- ch$v
    ok  <- !is.na(v)
    act <- v >= ch$thr
    ep  <- episode_stats(act, dt)

    if (any(ok)) {
      pk      <- max(v[ok])
      t_peak  <- time_s[which(ok & v == pk)[1]]
      mn      <- mean(v[ok])
      sdv     <- if (sum(ok) > 1) sd(v[ok]) else NA_real_
      prop_ac <- mean(act[ok])
    } else {
      pk <- t_peak <- mn <- sdv <- prop_ac <- NA_real_
    }

    data.frame(
      clip_id           = meta$clip_id,
      smiler_id         = meta$smiler_id,
      culture           = meta$culture,
      smile_type        = meta$smile_type,
      label_conflict    = meta$label_conflict,
      stimulus          = meta$stimulus,
      stimulus_num      = meta$stimulus_num,
      take_num          = meta$take_num,
      clip_duration_s   = n_frames * dt,
      n_frames          = n_frames,
      channel           = ch$name,
      channel_type      = ch$type,
      peak              = pk,
      mean_intensity    = mn,
      sd_intensity      = sdv,
      auc               = auc_trapz(v, dt),
      prop_active       = prop_ac,
      n_episodes        = ep$n_episodes,
      dur_total_s       = ep$dur_total,
      dur_longest_s     = ep$dur_longest,
      t_peak_s          = t_peak,
      t_peak_prop       = t_peak / (n_frames * dt),
      t_peak_usable     = as.integer(!isTRUE(act[1])),
      censored_start    = as.integer(isTRUE(act[1])),
      censored_end      = as.integer(isTRUE(act[n_frames])),
      n_valid_frames    = sum(ok),
      n_missing_frames  = sum(!ok),
      stringsAsFactors  = FALSE
    )
  })
  summary_list[[length(summary_list) + 1]] <- do.call(rbind, ch_rows)

  # --- manifest row --------------------------------------------------------
  manifest_rows[[length(manifest_rows) + 1]] <- data.frame(
    export_file     = basename(f),
    status          = "ok",
    clip_id         = meta$clip_id,
    smiler_id       = meta$smiler_id,
    culture         = meta$culture,
    smile_type      = meta$smile_type,
    label_conflict  = meta$label_conflict,
    stim_from_label = meta$stim_from_label,
    stim_from_code  = meta$stim_from_code,
    stimulus        = meta$stimulus,
    n_frames        = n_frames,
    duration_s      = n_frames * dt,
    frame_rate      = fps,
    n_fitfail_cells = n_fitfail,
    stringsAsFactors = FALSE
  )
}

manifest     <- do.call(rbind, manifest_rows)
frames_wide  <- do.call(rbind, frames_list)
clip_long    <- do.call(rbind, summary_list)

message("Parsed ", sum(manifest$status == "ok"), " usable clips of ",
        nrow(manifest), " files.")


# -----------------------------------------------------------------------------
# 5. WIDE (SPSS-FRIENDLY) CLIP SUMMARY
# -----------------------------------------------------------------------------
# One row per clip. Variable names are kept short and space-free so they
# survive import into SPSS.

key_metrics <- c("peak", "mean_intensity", "prop_active",
                 "dur_longest_s", "dur_total_s", "t_peak_s", "auc",
                 "n_episodes", "censored_start", "censored_end")

short_name <- c(peak = "pk", mean_intensity = "mn", prop_active = "prop",
                dur_longest_s = "durmax", dur_total_s = "durtot",
                t_peak_s = "tpk", auc = "auc", n_episodes = "neps",
                censored_start = "cens0", censored_end = "cens1")

id_cols <- c("clip_id", "smiler_id", "culture", "smile_type", "label_conflict",
             "stimulus", "stimulus_num", "take_num", "clip_duration_s", "n_frames")

clip_wide <- unique(clip_long[, id_cols])
clip_wide <- clip_wide[order(clip_wide$smiler_id, clip_wide$stimulus_num), ]

for (ch in unique(clip_long$channel)) {
  sub <- clip_long[clip_long$channel == ch, ]
  for (mt in key_metrics) {
    newcol <- paste0(ch, "_", short_name[[mt]])
    clip_wide[[newcol]] <- sub[[mt]][match(clip_wide$clip_id, sub$clip_id)]
  }
}


# -----------------------------------------------------------------------------
# 6. WRITE OUTPUTS
# -----------------------------------------------------------------------------

w <- function(obj, name) {
  p <- file.path(out_dir, name)
  write.csv(obj, p, row.names = FALSE, na = "")
  message("  wrote ", name, "  (", nrow(obj), " rows)")
}

message("\nWriting outputs...")
w(manifest,      "01_file_manifest.csv")
w(frames_wide,   "02_frames_wide.csv")
w(clip_long,     "03_clip_summary_long.csv")
w(clip_wide,     "04_clip_summary_wide.csv")
w(au_dictionary, "05_channel_dictionary.csv")


# -----------------------------------------------------------------------------
# 7. DATA QUALITY REPORT
# -----------------------------------------------------------------------------

rep_path <- file.path(out_dir, "06_data_quality_report.txt")
con <- file(rep_path, open = "wt")
p <- function(...) cat(..., "\n", sep = "", file = con)

p("FACEREADER EXTRACTION - DATA QUALITY REPORT")
p("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
p("Source   : ", raw_dir)
p(strrep("=", 74))

p("\n1. FILE INVENTORY")
p(strrep("-", 74))
st <- table(manifest$status)
for (i in seq_along(st)) p(sprintf("  %-20s %4d", names(st)[i], st[i]))
p(sprintf("  %-20s %4d", "TOTAL", nrow(manifest)))

bad <- manifest[manifest$status != "ok", ]
if (nrow(bad)) {
  p("\n  Files that could NOT be used (need re-export from FaceReader as a")
  p("  DETAILED log - the current export contains only dominant-expression")
  p("  transitions, with no emotion values and no action units):")
  for (i in seq_len(nrow(bad))) p("    - ", bad$export_file[i])
}

ok <- manifest[manifest$status == "ok", ]

p("\n2. DESIGN COVERAGE (usable clips)")
p(strrep("-", 74))
p("  Cells present:")
cells <- table(ok$culture, ifelse(is.na(ok$smile_type), "UNRESOLVED", ok$smile_type))
print_tab <- capture.output(print(cells))
for (l in print_tab) p("    ", l)
p("\n  Smilers: ", length(unique(ok$smiler_id)),
  "  (", paste(sort(unique(ok$smiler_id)), collapse = ", "), ")")
p("\n  Clips per smiler:")
cps <- table(ok$smiler_id)
for (i in seq_along(cps)) p(sprintf("    %-6s %2d", names(cps)[i], cps[i]))
p("\n  Clips per stimulus:")
cst <- table(ok$stimulus)
for (i in seq_along(cst)) p(sprintf("    %-14s %2d", names(cst)[i], cst[i]))

p("\n2b. STIMULUS LABEL INTEGRITY  ** ACTION REQUIRED **")
p(strrep("-", 74))
p("  Each clip records the stimulus twice: once as the source video name")
p("  (e.g. 'Happy 7 CN') and once inside the clip code (HA / SA). These")
p("  should always agree. Where they disagree the condition is ambiguous,")
p("  so smile_type is left BLANK and the clip is excluded from the")
p("  condition summaries below. It must not be analysed until resolved.")
conf <- ok[!is.na(ok$label_conflict) & ok$label_conflict == 1, ]
if (nrow(conf)) {
  p("\n  Conflicting clips: ", nrow(conf))
  for (i in seq_len(nrow(conf))) {
    p("    clip code ", conf$clip_id[i], "  says ", conf$stim_from_code[i],
      "  but source video is '", conf$stimulus[i], "' (", conf$stim_from_label[i], ")")
    p("      export file: ", conf$export_file[i])
  }
  p("\n  -> Ask Ted which is correct: was this smile recorded against a")
  p("     humorous or a sad video? One of the two labels is a typo.")
} else {
  p("\n  No conflicts found.")
}

p("\n3. TEMPORAL PROPERTIES")
p(strrep("-", 74))
p("  Frame rate(s) found: ", paste(unique(ok$frame_rate), collapse = ", "), " fps")
p("  NOTE: a 30 fps source was expected. At ", unique(ok$frame_rate)[1],
  " fps one frame = ",
  sprintf("%.0f", 1000 / unique(ok$frame_rate)[1]), " ms,")
p("  which is the floor on any duration or onset/offset measurement.")
p(sprintf("\n  Clip duration (s):  min %.2f   median %.2f   mean %.2f   max %.2f",
          min(ok$duration_s), median(ok$duration_s),
          mean(ok$duration_s), max(ok$duration_s)))
p("  Clip lengths are unequal, so any CUMULATIVE measure (dur_total_s, auc)")
p("  is partly a measure of clip length. Prefer peak / mean / prop_active,")
p("  or include clip_duration_s as a covariate.")

p("\n4. MISSING DATA (FIT_FAILED cells)")
p(strrep("-", 74))
p("  Total FIT_FAILED cells: ", sum(ok$n_fitfail_cells))
ff <- ok[ok$n_fitfail_cells > 0, c("export_file", "n_fitfail_cells")]
if (nrow(ff)) {
  p("  Affected files:")
  for (i in seq_len(nrow(ff)))
    p(sprintf("    %-62s %3d", substr(ff$export_file[i], 1, 62), ff$n_fitfail_cells[i]))
} else p("  None.")
p("  These are recoded to NA, never to zero.")

p("\n5. CENSORING - action units already on at clip start / still on at clip end")
p(strrep("-", 74))
p("  This is the issue Ted raised. An AU that is already active in frame 1")
p("  has an unknown onset; one still active in the final frame has an unknown")
p("  offset. Percentages below are of usable clips (n = ", nrow(ok), ").")
p("")
p(sprintf("  %-6s %-24s %11s %11s", "AU", "Muscle", "start-cens", "end-cens"))
aus <- au_dictionary[au_dictionary$channel_type == "action_unit", ]
for (i in seq_len(nrow(aus))) {
  s <- clip_long[clip_long$channel == aus$channel[i], ]
  p(sprintf("  %-6s %-24s %6d %4.0f%% %6d %4.0f%%",
            aus$channel[i], substr(aus$full_label[i], 1, 24),
            sum(s$censored_start), 100 * mean(s$censored_start),
            sum(s$censored_end),   100 * mean(s$censored_end)))
}
p("")
p("  Implication: peak intensity, mean intensity and prop_active are safe.")
p("  Raw onset/offset times and durations are NOT, for any AU with a high")
p("  censoring rate - those durations are lower bounds, not true values.")

p("\n6. AU12 (Lip Corner Puller) - the smile marker, by smile type")
p(strrep("-", 74))
s12 <- clip_long[clip_long$channel == "AU12" & !is.na(clip_long$smile_type), ]
p("  (clips with a label conflict are excluded)")
for (ty in unique(s12$smile_type)) {
  z <- s12[s12$smile_type == ty, ]
  p(sprintf("  %-9s n = %2d | peak %.2f (SD %.2f) | prop active %.2f | longest bout %.2fs",
            ty, nrow(z), mean(z$peak, na.rm = TRUE), sd(z$peak, na.rm = TRUE),
            mean(z$prop_active, na.rm = TRUE), mean(z$dur_longest_s, na.rm = TRUE)))
}
p("\n  AU06 (Cheek Raiser) - the Duchenne marker:")
s06 <- clip_long[clip_long$channel == "AU06" & !is.na(clip_long$smile_type), ]
for (ty in unique(s06$smile_type)) {
  z <- s06[s06$smile_type == ty, ]
  p(sprintf("  %-9s n = %2d | peak %.2f (SD %.2f) | prop active %.2f | longest bout %.2fs",
            ty, nrow(z), mean(z$peak, na.rm = TRUE), sd(z$peak, na.rm = TRUE),
            mean(z$prop_active, na.rm = TRUE), mean(z$dur_longest_s, na.rm = TRUE)))
}

p("\n7. CODING DECISIONS APPLIED")
p(strrep("-", 74))
p("  AU intensity : NotActive=0, A=1, B=2, C=3, D=4, E=5, FIT_FAILED=NA")
p("  AU active    : intensity >= ", AU_ACTIVE_MIN,
  " (", c("A","B","C","D","E")[AU_ACTIVE_MIN], " and above)")
p("  Emotion active: probability >= ", EMO_ACTIVE_MIN)
p("  Smile type   : HA stimulus = Genuine, SA stimulus = Fake")
p("  Smiler ID    : taken from the INTERNAL clip code (e.g. E17HA07-02 -> C17),")
p("                 NOT from the Participant_### numbers in the export")
p("                 filename, which are upload IDs and do not identify a person.")
p("  Stimulus     : also taken from the internal clip code. The chi_hap_N")
p("                 number in the export filename does not always match the")
p("                 stimulus (e.g. C24 chi_hap_4 is actually Happy 5 CN).")

p("\n", strrep("=", 74))
p("END OF REPORT")
close(con)
message("  wrote 06_data_quality_report.txt")

message("\nDone. ", length(list.files(out_dir)), " files in ", out_dir)

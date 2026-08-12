# =============================================================================
# 00_config.R
#
# Shared settings for the smile project. Every other script sources this,
# so paths and coding rules are defined in ONE place only.
#
# Usage, at the top of any script:
#     source("r_scripts/00_config.R")
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. WHERE THE RAW DATA LIVES
# -----------------------------------------------------------------------------
# The raw FaceReader exports now sit on the University research data server,
# NOT inside the project folder. Only scripts 00 and 01 need this volume;
# 02, 03 and the report run entirely from local results_outputs/.

RAW_DIR <- "/Volumes/PRJ-smile/raw_data_all"

# If you ever work from a local copy, point this at it instead, e.g.
# RAW_DIR <- "~/Documents/smile_backup/raw_data_all"


# -----------------------------------------------------------------------------
# 2. WHERE THE PROJECT LIVES
# -----------------------------------------------------------------------------
# Found by looking for r_scripts/, walking up from the working directory.
# This works whether you run from the project root, from r_scripts/, or
# from report/.

# A real project folder has r_scripts AND at least one of results_outputs,
# report or .git. Requiring two markers stops the search latching onto a
# stray copy - an old r_scripts in the Trash, for instance.
looks_like_project <- function(p) {
  if (!dir.exists(file.path(p, "r_scripts"))) return(FALSE)
  sum(c(dir.exists(file.path(p, "results_outputs")),
        dir.exists(file.path(p, "report")),
        dir.exists(file.path(p, ".git")))) >= 1
}

find_project_root <- function() {
  here <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  # Walk UP only. Searching downward once matched a deleted copy in the
  # Trash, so it is not done any more.
  p <- here
  for (i in 1:6) {
    if (looks_like_project(p)) return(p)
    p <- dirname(p)
  }

  stop("\n\n  Cannot find the project folder.\n",
       "    R is currently in: ", here, "\n\n",
       "  Set it explicitly, e.g.\n",
       '       setwd("~/Documents/Research projects /smile_project/smile_project_github/Smile_Project")\n\n',
       "  Or better, open the project once via RStudio:\n",
       "     File > New Project > Existing Directory\n",
       "  which creates a .Rproj so the directory is always correct.\n",
       call. = FALSE)
}

PROJECT_ROOT <- find_project_root()
# PROJECT_ROOT <- "/Users/delphine/.../Smile_Project"   # uncomment to force

if (grepl("/\\.Trash(/|$)|/\\.Trashes(/|$)", PROJECT_ROOT))
  stop("\n\n  The project folder resolved to the Trash:\n    ", PROJECT_ROOT,
       "\n\n  Set the working directory to the real project and try again.\n",
       call. = FALSE)

OUT_DIR    <- file.path(PROJECT_ROOT, "results_outputs")
FIG_DIR    <- file.path(OUT_DIR, "figures")
REPORT_DIR <- file.path(PROJECT_ROOT, "report")

for (d in c(OUT_DIR, FIG_DIR)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)


# -----------------------------------------------------------------------------
# 3. CHECK THE SERVER IS MOUNTED
# -----------------------------------------------------------------------------
# Call require_raw() at the top of any script that reads raw data. It fails
# with a useful message rather than an empty file list.

require_raw <- function() {
  if (!dir.exists(RAW_DIR)) {
    vol <- dirname(RAW_DIR)
    stop("\n\n  Cannot reach the raw data.\n",
         "    Expected at: ", RAW_DIR, "\n\n",
         "  The research drive is probably not mounted.\n",
         "    - In Finder: Go > Connect to Server, mount PRJ-smile\n",
         "    - Check the volume exists: ", vol, "\n",
         "    - Then run this script again.\n\n",
         "  Scripts 02 and 03 do NOT need the drive - they read\n",
         "  results_outputs/ locally.\n", call. = FALSE)
  }
  n <- length(list.files(RAW_DIR, pattern = "\\.txt$", recursive = TRUE))
  if (n == 0) stop("Found ", RAW_DIR, " but it contains no .txt files.", call. = FALSE)
  invisible(n)
}


# -----------------------------------------------------------------------------
# 4. CODING RULES
# -----------------------------------------------------------------------------

AU_ACTIVE_MIN  <- 1     # action unit counts as active at grade A and above
EMO_ACTIVE_MIN <- 0.5   # emotion counts as present at probability >= 0.5

SMILE_TYPE_MAP <- c(HA = "Genuine", SA = "Performed")
CULTURE_MAP    <- c(E = "Chinese",  W = "European")


# -----------------------------------------------------------------------------
# 5. ANALYSIS DECISIONS  (agreed with Ted and Zoe, meeting of 11 Aug 2026)
# -----------------------------------------------------------------------------

# Lead outcome. duchenne_prop is scale-free, unaffected by clip length, and
# captures the co-occurrence the Duchenne hypothesis is actually about.
PRIMARY_OUTCOME <- "duchenne_prop"

# Secondary outcomes. Peak intensity is included but DEMOTED: Zoe noted the
# two conditions are not balanced on intensity by design, since the smiles
# were naturally elicited, so absolute intensity should not be a headline
# index.
SECONDARY_OUTCOMES <- c("AU06_prop", "AU12_prop", "AU06_pk", "AU12_pk", "Happy_pk")

# Stimulus is NOT a random effect. Ted: each stimulus video ("Happy 7" etc.)
# contains several separate funny moments, so two people smiling at the same
# video are not responding to the same thing. Stimulus identity is therefore
# not a meaningful grouping factor.
USE_STIMULUS_RANDOM_EFFECT <- FALSE

# Clips with no AU12 activation are RETAINED. Zoe: the smiles were elicited
# by happy clips and the expressors rated their own state as positive, so a
# low-movement smile is still a smile. Excluding them would cost ecological
# validity.
EXCLUDE_SMILELESS_CLIPS <- FALSE

# Clip duration enters as a covariate only for outcomes that are not
# scale-free. Proportions are exempt.
DURATION_COVARIATE_SUFFIXES <- c("_pk", "_tpk", "_durtot", "_durmax", "_auc", "_neps")

ALPHA     <- 0.05
FDR_ALPHA <- 0.05


# -----------------------------------------------------------------------------
# 6. SHARED HELPERS
# -----------------------------------------------------------------------------

# "00:01:23.375" -> 83.375
parse_video_time <- function(x) {
  p <- do.call(rbind, strsplit(trimws(x), ":", fixed = TRUE))
  as.numeric(p[, 1]) * 3600 + as.numeric(p[, 2]) * 60 + as.numeric(p[, 3])
}

header_value <- function(lines, key) {
  h <- grep(paste0("^", key, "\t"), lines)
  if (!length(h)) return(NA_character_)
  trimws(sub(paste0("^", key, "\t"), "", lines[h[1]]))
}

# Grades to numbers. FIT_FAILED and anything unrecognised become NA,
# never zero: "not moving" and "could not measure" are different things.
recode_au <- function(x) {
  x <- trimws(x); out <- rep(NA_real_, length(x))
  out[x == "NotActive"] <- 0; out[x == "A"] <- 1; out[x == "B"] <- 2
  out[x == "C"] <- 3;         out[x == "D"] <- 4; out[x == "E"] <- 5
  out
}

recode_emotion <- function(x) suppressWarnings(as.numeric(trimws(x)))

split_au_name <- function(nm) {
  list(code  = paste0("AU", sub("^Action Unit ([0-9]+).*$", "\\1", nm)),
       label = trimws(sub("^Action Unit [0-9]+ *- *", "", nm)))
}

# Count runs of activity: how many separate bursts, how long in total,
# and how long the longest one lasted. NA is treated as not-active here.
episode_stats <- function(active, dt) {
  a <- active; a[is.na(a)] <- FALSE
  if (!any(a)) return(list(n_episodes = 0L, dur_total = 0, dur_longest = 0))
  r <- rle(a); lens <- r$lengths[r$values]
  list(n_episodes = length(lens), dur_total = sum(lens) * dt,
       dur_longest = max(lens) * dt)
}

# Trapezoidal area under the curve, skipping missing frames
auc_trapz <- function(v, dt) {
  ok <- !is.na(v); if (sum(ok) < 2) return(NA_real_)
  v <- v[ok]; sum((head(v, -1) + tail(v, -1)) / 2) * dt
}

# Pull the clip code out of the source video name recorded inside the file
extract_clip_code <- function(source_filename) {
  b <- basename(gsub("\\\\", "/", source_filename))
  m <- regmatches(b, regexpr("_converted_[A-Za-z0-9-]+_noback", b))
  if (length(m)) sub("_converted_(.*)_noback", "\\1", m) else NA_character_
}

# Colours, matching the report banner
USYD  <- "#E64626"; SLATE <- "#37474F"
LIGHT <- "#CFD8DC"; MID   <- "#78909C"

# Warn if an old local copy of the raw data still sits in the project: it is
# easy to analyse the wrong one by accident.
.local_raw <- file.path(PROJECT_ROOT, c("raw_data_all", "raw_data_email"))
.local_raw <- .local_raw[dir.exists(.local_raw)]
if (length(.local_raw)) {
  message("\nNOTE: raw data folders also exist inside the project:")
  for (d in .local_raw)
    message("  ", basename(d), "  (", length(list.files(d, pattern = "\\.txt$",
            recursive = TRUE)), " .txt files)")
  message("  The scripts read RAW_DIR only: ", RAW_DIR)
  message("  Consider archiving or removing the local copies to avoid confusion.\n")
}

message("Config loaded.")
message("  project : ", PROJECT_ROOT)
message("  raw data: ", RAW_DIR, if (dir.exists(RAW_DIR)) "  [mounted]" else "  [NOT MOUNTED]")

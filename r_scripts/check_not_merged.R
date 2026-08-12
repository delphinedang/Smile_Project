# =============================================================================
# Check that two DIFFERENT smiles from the same person are kept separate.
#
# The first check (a duplicate pair) already returned TRUE. This is the
# opposite test: two real, different smiles should return FALSE.
#
# Paste into the R console.
# =============================================================================

RAW <- "/Volumes/PRJ-smile/raw_data_all"

# ---- find the files for two clips by the same person ------------------------
# E11SA02-03 and E11SA02-04: same person, same video (Sad 2), two takes.
# If anything were going to be wrongly merged, it would be these.

find_clip <- function(code) {
  files <- list.files(RAW, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
  hit <- character(0)
  for (f in files) {
    h <- readLines(f, n = 10, warn = FALSE)
    line <- grep("^Filename\t", h, value = TRUE)
    if (length(line) && grepl(paste0("_converted_", code, "_noback"), line, fixed = TRUE)) {
      hit <- c(hit, f)
    }
  }
  hit
}

f1 <- find_clip("E11SA02-03")
f2 <- find_clip("E11SA02-04")

cat("\nE11SA02-03 found in", length(f1), "file(s)\n")
cat("E11SA02-04 found in", length(f2), "file(s)\n")
cat("(2 each is expected - every clip is stored twice)\n")


# ---- compare them -----------------------------------------------------------
x <- readLines(f1[1])
y <- readLines(f2[1])

cat("\n--- two DIFFERENT smiles from the same person ---\n")
cat("  identical?      ", identical(x, y), "   <- should be FALSE\n")
cat("  lines in each   ", length(x), "vs", length(y), "\n")


# ---- and confirm the duplicate pair, for contrast ---------------------------
if (length(f1) > 1) {
  cat("\n--- the same clip, stored in two folders ---\n")
  cat("  ", dirname(f1[1]), "\n")
  cat("  ", dirname(f1[2]), "\n")
  cat("  identical?      ", identical(readLines(f1[1]), readLines(f1[2])),
      "   <- should be TRUE\n")
}

cat("\nIf you get FALSE then TRUE, deduplication is working correctly:\n")
cat("different smiles are kept, exact copies are dropped.\n")

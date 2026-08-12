# =============================================================================
# 00b_diagnose_duplicates.R
#
# READ-ONLY. Run this BEFORE any analysis.
#
# The inspection report suggests the four condition folders contain
# overlapping copies of the same clips. This script settles it by comparing
# actual file CONTENT, not just names, and works out how many unique clips
# we really have.
#
# Output: results_outputs/00b_duplicate_report.txt
#         results_outputs/00b_unique_clip_list.csv
#
# Base R only.
# =============================================================================

find_root <- function() {
  p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 1:4) {
    if (dir.exists(file.path(p, "raw_data_all"))) return(p)
    p <- dirname(p)
  }
  stop("Could not find 'raw_data_all'.", call. = FALSE)
}
project_root <- find_root()
raw_dir <- file.path(project_root, "raw_data_all")
out_dir <- file.path(project_root, "results_outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

con <- file(file.path(out_dir, "00b_duplicate_report.txt"), open = "wt")
say <- function(...) { t <- paste0(..., collapse = "")
                       cat(t, "\n", sep = ""); cat(t, "\n", sep = "", file = con) }

say("DUPLICATE DIAGNOSIS")
say("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say(strrep("=", 74))

files <- list.files(raw_dir, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
say(sprintf("\nFiles scanned: %d", length(files)))

# ---- read identifying info + a content fingerprint --------------------------
say("\nReading files and fingerprinting content...")

rec <- lapply(files, function(f) {
  lines <- readLines(f, warn = FALSE)
  lines <- sub("\r$", "", lines)

  hv <- function(k) { h <- grep(paste0("^", k, "\t"), lines)
                      if (length(h)) trimws(sub(paste0("^", k, "\t"), "", lines[h[1]])) else NA_character_ }
  src <- hv("Filename")
  b   <- basename(gsub("\\\\", "/", src))
  m   <- regmatches(b, regexpr("_converted_[A-Za-z0-9-]+_noback", b))
  code <- if (length(m)) sub("_converted_(.*)_noback", "\\1", m) else NA_character_

  hdr_i <- grep("^Video Time\t", lines)
  body  <- if (length(hdr_i)) lines[(hdr_i[1] + 1):length(lines)] else character(0)
  body  <- body[nzchar(trimws(body))]

  # fingerprint = the data rows themselves. Two files with the same
  # fingerprint contain identical measurements.
  # Fingerprint from the data rows only, NOT the header, because the same
  # clip copied into two folders can carry a different path in its header.
  # Three independent parts, so accidental collisions are effectively impossible.
  txt <- paste(body, collapse = "")
  iv  <- utf8ToInt(txt)
  fp  <- paste(length(body), nchar(txt),
               sum(as.numeric(iv)),
               sum(as.numeric(iv) * seq_along(iv)) %% 1e15, sep = "_")

  rel <- sub(paste0(raw_dir, "/"), "", f, fixed = TRUE)
  data.frame(
    folder      = strsplit(rel, "/")[[1]][1],
    file        = basename(f),
    clip_code   = code,
    source_video= sub("\\.mp4.*$", "", b),
    n_frames    = length(body),
    fingerprint = fp,
    stringsAsFactors = FALSE)
})
d <- do.call(rbind, rec)

# ---- 1. duplicate CONTENT ---------------------------------------------------
say("\n1. IDENTICAL CONTENT")
say(strrep("-", 74))
fp_n <- table(d$fingerprint)
say(sprintf("  Total files                 : %d", nrow(d)))
say(sprintf("  Distinct content signatures : %d", length(fp_n)))
say(sprintf("  Files that are copies        : %d", nrow(d) - length(fp_n)))
say("")
if (any(fp_n > 1)) {
  say(sprintf("  ** %d clips appear more than once **", sum(fp_n > 1)))
  say("  Copies per clip:")
  for (l in capture.output(print(table(as.integer(fp_n))))) say("    ", l)
} else say("  No duplicate content found.")

# ---- 2. where do the copies live? -------------------------------------------
say("\n2. WHICH FOLDERS DO THE COPIES SPAN?")
say(strrep("-", 74))
dup_fp <- names(fp_n)[fp_n > 1]
if (length(dup_fp)) {
  span <- sapply(dup_fp, function(x) {
    paste(sort(unique(d$folder[d$fingerprint == x])), collapse = "  +  ")
  })
  for (l in capture.output(print(sort(table(span), decreasing = TRUE)))) say("  ", l)
  say("")
  say("  First 10 duplicated clips:")
  say(sprintf("  %-14s %-42s %s", "clip code", "folders", "n"))
  for (x in head(dup_fp, 10)) {
    sub <- d[d$fingerprint == x, ]
    say(sprintf("  %-14s %-42s %d",
                sub$clip_code[1],
                paste(sort(unique(sub$folder)), collapse = " + "),
                nrow(sub)))
  }
}

# ---- 3. duplicate clip CODES (may differ in content) ------------------------
say("\n3. REPEATED CLIP CODES")
say(strrep("-", 74))
cc <- table(d$clip_code[!is.na(d$clip_code)])
say(sprintf("  Distinct clip codes: %d", length(cc)))
say(sprintf("  Codes used more than once: %d", sum(cc > 1)))
if (any(cc > 1)) {
  say("\n  Do repeated codes always have identical content?")
  mixed <- 0
  for (code in names(cc)[cc > 1]) {
    if (length(unique(d$fingerprint[which(d$clip_code == code)])) > 1) mixed <- mixed + 1
  }
  say(sprintf("    same content  : %d codes", sum(cc > 1) - mixed))
  say(sprintf("    DIFFERENT content: %d codes  <- these are a real problem", mixed))
  if (mixed > 0) {
    say("\n  Codes reused for DIFFERENT clips (first 10):")
    n <- 0
    for (code in names(cc)[cc > 1]) {
      if (length(unique(d$fingerprint[which(d$clip_code == code)])) > 1 && n < 10) {
        sub <- d[which(d$clip_code == code), ]
        say("    ", code)
        for (i in seq_len(nrow(sub)))
          say(sprintf("       %-26s %-24s %d frames", sub$folder[i],
                      sub$source_video[i], sub$n_frames[i]))
        n <- n + 1
      }
    }
  }
}

# ---- 4. what is the TRUE dataset? -------------------------------------------
say("\n4. THE UNIQUE DATASET")
say(strrep("-", 74))
u <- d[!duplicated(d$fingerprint), ]
u$culture <- ifelse(grepl("^E", u$clip_code), "Chinese",
             ifelse(grepl("^W", u$clip_code), "European", NA))
u$type <- ifelse(grepl("HA", u$clip_code), "Genuine",
          ifelse(grepl("SA", u$clip_code), "Fake", NA))
u$smiler <- sub("^([A-Za-z]+[0-9]+).*$", "\\1", u$clip_code)

say(sprintf("  Unique clips: %d  (from %d files)", nrow(u), nrow(d)))
say("\n  Design coverage, counting each clip ONCE:")
for (l in capture.output(print(table(u$culture, u$type, useNA = "ifany")))) say("    ", l)

say("\n  Smilers per cell:")
uu <- u[!is.na(u$culture) & !is.na(u$type), ]
for (l in capture.output(print(tapply(uu$smiler, list(uu$culture, uu$type),
                                      function(x) length(unique(x)))))) say("    ", l)

say("\n  Clips per smiler:")
for (l in capture.output(print(table(u$smiler)))) say("    ", l)

write.csv(u[, c("clip_code","culture","type","smiler","source_video",
                "n_frames","folder","file")],
          file.path(out_dir, "00b_unique_clip_list.csv"), row.names = FALSE)

say("\n5. WHAT THIS MEANS")
say(strrep("-", 74))
if (nrow(u) < nrow(d)) {
  say(sprintf("  %d of the %d files are duplicates.", nrow(d) - nrow(u), nrow(d)))
  say("  They MUST be removed before analysis. Counting the same smile twice")
  say("  would inflate the sample and make results look stronger than they are.")
  say("")
  say("  The unique clip list has been written to 00b_unique_clip_list.csv.")
  say("  The extraction script can be pointed at that list instead of the folders.")
} else {
  say("  No duplicates - the folders can be used as they are.")
}
say("\n  Check the smiler counts above. If there are far fewer European smilers")
say("  than Chinese, ask Ted whether more European data exists.")

say("\n", strrep("=", 74))
say("END")
close(con)
message("\nWrote 00b_duplicate_report.txt and 00b_unique_clip_list.csv")

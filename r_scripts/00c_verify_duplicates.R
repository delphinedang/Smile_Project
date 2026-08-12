# =============================================================================
# 00c_verify_duplicates.R
#
# READ-ONLY. Independent double-check of the duplicate finding.
#
# 00b used a numeric fingerprint. Fingerprints CAN collide, so this script
# does not trust it. It compares files EXACTLY - full text, character for
# character - and reports file sizes so you can verify by hand.
#
# It also prints full file paths for a few examples so you can open the
# pairs yourself and see whether they really are the same clip.
#
# Output: results_outputs/00c_verification_report.txt
#         results_outputs/00c_duplicate_pairs.csv   (every pair, for checking)
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

con <- file(file.path(out_dir, "00c_verification_report.txt"), open = "wt")
say <- function(...) { t <- paste0(..., collapse = "")
                       cat(t, "\n", sep = ""); cat(t, "\n", sep = "", file = con) }

say("DUPLICATE VERIFICATION  -  exact comparison, no fingerprints")
say("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say(strrep("=", 74))

files <- list.files(raw_dir, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
say(sprintf("\nFiles on disk: %d", length(files)))

say("\nFolder counts (should add up to the number above):")
rel <- sub(paste0(raw_dir, "/"), "", files, fixed = TRUE)
top <- sapply(strsplit(rel, "/"), `[`, 1)
tt <- table(top)
for (i in seq_along(tt)) say(sprintf("  %-28s %4d", names(tt)[i], tt[i]))
say(sprintf("  %-28s %4d", "TOTAL", sum(tt)))

# ---- read every file in full ------------------------------------------------
say("\nReading all files in full (this may take a minute)...")

dat <- lapply(files, function(f) {
  raw   <- readLines(f, warn = FALSE)
  clean <- sub("\r$", "", raw)
  hdr_i <- grep("^Video Time\t", clean)
  body  <- if (length(hdr_i)) clean[(hdr_i[1] + 1):length(clean)] else character(0)
  body  <- body[nzchar(trimws(body))]
  hv <- function(k) { h <- grep(paste0("^", k, "\t"), clean)
                      if (length(h)) trimws(sub(paste0("^", k, "\t"), "", clean[h[1]])) else NA_character_ }
  list(path = f,
       folder = strsplit(sub(paste0(raw_dir, "/"), "", f, fixed = TRUE), "/")[[1]][1],
       file = basename(f),
       size = file.info(f)$size,
       n_rows = length(body),
       body_text = paste(body, collapse = "\n"),   # EXACT data, not a hash
       full_text = paste(clean, collapse = "\n"),  # EXACT whole file
       srcvid = hv("Filename"))
})

body_txt <- vapply(dat, `[[`, "", "body_text")
full_txt <- vapply(dat, `[[`, "", "full_text")
folder   <- vapply(dat, `[[`, "", "folder")
fname    <- vapply(dat, `[[`, "", "file")
fsize    <- vapply(dat, function(x) as.numeric(x$size), 0)
nrows    <- vapply(dat, `[[`, 0L, "n_rows")

# ---- 1. exact comparison ----------------------------------------------------
say("\n1. EXACT COMPARISON OF MEASUREMENT DATA")
say(strrep("-", 74))
say("  Two files count as the same clip only if every single data row is")
say("  character-for-character identical. No hashing, no approximation.")
say("")

grp <- match(body_txt, unique(body_txt))       # exact string grouping
n_unique <- length(unique(body_txt))
say(sprintf("  Files                     : %d", length(files)))
say(sprintf("  Distinct measurement sets : %d", n_unique))
say(sprintf("  Redundant copies          : %d", length(files) - n_unique))

if (length(files) == n_unique) {
  say("\n  >>> NO DUPLICATES. All files contain different data. <<<")
  say("  The earlier finding was wrong. Use all files.")
} else {
  say(sprintf("\n  >>> CONFIRMED: %d of %d files repeat data already present. <<<",
              length(files) - n_unique, length(files)))
}

# ---- 2. are the copies byte-identical, or only data-identical? --------------
say("\n2. HOW IDENTICAL ARE THEY?")
say(strrep("-", 74))
dupe_groups <- which(table(grp) > 1)
if (length(dupe_groups)) {
  same_whole <- same_name <- same_size <- 0
  for (g in as.integer(names(dupe_groups))) {
    idx <- which(grp == g)
    if (length(unique(full_txt[idx])) == 1) same_whole <- same_whole + 1
    if (length(unique(fname[idx]))    == 1) same_name  <- same_name  + 1
    if (length(unique(fsize[idx]))    == 1) same_size  <- same_size  + 1
  }
  say(sprintf("  Repeated clips: %d", length(dupe_groups)))
  say(sprintf("    whole file identical (incl. header): %d", same_whole))
  say(sprintf("    filename also identical            : %d", same_name))
  say(sprintf("    file size also identical           : %d", same_size))
  say("")
  say("  If filenames and sizes match too, these are straightforward copies")
  say("  of the same export - most likely a folder was duplicated.")
}

# ---- 3. concrete examples you can open yourself -----------------------------
say("\n3. CHECK THESE YOURSELF")
say(strrep("-", 74))
say("  Open each pair below and compare. They should be identical.")
say("")
shown <- 0
for (g in as.integer(names(dupe_groups))) {
  if (shown >= 3) break
  idx <- which(grp == g)
  say(sprintf("  --- Example %d: %d data rows, %s bytes ---",
              shown + 1, nrows[idx[1]], format(fsize[idx[1]], big.mark = ",")))
  for (i in idx) say("    ", dat[[i]]$path)
  say(sprintf("    source video recorded inside the file: %s",
              basename(gsub("\\\\", "/", dat[[idx[1]]]$srcvid))))
  say(sprintf("    identical data rows: %s | identical whole file: %s",
              length(unique(body_txt[idx])) == 1,
              length(unique(full_txt[idx])) == 1))
  say("")
  shown <- shown + 1
}

# ---- 4. sanity check in the OTHER direction ---------------------------------
say("\n4. SANITY CHECK - are the 'unique' files genuinely different?")
say(strrep("-", 74))
say("  If the method were broken it might merge files that are actually")
say("  different. Checking 5 random pairs of supposedly-different files:")
say("")
set.seed(1)
u_idx <- which(!duplicated(body_txt))
if (length(u_idx) > 10) {
  for (k in 1:5) {
    p <- sample(u_idx, 2)
    say(sprintf("    %s  vs  %s   -> identical? %s",
                substr(fname[p[1]], 1, 30), substr(fname[p[2]], 1, 30),
                body_txt[p[1]] == body_txt[p[2]]))
  }
  say("\n  All should say FALSE. If any say TRUE, something is wrong.")
}

# ---- 5. what the folders actually distinguish -------------------------------
say("\n5. WHAT DO THE FOLDER NAMES ACTUALLY TELL US?")
say(strrep("-", 74))
codes <- sapply(dat, function(x) {
  b <- basename(gsub("\\\\", "/", x$srcvid))
  m <- regmatches(b, regexpr("_converted_[A-Za-z0-9-]+_noback", b))
  if (length(m)) sub("_converted_(.*)_noback", "\\1", m) else NA_character_
})
cult <- ifelse(grepl("^E", codes), "E (Chinese)",
        ifelse(grepl("^W", codes), "W (European)", NA))
say("  Culture letter in the clip code, by folder:")
for (l in capture.output(print(table(folder, cult, useNA = "ifany")))) say("  ", l)
say("")
say("  If E-coded clips appear in BOTH Chinese and European folders, then the")
say("  folder name does not identify the smiler's culture. Only the clip code does.")

# ---- 6. write every pair out for inspection ---------------------------------
pairs <- do.call(rbind, lapply(as.integer(names(dupe_groups)), function(g) {
  idx <- which(grp == g)
  data.frame(group = g,
             clip_code = codes[idx[1]],
             n_copies = length(idx),
             folder = folder[idx],
             file = fname[idx],
             size_bytes = fsize[idx],
             n_data_rows = nrows[idx],
             stringsAsFactors = FALSE)
}))
if (!is.null(pairs)) {
  write.csv(pairs, file.path(out_dir, "00c_duplicate_pairs.csv"), row.names = FALSE)
  say(sprintf("\n  Every duplicate group written to 00c_duplicate_pairs.csv (%d rows)",
              nrow(pairs)))
}

say("\n", strrep("=", 74))
say("END OF VERIFICATION")
close(con)
message("\nWrote 00c_verification_report.txt and 00c_duplicate_pairs.csv")

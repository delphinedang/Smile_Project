# =============================================================================
# 00_inspect_raw_data.R
#
# READ-ONLY. Changes nothing, writes one report file.
#
# Purpose: look at the full dataset before parsing it, and report anything
#          that differs from the pilot batch. Run this FIRST.
#
# Input : smile_project/raw_data_all/<4 condition folders>/*.txt
# Output: smile_project/results_outputs/00_raw_data_inspection.txt
#
# Base R only - no packages needed.
# =============================================================================

# ---- locate the project -----------------------------------------------------
find_root <- function() {
  p <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in 1:4) {
    if (dir.exists(file.path(p, "raw_data_all"))) return(p)
    p <- dirname(p)
  }
  stop("Could not find a folder containing 'raw_data_all'.\n",
       "  Set it manually: project_root <- '/path/to/smile_project'", call. = FALSE)
}
project_root <- find_root()
# project_root <- "/Users/delphine/Documents/Research projects /smile_project"

raw_dir <- file.path(project_root, "raw_data_all")
out_dir <- file.path(project_root, "results_outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

con <- file(file.path(out_dir, "00_raw_data_inspection.txt"), open = "wt")
say <- function(...) { txt <- paste0(..., collapse = "")
                       cat(txt, "\n", sep = ""); cat(txt, "\n", sep = "", file = con) }

say("RAW DATA INSPECTION")
say("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say("Source   : ", raw_dir)
say(strrep("=", 74))


# ---- 1. folders -------------------------------------------------------------
say("\n1. FOLDERS FOUND")
say(strrep("-", 74))
folders <- list.dirs(raw_dir, recursive = FALSE, full.names = FALSE)
if (!length(folders)) say("  (none - files may sit directly in raw_data_all)")
for (f in folders) {
  n <- length(list.files(file.path(raw_dir, f), pattern = "\\.txt$", recursive = TRUE))
  say(sprintf("  %-32s %4d .txt files", f, n))
}

files <- list.files(raw_dir, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
say(sprintf("\n  TOTAL .txt files: %d", length(files)))
if (!length(files)) { say("\nNo .txt files found - check the path."); close(con); stop("no files") }

# any non-txt sitting in there?
other <- setdiff(list.files(raw_dir, recursive = TRUE, full.names = TRUE), files)
other <- other[!dir.exists(other)]
if (length(other)) {
  say(sprintf("\n  Non-.txt files present: %d  (first few)", length(other)))
  for (o in head(other, 5)) say("    ", sub(paste0(raw_dir, "/"), "", o, fixed = TRUE))
}


# ---- 2. export type ---------------------------------------------------------
say("\n2. EXPORT TYPE  (detailed = usable, state = no action-unit data)")
say(strrep("-", 74))

info <- lapply(files, function(f) {
  lines <- tryCatch(readLines(f, n = 200, warn = FALSE), error = function(e) character(0))
  lines <- sub("\r$", "", lines)
  hdr_i <- grep("^Video Time\t", lines)
  hv <- function(k) { h <- grep(paste0("^", k, "\t"), lines)
                      if (length(h)) trimws(sub(paste0("^", k, "\t"), "", lines[h[1]])) else NA_character_ }
  type <- if (!length(hdr_i)) "no_data_table"
          else if (identical(trimws(strsplit(lines[hdr_i[1]], "\t")[[1]]),
                             c("Video Time", "Dominant Expression"))) "state_log"
          else "detailed"
  hdr <- if (length(hdr_i)) trimws(strsplit(lines[hdr_i[1]], "\t")[[1]]) else character(0)
  list(path = f,
       folder = basename(dirname(dirname(f))),   # condition folder if nested
       folder1 = basename(dirname(f)),
       file = basename(f),
       type = type,
       fps = suppressWarnings(as.numeric(hv("Frame rate"))),
       srcname = hv("Filename"),
       ncol = length(hdr),
       hdr = paste(hdr, collapse = "|"))
})

type_tab <- table(sapply(info, `[[`, "type"))
for (i in seq_along(type_tab)) say(sprintf("  %-16s %4d", names(type_tab)[i], type_tab[i]))

state_files <- vapply(info, function(x) x$type == "state_log", logical(1))
if (any(state_files)) {
  say("\n  State-log files (need re-export):")
  for (x in info[state_files]) say("    ", x$file)
}


# ---- 3. column layout -------------------------------------------------------
say("\n3. COLUMN LAYOUT  (detailed files only)")
say(strrep("-", 74))
det <- info[sapply(info, function(x) x$type == "detailed")]
if (length(det)) {
  layouts <- table(sapply(det, `[[`, "hdr"))
  say(sprintf("  Distinct layouts: %d", length(layouts)))
  say(sprintf("  Column counts   : %s",
              paste(sort(unique(sapply(det, `[[`, "ncol"))), collapse = ", ")))
  if (length(layouts) > 1) {
    say("  ** WARNING: layouts differ between files. Counts per layout:")
    for (i in seq_along(layouts)) say(sprintf("     layout %d: %d files", i, layouts[i]))
  } else say("  All detailed files share an identical layout. Good.")
}


# ---- 4. frame rate ----------------------------------------------------------
say("\n4. FRAME RATE")
say(strrep("-", 74))
fps <- sapply(det, `[[`, "fps")
for (v in sort(unique(fps))) say(sprintf("  %.3f fps : %d files", v, sum(fps == v)))


# ---- 5. THE IMPORTANT ONE: clip code format ---------------------------------
say("\n5. INTERNAL CLIP CODES")
say(strrep("-", 74))
say("  Format expected from the pilot batch: <letter><number><HA|SA><number>-<number>")
say("  e.g. E11HA01-01   E = East/Chinese, 11 = smiler, HA = humorous, 01 = stimulus")
say("")

codes <- sapply(det, function(x) {
  b <- basename(gsub("\\\\", "/", x$srcname))
  m <- regmatches(b, regexpr("_converted_[A-Za-z0-9-]+_noback", b))
  if (length(m)) sub("_converted_(.*)_noback", "\\1", m) else NA_character_
})
say(sprintf("  Codes extracted: %d of %d detailed files", sum(!is.na(codes)), length(det)))
if (any(is.na(codes))) {
  say("  ** Files where no code could be read - showing the raw source name:")
  for (x in det[is.na(codes)][1:min(5, sum(is.na(codes)))])
    say("     ", basename(gsub("\\\\", "/", x$srcname)))
}

ok_codes <- codes[!is.na(codes)]
std <- grepl("^[A-Za-z]+[0-9]+(HA|SA)[0-9]+-[0-9]+$", ok_codes)
say(sprintf("\n  Match the expected pattern : %d", sum(std)))
say(sprintf("  Do NOT match               : %d", sum(!std)))
if (any(!std)) {
  say("  ** Non-matching codes (these will need a parser change):")
  for (c in head(unique(ok_codes[!std]), 15)) say("     ", c)
}

say("\n  >>> CULTURE PREFIXES FOUND (the letter at the start of each code) <<<")
pre <- toupper(sub("^([A-Za-z]+).*$", "\\1", ok_codes))
pt <- table(pre)
for (i in seq_along(pt)) say(sprintf("     '%s' : %5d clips", names(pt)[i], pt[i]))
say("     (pilot batch used 'E'. The parser must be told what European uses.)")

say("\n  Stimulus type codes:")
sty <- table(sub("^[A-Za-z]+[0-9]+(HA|SA).*$", "\\1", ok_codes[std]))
for (i in seq_along(sty)) say(sprintf("     %s : %5d clips", names(sty)[i], sty[i]))


# ---- 6. folder vs code cross-check ------------------------------------------
say("\n6. DOES THE FOLDER AGREE WITH THE CLIP CODE?")
say(strrep("-", 74))
say("  The folder name and the internal code are two independent records.")
say("  Any disagreement needs resolving before analysis.\n")

fold <- sapply(det, function(x) {
  # find whichever ancestor folder names the condition
  p <- strsplit(gsub("\\\\", "/", x$path), "/")[[1]]
  hit <- grep("(Fake|Genuine)", p, ignore.case = TRUE)
  if (length(hit)) p[hit[1]] else NA_character_
})
code_type <- rep(NA_character_, length(det))
code_type[!is.na(codes) & std] <- sub("^[A-Za-z]+[0-9]+(HA|SA).*$", "\\1", codes[!is.na(codes) & std])
code_cult <- rep(NA_character_, length(det))
code_cult[!is.na(codes)] <- toupper(sub("^([A-Za-z]+).*$", "\\1", codes[!is.na(codes)]))

if (any(!is.na(fold))) {
  print_tab <- table(folder = fold, code = code_type, useNA = "ifany")
  for (l in capture.output(print(print_tab))) say("  ", l)
  say("")
  say("  Expected: 'Genuine' folders -> HA,  'Fake' folders -> SA")

  bad <- which(!is.na(fold) & !is.na(code_type) &
               ((grepl("Genuine", fold, ignore.case = TRUE) & code_type != "HA") |
                (grepl("Fake",    fold, ignore.case = TRUE) & code_type != "SA")))
  say(sprintf("\n  Mismatches: %d", length(bad)))
  if (length(bad)) for (i in head(bad, 20))
    say(sprintf("     %-14s folder='%s'  file=%s", codes[i], fold[i], det[[i]]$file))

  say("\n  Culture prefix by folder:")
  for (l in capture.output(print(table(folder = fold, prefix = code_cult, useNA = "ifany"))))
    say("  ", l)
} else say("  Could not identify condition folders by name - check folder naming.")


# ---- 7. smilers -------------------------------------------------------------
say("\n7. SMILERS")
say(strrep("-", 74))
sm_num <- rep(NA_integer_, length(det))
sm_num[!is.na(codes)] <- suppressWarnings(
  as.integer(sub("^[A-Za-z]+([0-9]+).*$", "\\1", codes[!is.na(codes)])))
sm_id <- ifelse(is.na(code_cult) | is.na(sm_num), NA_character_,
                sprintf("%s%02d", code_cult, sm_num))
say(sprintf("  Distinct smilers: %d", length(unique(na.omit(sm_id)))))
say("\n  Clips per smiler:")
tt <- sort(table(na.omit(sm_id)))
say(sprintf("    min %d, median %.0f, max %d",
            min(tt), median(tt), max(tt)))
for (l in capture.output(print(table(na.omit(sm_id))))) say("  ", l)

say("\n  ** ID COLLISION CHECK **")
say("  If two cultures reuse the same numbers, the prefix must be kept in the ID.")
coll <- table(sm_num, code_cult)
dupes <- rownames(coll)[rowSums(coll > 0) > 1]
if (length(dupes)) {
  say(sprintf("  Numbers reused across cultures: %s", paste(dupes, collapse = ", ")))
  say("  -> This is fine, PROVIDED the culture letter is kept in the smiler ID")
  say("     (E07 and W07 are different people). The parser already does this.")
} else {
  say("  No overlap - each smiler number belongs to one culture only.")
}

say("\n  Smilers per culture:")
for (l in capture.output(print(table(code_cult)))) say("  ", l)


# ---- 8. design coverage -----------------------------------------------------
say("\n8. DESIGN COVERAGE  (what the analysis will actually have)")
say(strrep("-", 74))
if (any(!is.na(code_cult)) && any(!is.na(code_type))) {
  for (l in capture.output(print(table(culture = code_cult, type = code_type))))
    say("  ", l)
  say("\n  Smilers contributing to each cell:")
  d <- data.frame(sm_id, code_cult, code_type, stringsAsFactors = FALSE)
  d <- d[complete.cases(d), ]
  for (l in capture.output(print(tapply(d$sm_id, list(d$code_cult, d$code_type),
                                        function(x) length(unique(x))))))
    say("  ", l)
}


# ---- 9. summary of what to tell the analyst ---------------------------------
say("\n9. WHAT TO CHECK IN THIS REPORT")
say(strrep("-", 74))
say("  a) Section 5: which letters are used for culture. The parser currently")
say("     knows E = Chinese and assumes W = European. If European uses something")
say("     else, one line in 01_extract_facereader.R needs changing.")
say("  b) Section 6: any folder-vs-code mismatches must be resolved with Ted.")
say("  c) Section 7: whether smiler numbers collide across cultures.")
say("  d) Section 2: how many state logs still need re-exporting.")
say("  e) Section 4: whether frame rate is 8 fps throughout.")

say("\n", strrep("=", 74))
say("END OF INSPECTION")
close(con)

message("\nWrote results_outputs/00_raw_data_inspection.txt")

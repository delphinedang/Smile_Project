# =============================================================================
# 00a_inspect_raw_data.R
#
# READ-ONLY. Run this first whenever new data arrives. It changes nothing.
#
# Two jobs:
#   (a) report anything about the raw exports that would break or mislead
#       the extraction - unknown culture codes, folder/code disagreements,
#       smiler ID collisions, unexpected export formats;
#   (b) verify duplicates by EXACT text comparison. The four condition
#       folders have been found to hold two copies of every clip, so this
#       must be confirmed each time, not assumed.
#
# Reads : the research drive (path set in r_scripts/00_config.R)
# Writes: results_outputs/00a_inspection_report.txt
#         results_outputs/00a_duplicate_pairs.csv
# =============================================================================

# ---- load shared config (works from project root, r_scripts/, or report/) ----
.cfg <- NULL
for (.p in c("r_scripts/00_config.R", "00_config.R", "../r_scripts/00_config.R",
             "../../r_scripts/00_config.R")) if (file.exists(.p)) { .cfg <- .p; break }
if (is.null(.cfg)) stop("Cannot find r_scripts/00_config.R. Set the working ",
                        "directory to the Smile_Project folder.", call. = FALSE)
source(.cfg)

require_raw()

# Say plainly where this will write, before doing anything, so a wrong
# project root is obvious immediately rather than after the fact.
message("\nReading from : ", RAW_DIR)
message("Writing to   : ", OUT_DIR)
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

con <- file(file.path(OUT_DIR, "00a_inspection_report.txt"), open = "wt")
say <- function(...) { t <- paste0(..., collapse = "")
                       cat(t, "\n", sep = ""); cat(t, "\n", sep = "", file = con) }

say("RAW DATA INSPECTION")
say("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say("Source   : ", RAW_DIR)
say(strrep("=", 74))


# -----------------------------------------------------------------------------
# 1. WHAT IS THERE
# -----------------------------------------------------------------------------
say("\n1. FOLDERS AND FILE COUNTS")
say(strrep("-", 74))

files <- list.files(RAW_DIR, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
rel   <- sub(paste0(RAW_DIR, "/"), "", files, fixed = TRUE)
top   <- vapply(strsplit(rel, "/"), `[`, "", 1)

tt <- table(top)
for (i in seq_along(tt)) say(sprintf("  %-30s %5d", names(tt)[i], tt[i]))
say(sprintf("  %-30s %5d", "TOTAL", length(files)))

other <- setdiff(list.files(RAW_DIR, recursive = TRUE, full.names = TRUE), files)
other <- other[!dir.exists(other)]
if (length(other)) {
  say(sprintf("\n  Non-.txt files present: %d (ignored). First few:", length(other)))
  for (o in head(other, 5)) say("    ", sub(paste0(RAW_DIR, "/"), "", o, fixed = TRUE))
}


# -----------------------------------------------------------------------------
# 2. READ EVERY FILE
# -----------------------------------------------------------------------------
message("Reading ", length(files), " files...")

dat <- lapply(files, function(f) {
  lines <- sub("\r$", "", readLines(f, warn = FALSE))
  hdr_i <- grep("^Video Time\t", lines)
  type <- if (!length(hdr_i)) "no_data_table"
          else if (identical(trimws(strsplit(lines[hdr_i[1]], "\t")[[1]]),
                             c("Video Time", "Dominant Expression"))) "state_log"
          else "detailed"
  body <- if (type == "detailed") {
    b <- lines[(hdr_i[1] + 1):length(lines)]; b[nzchar(trimws(b))]
  } else character(0)
  src <- header_value(lines, "Filename")
  list(path = f,
       folder = strsplit(sub(paste0(RAW_DIR, "/"), "", f, fixed = TRUE), "/")[[1]][1],
       file = basename(f), type = type,
       fps = suppressWarnings(as.numeric(header_value(lines, "Frame rate"))),
       code = extract_clip_code(src),
       stimulus = sub("\\.mp4.*$", "", basename(gsub("\\\\", "/", src))),
       n_rows = length(body),
       size = as.numeric(file.info(f)$size),
       hdr = if (type == "detailed") paste(trimws(strsplit(lines[hdr_i[1]], "\t")[[1]]),
                                           collapse = "|") else "",
       body_text = paste(body, collapse = "\n"))
})

type_v <- vapply(dat, `[[`, "", "type")
say("\n2. EXPORT TYPE")
say(strrep("-", 74))
for (i in seq_along(table(type_v)))
  say(sprintf("  %-16s %5d", names(table(type_v))[i], table(type_v)[i]))
say("")
say("  'detailed' files carry per-frame emotions and action units - usable.")
say("  'state_log' files carry only dominant-expression changes - NOT usable,")
say("  they must be re-exported from FaceReader as detailed logs.")

if (any(type_v == "state_log")) {
  say("\n  State logs needing re-export:")
  for (x in dat[type_v == "state_log"]) say("    ", x$folder, "/", x$file)
}

det <- dat[type_v == "detailed"]


# -----------------------------------------------------------------------------
# 3. COLUMN LAYOUT AND FRAME RATE
# -----------------------------------------------------------------------------
say("\n3. FILE STRUCTURE")
say(strrep("-", 74))
lay <- table(vapply(det, `[[`, "", "hdr"))
say(sprintf("  Distinct column layouts: %d %s", length(lay),
            if (length(lay) == 1) "(consistent - good)" else "** INCONSISTENT **"))
say("\n  Frame rates:")
for (i in seq_along(table(vapply(det, `[[`, 0, "fps"))))
  say(sprintf("    %-10s %5d files",
              names(table(vapply(det, `[[`, 0, "fps")))[i],
              table(vapply(det, `[[`, 0, "fps"))[i]))
say("\n  Frame rate is read per file, so small differences are handled.")


# -----------------------------------------------------------------------------
# 4. DUPLICATES - exact text comparison, no hashing
# -----------------------------------------------------------------------------
say("\n4. DUPLICATE CLIPS")
say(strrep("-", 74))
say("  Two files count as the same clip only if every data row matches")
say("  character for character. No fingerprints, no approximation.")
say("")

body <- vapply(dat, `[[`, "", "body_text")
grp  <- match(body, unique(body))
n_uniq <- length(unique(body))

say(sprintf("  Files                     : %d", length(dat)))
say(sprintf("  Distinct measurement sets : %d", n_uniq))
say(sprintf("  Redundant copies          : %d", length(dat) - n_uniq))

dupe_groups <- as.integer(names(which(table(grp) > 1)))
if (length(dupe_groups)) {
  same_whole <- sum(vapply(dupe_groups, function(g) {
    i <- which(grp == g); length(unique(vapply(dat[i], `[[`, "", "file"))) == 1 }, TRUE))
  say(sprintf("\n  Repeated clips: %d", length(dupe_groups)))
  say(sprintf("  ...of which the filename also matches: %d", same_whole))
  say("\n  Folders the copies span:")
  span <- vapply(dupe_groups, function(g) {
    paste(sort(unique(vapply(dat[which(grp == g)], `[[`, "", "folder"))), collapse = " + ")
  }, "")
  for (l in capture.output(print(sort(table(span), decreasing = TRUE)))) say("    ", l)

  say("\n  Three examples - open these and compare for yourself:")
  for (k in seq_len(min(3, length(dupe_groups)))) {
    i <- which(grp == dupe_groups[k])
    say(sprintf("    [%d rows, %s bytes]", dat[[i[1]]]$n_rows,
                format(dat[[i[1]]]$size, big.mark = ",")))
    for (j in i) say("      ", dat[[j]]$path)
  }

  pairs <- do.call(rbind, lapply(dupe_groups, function(g) {
    i <- which(grp == g)
    data.frame(group = g, clip_code = dat[[i[1]]]$code, n_copies = length(i),
               folder = vapply(dat[i], `[[`, "", "folder"),
               file = vapply(dat[i], `[[`, "", "file"),
               stringsAsFactors = FALSE)
  }))
  write.csv(pairs, file.path(OUT_DIR, "00a_duplicate_pairs.csv"), row.names = FALSE)
  say(sprintf("\n  Full list written to 00a_duplicate_pairs.csv (%d rows)", nrow(pairs)))
} else say("\n  No duplicates found.")

# reverse check: are the 'unique' files genuinely different?
say("\n  Reverse check - 5 pairs the method says are DIFFERENT:")
set.seed(1)
u <- which(!duplicated(body))
if (length(u) > 10) {
  for (k in 1:5) {
    p <- sample(u, 2)
    say(sprintf("    %-28s vs %-28s identical? %s",
                substr(dat[[p[1]]]$file, 1, 28), substr(dat[[p[2]]]$file, 1, 28),
                body[p[1]] == body[p[2]]))
  }
  say("    All should read FALSE.")
}


# -----------------------------------------------------------------------------
# 5. CLIP CODES
# -----------------------------------------------------------------------------
say("\n5. CLIP CODES")
say(strrep("-", 74))
say("  Expected form: <letters><number><HA|SA><number>-<number>, e.g. E11HA01-01")
say("")
codes <- vapply(det, function(x) if (is.na(x$code)) "" else x$code, "")
ok_c  <- codes[nzchar(codes)]
std   <- grepl("^[A-Za-z]+[0-9]+(HA|SA)[0-9]+-[0-9]+$", ok_c)
say(sprintf("  Codes read      : %d of %d detailed files", length(ok_c), length(det)))
say(sprintf("  Match the form  : %d", sum(std)))
say(sprintf("  Do NOT match    : %d", sum(!std)))
if (any(!std)) for (c in head(unique(ok_c[!std]), 10)) say("      ", c)

say("\n  Culture letters found:")
pre <- toupper(sub("^([A-Za-z]+).*$", "\\1", ok_c))
for (i in seq_along(table(pre)))
  say(sprintf("    '%s' -> %-10s %5d clips", names(table(pre))[i],
              ifelse(is.na(CULTURE_MAP[names(table(pre))[i]]), "** UNKNOWN **",
                     CULTURE_MAP[names(table(pre))[i]]), table(pre)[i]))
unknown <- setdiff(names(table(pre)), names(CULTURE_MAP))
if (length(unknown))
  say("\n  ** Add these to CULTURE_MAP in 00_config.R before extracting. **")

say("\n  Condition letters found:")
for (i in seq_along(table(sub("^[A-Za-z]+[0-9]+(HA|SA).*$", "\\1", ok_c[std]))))
  say(sprintf("    %-4s %5d", names(table(sub("^[A-Za-z]+[0-9]+(HA|SA).*$", "\\1", ok_c[std])))[i],
              table(sub("^[A-Za-z]+[0-9]+(HA|SA).*$", "\\1", ok_c[std]))[i]))

# codes reused for genuinely different clips
say("\n  Clip codes used for more than one DIFFERENT clip:")
uniq_idx <- which(!duplicated(body))
uc <- vapply(dat[uniq_idx], function(x) if (is.na(x$code)) "" else x$code, "")
reused <- names(which(table(uc[nzchar(uc)]) > 1))
if (length(reused)) {
  for (r in reused) {
    say("    ** ", r)
    for (j in uniq_idx[uc == r])
      say(sprintf("         '%s'  %d rows  (%s)", dat[[j]]$stimulus, dat[[j]]$n_rows,
                  dat[[j]]$folder))
  }
  say("    These are ambiguous and will be held out of the analysis.")
} else say("    none")


# -----------------------------------------------------------------------------
# 6. FOLDER vs CODE
# -----------------------------------------------------------------------------
say("\n6. DO THE FOLDERS AGREE WITH THE CODES?")
say(strrep("-", 74))
fold <- vapply(det, `[[`, "", "folder")
cult <- ifelse(nzchar(codes), toupper(sub("^([A-Za-z]+).*$", "\\1", codes)), NA)
cond <- ifelse(grepl("^[A-Za-z]+[0-9]+(HA|SA)", codes),
               sub("^[A-Za-z]+[0-9]+(HA|SA).*$", "\\1", codes), NA)

say("  Culture letter by folder:")
for (l in capture.output(print(table(folder = fold, culture = cult, useNA = "ifany"))))
  say("    ", l)
say("")
say("  If a folder contains both letters, the folder name does NOT identify")
say("  culture and must not be used for it. Only the clip code is reliable.")

say("\n  Condition by folder:")
for (l in capture.output(print(table(folder = fold, condition = cond, useNA = "ifany"))))
  say("    ", l)
bad <- which(!is.na(cond) &
             ((grepl("Genuine", fold, ignore.case = TRUE) & cond != "HA") |
              (grepl("Fake|Performed", fold, ignore.case = TRUE) & cond != "SA")))
say(sprintf("\n  Clips whose folder and code disagree on condition: %d", length(bad)))
if (length(bad)) for (i in head(bad, 15))
  say(sprintf("    %-14s folder='%s'", codes[i], fold[i]))


# -----------------------------------------------------------------------------
# 7. SMILERS
# -----------------------------------------------------------------------------
say("\n7. SMILERS  (counting each clip once)")
say(strrep("-", 74))
say("  IDs are taken from the code EXACTLY as written and are NOT padded.")
say("  E1 and E01 are different people; padding would merge them.")
say("")
u_det <- dat[uniq_idx]; u_det <- u_det[vapply(u_det, `[[`, "", "type") == "detailed"]
uc2 <- vapply(u_det, function(x) if (is.na(x$code)) "" else x$code, "")
sm  <- ifelse(nzchar(uc2), sub("^([A-Za-z]+[0-9]+).*$", "\\1", uc2), NA)
say(sprintf("  Distinct smilers: %d", length(unique(na.omit(sm)))))
say("\n  Clips per smiler:")
for (l in capture.output(print(table(na.omit(sm))))) say("    ", l)

# padded/unpadded collision
num  <- suppressWarnings(as.integer(sub("^[A-Za-z]+", "", na.omit(sm))))
lets <- sub("[0-9]+$", "", na.omit(sm))
key  <- paste0(lets, num)
coll <- names(which(table(unique(data.frame(sm = na.omit(sm), key))$key) > 1))
say(sprintf("\n  Smiler numbers written two ways (e.g. E1 and E01): %d", length(coll)))
if (length(coll)) {
  for (k in coll) {
    variants <- unique(na.omit(sm)[key == k])
    say("    ", paste(variants, collapse = " and "),
        "  -> kept separate; check with Ted they are different people")
  }
}

say("\n8. DESIGN COVERAGE  (unique clips)")
say(strrep("-", 74))
cu <- ifelse(nzchar(uc2), toupper(sub("^([A-Za-z]+).*$", "\\1", uc2)), NA)
co <- ifelse(grepl("^[A-Za-z]+[0-9]+(HA|SA)", uc2),
             sub("^[A-Za-z]+[0-9]+(HA|SA).*$", "\\1", uc2), NA)
for (l in capture.output(print(table(culture = cu, condition = co, useNA = "ifany"))))
  say("  ", l)
say("\n  Smilers per cell:")
dd <- data.frame(sm, cu, co, stringsAsFactors = FALSE); dd <- dd[complete.cases(dd), ]
for (l in capture.output(print(tapply(dd$sm, list(dd$cu, dd$co),
                                      function(z) length(unique(z)))))) say("  ", l)

say("\n9. WHAT TO CHECK BEFORE EXTRACTING")
say(strrep("-", 74))
say("  - Section 5: any UNKNOWN culture letters? Add them to 00_config.R.")
say("  - Section 5: any codes reused for different clips? Ask Ted.")
say("  - Section 6: folder/code disagreements on condition?")
say("  - Section 7: any padded/unpadded smiler pairs to confirm?")
say("  - Section 2: any state logs still needing re-export?")

say("\n", strrep("=", 74))
say("END OF INSPECTION")
close(con)

message("\nDone. Files written:")
for (f in c("00a_inspection_report.txt", "00a_duplicate_pairs.csv")) {
  fp <- file.path(OUT_DIR, f)
  if (file.exists(fp))
    message("  ", fp, "  (", format(file.info(fp)$size, big.mark = ","), " bytes)")
}

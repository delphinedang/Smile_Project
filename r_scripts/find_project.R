# =============================================================================
# Paste this into the R console. It finds your project folder for you.
# =============================================================================

# ---- STEP 1: search for it --------------------------------------------------
# Looks for any folder containing an 'r_scripts' subfolder.

hits <- list.dirs("~/Documents", recursive = TRUE, full.names = TRUE)
hits <- hits[basename(hits) == "r_scripts"]
projects <- dirname(hits)

cat("\nProject folders found:\n")
if (length(projects)) {
  for (i in seq_along(projects)) cat("  [", i, "] ", projects[i], "\n", sep = "")
} else {
  cat("  none under ~/Documents\n")
  cat("  Try searching wider:\n")
  cat("    hits <- list.dirs('~', recursive = TRUE, full.names = TRUE)\n")
  cat("    hits[basename(hits) == 'r_scripts']\n")
}


# ---- STEP 2: go there -------------------------------------------------------
# Copy the path printed above into setwd(), or if there is exactly one:

if (length(projects) == 1) {
  setwd(projects[1])
  cat("\nWorking directory set to:\n  ", getwd(), "\n")
}


# ---- STEP 3: check it looks right -------------------------------------------
cat("\nContents of the project folder:\n")
print(list.dirs(".", recursive = FALSE, full.names = FALSE))

cat("\nScripts found:\n")
print(list.files("r_scripts", pattern = "\\.R$"))

cat("\nResearch drive mounted? ",
    dir.exists("/Volumes/PRJ-smile/raw_data_all"), "\n")


# ---- STEP 4: make this permanent --------------------------------------------
# In RStudio: File > New Project > Existing Directory > choose this folder.
# That creates a .Rproj file. From then on, opening the project always puts
# R in the right place and none of this is needed again.

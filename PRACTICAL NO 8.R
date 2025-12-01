# ====================================================================
# Breast Cancer — Auto-cleaning & Missing-value handler (dataset-aware)
# ====================================================================
# Usage: keep this script in same session where "Breast_Cancer.csv" is present
# It will produce "Breast_Cancer_cleaned.csv" in working directory.
# Comments in Hinglish for readability.
# ====================================================================

# ---- 0. packages ----
packages <- c("dplyr", "tidyr", "readr", "stringr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# ---- 1. Read dataset (use uploaded file if present) ----
# The uploaded path noted: "/mnt/data/Breast_Cancer.csv"
# But we use a flexible approach: try that path first, else default filename.
possible_paths <- c("/mnt/data/Breast_Cancer.csv", "Breast_Cancer.csv", "breast_cancer.csv")
input_file <- NULL
for (p in possible_paths) {
  if (file.exists(p)) { input_file <- p; break }
}
if (is.null(input_file)) {
  stop("Breast_Cancer.csv not found in expected locations. Place file in working directory.")
}

df <- readr::read_csv(input_file, na = c("", "NA"))
cat("Loaded:", input_file, "\nRows:", nrow(df), "Cols:", ncol(df), "\n\n")

# ---- 2. Quick inspect ----
cat("Column names:\n"); print(names(df)); cat("\nMissing per column (before):\n")
print(colSums(is.na(df))); cat("\n")

# ---- 3. Helpers ----
get_mode <- function(x) {
  x_no <- na.omit(x)
  if (length(x_no) == 0) return(NA_character_)
  ux <- unique(x_no)
  ux[which.max(tabulate(match(x_no, ux)))]
}
is_numeric_like <- function(x) {
  if (!is.character(x)) return(FALSE)
  s <- stringr::str_trim(x)
  # allow decimal and integer numbers, drop empty/NA
  s <- s[s != "" & !is.na(s)]
  if (length(s) == 0) return(FALSE)
  suppressWarnings(all(!is.na(as.numeric(s))))
}
convert_numeric_like <- function(dat) {
  for (nm in names(dat)) {
    if (is.character(dat[[nm]]) && is_numeric_like(dat[[nm]])) {
      dat[[nm]] <- as.numeric(stringr::str_trim(dat[[nm]]))
      message("Converted to numeric-like: ", nm)
    }
  }
  dat
}

# ---- 4. Preprocess conversions ----
df <- convert_numeric_like(df)

# If there's an 'id' like column, standardize name to Patient_ID (non-destructive)
id_cols <- names(df)[tolower(names(df)) %in% c("id", "patient_id", "patientid", "pid")]
if (length(id_cols) > 0) {
  # rename first matching to Patient_ID
  names(df)[names(df) == id_cols[1]] <- "Patient_ID"
  message("Renamed column '", id_cols[1], "' -> 'Patient_ID'")
} else {
  # if no id, create Patient_ID sequentially
  df$Patient_ID <- paste0("BC", sprintf("%04d", seq_len(nrow(df))))
  message("No ID column found -> created Patient_ID")
}

# ---- 5. Dataset-aware custom rules (only apply if those columns exist) ----
# - Diagnosis: keep as-is; if missing, mark as "Unknown"
if ("Diagnosis" %in% names(df)) {
  df$Diagnosis <- as.character(df$Diagnosis)
  df$Diagnosis[is.na(df$Diagnosis)] <- "Unknown"
  message("Diagnosis missing values -> 'Unknown'")
}

# - Common numeric columns conversions (if present)
# Typical BC datasets may have columns with names containing 'radius','texture','smooth','perimeter','area'
numeric_patterns <- c("radius", "texture", "smooth", "perimeter", "area", "compactness", "concavity", "symmetry", "fractal")
for (pat in numeric_patterns) {
  hits <- grep(pat, names(df), ignore.case = TRUE, value = TRUE)
  if (length(hits) > 0) {
    for (h in hits) {
      if (!is.numeric(df[[h]])) {
        df[[h]] <- suppressWarnings(as.numeric(df[[h]]))
      }
      message("Checked numeric-like column:", h)
    }
  }
}

# ---- 6. Imputation strategy ----
cleaned <- df  # copy

# If Price-like logic not relevant here; we use general rules:
for (nm in names(cleaned)) {
  col <- cleaned[[nm]]
  if (any(is.na(col))) {
    if (is.numeric(col)) {
      med <- median(col, na.rm = TRUE)
      if (is.na(med)) med <- 0
      cleaned[[nm]][is.na(cleaned[[nm]])] <- med
      message(sprintf("Numeric imputed (median) for %s -> %s", nm, format(med, digits=6)))
    } else if (is.logical(col)) {
      cleaned[[nm]][is.na(cleaned[[nm]])] <- FALSE
      message(sprintf("Logical imputed (FALSE) for %s", nm))
    } else { # character/factor
      modev <- get_mode(col)
      if (is.na(modev)) modev <- "Unknown"
      cleaned[[nm]][is.na(cleaned[[nm]])] <- modev
      message(sprintf("Categorical imputed (mode/'Unknown') for %s -> %s", nm, as.character(modev)))
    }
  }
}

# ---- 7. Optional group-wise example:
# If 'Diagnosis' exists and some numeric columns still had NAs earlier,
# you might want diagnosis-wise medians. We'll show an example for 'mean_radius' if present.
if ("Diagnosis" %in% names(df)) {
  candidate_num <- names(df)[sapply(df, is.numeric)]
  # example: if any numeric still equal to median fill we set and category med exists, replace
  for (colname in candidate_num) {
    # compute diag-wise medians from original df (before global fill)
    diag_medians <- df %>%
      filter(!is.na(.data[[colname]]), !is.na(Diagnosis)) %>%
      group_by(Diagnosis) %>%
      summarize(med = median(.data[[colname]], na.rm = TRUE), .groups = "drop")
    if (nrow(diag_medians) > 0) {
      cleaned <- cleaned %>%
        left_join(diag_medians, by = "Diagnosis") %>%
        mutate(!!colname := if_else(!is.na(med) & is.na(df[[colname]]), med, .data[[colname]])) %>%
        select(-med)
      # Note: this replaces values ONLY for rows where original df had NA
    }
  }
  message("Applied optional diagnosis-wise numeric imputation where possible.")
}

# ---- 8. Final diagnostics & save ----
cat("\nMissing per column (after):\n"); print(colSums(is.na(cleaned))); cat("\n")
num_summary <- cleaned %>% select(where(is.numeric))
if (ncol(num_summary) > 0) {
  cat("Numeric summary (selected):\n"); print(summary(num_summary)); cat("\n")
}

out_file <- "Breast_Cancer_cleaned.csv"
readr::write_csv(cleaned, out_file)
cat("Cleaned file written to:", out_file, "\n")

# ---- 9. Provide quick counts ----
cat(sprintf("Rows: %d | Columns: %d\n", nrow(cleaned), ncol(cleaned)))
cat("Diagnosis value counts:\n"); if ("Diagnosis" %in% names(cleaned)) print(table(cleaned$Diagnosis)) else cat("No Diagnosis column.\n")
cat("\nDone. Agar aur customization chahiye (e.g., specific columns ka different rule, or scaling, or encoding), batao.\n")

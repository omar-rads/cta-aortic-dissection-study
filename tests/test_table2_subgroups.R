# Synthetic regression and rejection checks. No clinical data are needed.
script <- normalizePath("scripts/validation/reproduce_table2_subgroups.R", mustWork = TRUE)
tmp <- tempfile("table2-test-")
dir.create(tmp)
make_rows <- function(score, lab, positive, negative, unknown, classification) {
  outcome <- c(rep("Positive", positive), rep("Negative", negative), rep("Unknown", unknown))
  data.frame(add_rs = score, d_dimer_status = lab, algorithm_class = classification,
             aas_outcome = outcome, aas_positive = c(rep(1L, positive), rep(0L, negative), rep(NA_integer_, unknown)))
}
# Artificial records with the published aggregate margins, not patient records.
x <- rbind(make_rows(2, "Absent/not obtained", 32, 198, 0, "Concordant"),
           make_rows(1, "Positive", 1, 48, 1, "Concordant"),
           make_rows(0, "Absent/not obtained", 42, 782, 1, "Discordant"),
           make_rows(1, "Negative", 0, 48, 0, "Discordant"),
           make_rows(NA_real_, "Negative", 4, 35, 1, "Unclassifiable"))
run_audit <- function(data, label, should_pass) {
  input <- file.path(tmp, paste0(label, ".rds"))
  out <- file.path(tmp, label)
  saveRDS(data, input)
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--vanilla", shQuote(script), shQuote(input), shQuote(out)),
                    stdout = FALSE, stderr = FALSE)
  stopifnot((status == 0L) == should_pass)
  if (!should_pass) stopifnot(!dir.exists(out))
  invisible(out)
}
base <- run_audit(x, "valid", TRUE)
z <- read.csv(file.path(base, "subgroup_counts.csv"))
stopifnot(identical(z$positive, c(32L, 1L, 42L, 0L, 4L)),
          identical(z$known, c(230L, 49L, 824L, 48L, 39L)),
          isTRUE(all.equal(z$yield_pct, 100 * z$positive / z$known, tolerance = 1e-12)),
          z$exact95_upper_pct[4] > 0)
# Row order, high-score lab status and missing-score lab status do not change membership.
y <- x[nrow(x):1, ]; y$d_dimer_status[y$add_rs %in% 2:3] <- "Negative"
y$d_dimer_status[is.na(y$add_rs)] <- "Positive"
variant <- run_audit(y, "invariance", TRUE)
for (f in c("subgroup_counts.csv", "algorithm_counts.csv", "score_counts.csv", "assertions.csv")) {
  stopifnot(identical(unname(tools::md5sum(file.path(base, f))), unname(tools::md5sum(file.path(variant, f)))))
}
y <- x; y$algorithm_class[1] <- "Discordant"; run_audit(y, "bad_high_score", FALSE)
y <- x; y$aas_positive[1] <- 0L; run_audit(y, "bad_outcome", FALSE)
y <- x; y$aas_positive[280] <- 0L; run_audit(y, "unknown_as_negative", FALSE)
y <- x; y$d_dimer_status[1] <- "Indeterminate"; run_audit(y, "bad_lab", FALSE)
y <- x; y$add_rs[1] <- 4; run_audit(y, "bad_score", FALSE)
y <- x; y$algorithm_class[1] <- NA; run_audit(y, "missing_class", FALSE)
y <- x; y$aas_outcome[1] <- NA; run_audit(y, "missing_outcome_label", FALSE)
y <- x; y$aas_positive <- NULL; run_audit(y, "missing_column", FALSE)
run_audit(x[-1, ], "changed_count", FALSE)
y <- x; y$algorithm_class[231] <- "Discordant"; run_audit(y, "bad_low_positive", FALSE)
y <- x; y$algorithm_class[1106] <- "Concordant"; run_audit(y, "bad_low_negative", FALSE)
y <- x; y$algorithm_class[1154] <- "Concordant"; run_audit(y, "bad_missing_score", FALSE)
y <- x; y$d_dimer_status[281] <- "Negative"; y$d_dimer_status[1106] <- "Absent/not obtained"
run_audit(y, "compensating_count_swap", FALSE)

# A second execution must refuse an existing output directory without touching it.
before <- tools::md5sum(list.files(base, full.names = TRUE))
status <- system2(file.path(R.home("bin"), "Rscript"),
                  c("--vanilla", shQuote(script), shQuote(file.path(tmp, "valid.rds")), shQuote(base)),
                  stdout = FALSE, stderr = FALSE)
stopifnot(status != 0L, identical(before, tools::md5sum(list.files(base, full.names = TRUE))))
cat("PASS: valid margins, row and lab-status invariance, thirteen invalid inputs, and overwrite refusal.\n")

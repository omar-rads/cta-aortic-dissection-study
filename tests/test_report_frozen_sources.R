# Exercise the actual source-verification block without loading study data.
code <- parse("R/18_report_alt_dx_v2_finalize.R", keep.source = FALSE)
matches <- vapply(code, function(x) {
  is.call(x) && identical(x[[1]], as.name("for")) &&
    identical(x[[3]], quote(names(frozen$file_sha256)))
}, logical(1))
stopifnot(sum(matches) == 1L)
guard <- code[[which(matches)]]

check_guard <- function() {
  scratch <- tempfile("frozen-source-test-")
  dir.create(scratch)
  previous <- setwd(scratch)
  on.exit(setwd(previous))
  dir.create("R")
  path <- "R/17_report_alt_dx_calibration_ingest.R"
  writeLines("synthetic source", path)
  frozen <- list(file_sha256 = setNames(list(digest::digest(file = path, algo = "sha256")), path))
  expected <- frozen
  evaluate <- function() eval(guard, envir = list2env(list(frozen = frozen)))
  expect_error <- function(pattern) {
    message <- tryCatch({ evaluate(); "" }, error = conditionMessage)
    stopifnot(grepl(pattern, message, fixed = TRUE), identical(frozen, expected))
  }
  evaluate()
  stopifnot(identical(frozen, expected))
  writeLines("changed synthetic source", path)
  expect_error("Frozen source hash changed")
  names(frozen$file_sha256) <- "R/20_report_alt_dx_calibration_ingest.R"
  expected <- frozen
  expect_error("Frozen source file is missing")
  names(frozen$file_sha256) <- "docs/report_alt_dx_v1_v2_change_log.md"
  expected <- frozen
  expect_error("Frozen source file is missing")
}
check_guard()
cat("Frozen R source checks passed: matching, changed, old-layout and missing sources.\n")

# tests/run_r_tests.R
# StatLab Desktop R Test Runner (Pure Base R)

local({
  if (!dir.exists("tests")) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0L) {
      script_path <- normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE)
      repo_root <- dirname(dirname(script_path))
      if (dir.exists(file.path(repo_root, "tests"))) {
        setwd(repo_root)
      }
    }
  }
})

.sl_test_state <- new.env(parent = emptyenv())
.sl_test_state$passed <- 0L
.sl_test_state$failed <- 0L
.sl_test_state$current_file <- "<interactive>"

assert_true <- function(cond, msg = "") {
  if (isTRUE(cond)) {
    .sl_test_state$passed <- .sl_test_state$passed + 1L
    invisible(TRUE)
  } else {
    .sl_test_state$failed <- .sl_test_state$failed + 1L
    label <- if (nzchar(msg)) msg else deparse(substitute(cond))[1L]
    cat(sprintf(
      "[FAIL] %s: %s (got: %s)\n",
      .sl_test_state$current_file,
      label,
      paste(deparse(cond), collapse = " ")
    ))
    invisible(FALSE)
  }
}

assert_near <- function(actual, expected, tol = 1e-8, msg = "") {
  label <- if (nzchar(msg)) {
    msg
  } else {
    sprintf("%s ~= %s", deparse(substitute(actual))[1L], deparse(substitute(expected))[1L])
  }
  ok <- is.numeric(actual) && is.numeric(expected) &&
    length(actual) == length(expected) && length(actual) > 0L &&
    all(is.finite(actual)) && all(is.finite(expected))
  if (ok) {
    max_diff <- max(abs(as.numeric(actual) - as.numeric(expected)))
    if (max_diff <= tol) {
      .sl_test_state$passed <- .sl_test_state$passed + 1L
      return(invisible(TRUE))
    }
    .sl_test_state$failed <- .sl_test_state$failed + 1L
    cat(sprintf(
      "[FAIL] %s: %s (max |actual - expected| = %.3e > tol %.3e; actual = %s, expected = %s)\n",
      .sl_test_state$current_file,
      label,
      max_diff,
      tol,
      paste(format(actual, digits = 12L), collapse = ", "),
      paste(format(expected, digits = 12L), collapse = ", ")
    ))
    return(invisible(FALSE))
  }
  .sl_test_state$failed <- .sl_test_state$failed + 1L
  cat(sprintf(
    "[FAIL] %s: %s (invalid/non-finite or mismatched numeric inputs: actual = %s, expected = %s)\n",
    .sl_test_state$current_file,
    label,
    paste(deparse(actual), collapse = " "),
    paste(deparse(expected), collapse = " ")
  ))
  invisible(FALSE)
}

if (dir.exists("R")) {
  r_files <- sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE))
  for (rf in r_files) {
    tryCatch(
      source(rf, local = FALSE, encoding = "UTF-8"),
      error = function(e) {
        cat(sprintf("[FAIL] Error sourcing %s: %s\n", rf, conditionMessage(e)))
        quit(save = "no", status = 1L)
      }
    )
  }
}

test_files <- sort(list.files("tests", pattern = "^test_.*\\.[Rr]$", full.names = TRUE))
if (length(test_files) == 0L) {
  cat("[FAIL] No test files matching tests/test_*.R found.\n")
  quit(save = "no", status = 1L)
}

for (tf in test_files) {
  .sl_test_state$current_file <- basename(tf)
  before_pass <- .sl_test_state$passed
  before_fail <- .sl_test_state$failed
  test_env <- new.env(parent = .GlobalEnv)
  tryCatch(
    source(tf, local = test_env, encoding = "UTF-8"),
    error = function(e) {
      .sl_test_state$failed <- .sl_test_state$failed + 1L
      cat(sprintf("[FAIL] %s (ERROR): %s\n", basename(tf), conditionMessage(e)))
    }
  )
  file_pass <- .sl_test_state$passed - before_pass
  file_fail <- .sl_test_state$failed - before_fail
  status_tag <- if (file_fail == 0L) "PASS" else "FAIL"
  cat(sprintf(
    "[%s] %s (%d passed, %d failed)\n",
    status_tag, basename(tf), file_pass, file_fail
  ))
}

if (.sl_test_state$failed > 0L) {
  cat(sprintf(
    "\nSummary: FAIL (%d passed, %d failures)\n",
    .sl_test_state$passed, .sl_test_state$failed
  ))
  quit(save = "no", status = 1L)
} else {
  cat(sprintf(
    "\nSummary: PASS (%d passed, 0 failures)\n",
    .sl_test_state$passed
  ))
  quit(save = "no", status = 0L)
}

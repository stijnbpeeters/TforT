# `devtools::test()` loads TforT before running tests. This fallback supports
# `testthat::test_dir("tests/testthat")` without sourcing functions globally.
if (!"package:TforT" %in% search()) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install `pkgload`, or run the tests with `devtools::test()`.")
  }
  package_root <- normalizePath(
    testthat::test_path("..", ".."), mustWork = TRUE
  )
  pkgload::load_all(package_root, export_all = FALSE, quiet = TRUE)
}

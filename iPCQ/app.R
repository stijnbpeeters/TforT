# Development launcher for the packaged iPCQ application.
# Installed users should call TforT::run_ipcq_app().

if (!requireNamespace("TforT", quietly = TRUE)) {
  app_directory <- normalizePath(getwd(), mustWork = TRUE)
  package_root <- if (file.exists(file.path(app_directory, "DESCRIPTION"))) {
    app_directory
  } else {
    normalizePath(file.path(app_directory, ".."), mustWork = TRUE)
  }

  if (!file.exists(file.path(package_root, "DESCRIPTION"))) {
    stop("Cannot find the TforT package root from the iPCQ app directory.",
         call. = FALSE)
  }
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop(paste0(
      "TforT is not installed. Install `pkgload`, then use Run App again; ",
      "or run `devtools::load_all()` followed by `run_ipcq_app()`."
    ), call. = FALSE)
  }

  pkgload::load_all(package_root, quiet = TRUE)
}

TforT::run_ipcq_app()

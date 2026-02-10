# https://github.com/rstudio/renv/issues/472#issuecomment-2759330703
# info on mooving renv to boot folder
Sys.setenv(RENV_PATHS_RENV = "boot/renv")
Sys.setenv(RENV_PATHS_LOCKFILE = "boot/renv.lock")
source("boot/renv/activate.R")

options(
  repos =
    c(
      CRAN = "https://cloud.r-project.org",
      CRANExtra = NULL,
      fishfollower = "https://fishfollower.r-universe.dev",
      flr = "https://flr.r-universe.dev",
      icestoolsprod = "https://ices-tools-prod.r-universe.dev",
      noaa = "https://noaa-fisheries-integrated-toolbox.r-universe.dev"
    )
)

# show number of dependencies:
local({
  if (file.exists("boot/renv.lock")) {
    ndeps <- length(jsonlite::fromJSON("boot/renv.lock")$Packages)
    message("- Number of project dependencies: ", ndeps)
  }
})

# check boot data entries:
local({
  check.boot.data <- function() {
    bib.entries <- TAF::read.bib("boot/DATA.bib")

    # todo: check url, and check if data present but not in DATA.bib
    checks <-
      sapply(bib.entries, function(entry) {
        if (entry$source == "file") {
          file.exists(file.path("boot", "data", entry$key))
        } else if (entry$source %in% c("folder", "script")) {
          if (dir.exists(file.path("boot", "data", entry$key))) {
            # check if folder/script contains any files
            length(list.files(file.path("boot", "data", entry$key), recursive = TRUE)) > 0
          } else {
            FALSE
          }
        } else {
          NA
        }
      })

    if (any(!checks, na.rm = TRUE)) {
      missing <- names(checks)[!checks]
      message(
        "- Project boot folder is out of sync:\n  The following data entries are missing:\n",
        paste("  -", missing, collapse = "\n"),
        "\n  Run `TAF::taf.boot()` to update the boot folder with the missing data entries."
      )
    } else {
      message("- All data entries in boot/DATA.bib are present")
    }

    invisible(checks)
  }

  if (requireNamespace("TAF", quietly = TRUE)) {
    check.boot.data()
  } else {
    message("- TAF package not available, skipping boot data checks.")
  }
})

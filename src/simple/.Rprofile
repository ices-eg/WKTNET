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
  if (
    requireNamespace("icesTAF", quietly = TRUE) ||
      packageVersion("icesTAF") >= package_version("4.3.1")
  ) {
    icesTAF::check.boot.data()
  } else {
    message("- icesTAF (4.3.1) package not available, skipping boot data checks.")
  }
})

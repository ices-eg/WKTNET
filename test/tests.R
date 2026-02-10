library(qcTAF)
library(renv)

# run tests from qcTAF on templates in src/

# list summary of templates
# list template dependencies

# check that templates renv.lock file is up to date

dependencies <- jsonlite::fromJSON("../src/boot/renv.lock")$Packages

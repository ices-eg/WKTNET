## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(RDBEScore)

mkdir("data")
rdbes <- createRDBESDataObject("boot/data/WebAPI_test/CS_ZW_2022_download.zip")
# validation, QC, processing,
saveRDS(rdbes, "data/rdbes.Rds")

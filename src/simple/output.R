## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)

mkdir("output")
output <- readRDS("model/NatEst.Rds")
# convertion to CEF
saveRDS(output,"output/CEF.Rds")

## Run analysis, write model results

## Before:
## After:

library(icesTAF)

mkdir("model")
model <- readRDS("data/rdbes.Rds")
# rasing
saveRDS(model, "model/NatEst.Rds")

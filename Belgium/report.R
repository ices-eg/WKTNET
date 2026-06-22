
rm(list=ls())
gc()

library(icesTAF)
library(icesSAG)
library(icesASD)
library(rmarkdown)
library(Cairo)

# source.taf("report")
source("boot/data/Utilities.R")

# Load nested inputs and model outputs
load(file = "data/input_data_AllStocks.Rdata")
load(file = "model/raised_outputs_AllStocks.Rdata")
load(file = "output/all_tables.RData")


mkdir("report")


cp("boot/initial/report/*", "report/")
outdir <- "report/"
# istock <- "whg.27.47d"
# istock <- "meg.27.7b-k8abd"
# istock <- "mon.27.78abd"
# iraise<-"sol.27.7fg_TBB_DEF_70-99_0_0_2024_YEAR"
# iraise <-"mac.27.nea_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <- "bll.27.3a47de_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <- "her.27.25-2932_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <- "ple.27.420_TBB_DEF_70-99_0_0_2024_Q1Q4"
# Loop over each stock in the all_tables list
for (iraise in names(all_tables)) {
  # Extract the stock code (everything before the first "-")
  istock <- sub("_.*", "", iraise)
  msg(paste("Rendering report for rasing of :", iraise))

  ofile  = paste0("report_", iraise, ".html")
  # Pass the stock name as a parameter to the Rmd
  rmarkdown::render(
    input = "boot/initial/data/report_Raising.Rmd",
        output_dir = outdir,
    output_file = ofile ,
    clean = T,
    quiet = F,
    encoding = "UTF-8",
    params = list(
      istock = istock,
      iraise = iraise,
      stock_inputs  = all_inputs[[iraise]],
      stock_outputs = all_outputs[[iraise]],
      stock_tables  = all_tables[[iraise]]
    ),
    envir = parent.frame()
  )
  
  msg("✅ Rendered:", ofile, "\n")
}

msg("🎉 All per-stock reports generated successfully!\n")

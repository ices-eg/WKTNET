### Run the RAISE TAF scripts

rm(list=ls())
gc()

library(icesTAF)
library(RDBEScore)
library(icesRDBES)

wd<-getwd()
setwd(paste0(wd,"/TAF_national_raising/BEL_TAF_2026"))


#Create an initial draft version of a ‘DATA.bib’ metadata file.
draft.data(file=T)

# Process metadata files ‘SOFTWARE.bib’ and ‘DATA.bib’ to set up software and data files required for the analysis.
taf.boot()

#Run core TAF scripts in current directory.
source.all()

# Compare with InterCatch raising outputs
source("Validation_Q.R")

# upload the HNI and HEN files
source("Upload.R")





iraise<-"whg.27.7b-ce-k_TBB_DEF_70-99_0_0_2024_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")


iraise<-"whg.27.7b-ce-k_TBB_DEF_70-99_0_0_2025_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"tur.27.4_TBB_DEF_70-99_0_0_2024_Q1Q4"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"tur.27.4_TBB_DEF_70-99_0_0_2025_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"sol.27.7fg_TBB_DEF_70-99_0_0_2024_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"sol.27.7fg_TBB_DEF_70-99_0_0_2025_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"sol.27.7a_TBB_DEF_70-99_0_0_2024_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"sol.27.7a_TBB_DEF_70-99_0_0_2025_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")


iraise<-"ple.27.420_TBB_DEF_70-99_0_0_2025_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"ple.27.420_TBB_DEF_70-99_0_0_2024_Q1Q4"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"pil.27.8c9a_TBB_DEF_70-99_0_0_2024_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"mac.27.nea_TBB_DEF_70-99_0_0_2024_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"her.27.25-2932_TBB_DEF_70-99_0_0_2024_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"bll.27.3a47de_TBB_DEF_70-99_0_0_2024_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")

iraise<-"bll.27.3a47de_TBB_DEF_70-99_0_0_2025_YEAR"
hni_file     <- paste0("output/HNI_", iraise, ".csv")
effort_file  <- paste0("output/HEN_", iraise, ".csv")
result <- rdbes_upload_data(hni_file, hierarchy = "HNI")
result <- rdbes_upload_data(effort_file, hierarchy = "HEN")



#install.packages('icesRDBES', repos = c('https://ices-tools-prod.r-universe.dev', 'https://cloud.r-project.org'))
#install.packages('icesASD', repos = c('https://ices-tools-prod.r-universe.dev', 'https://cloud.r-project.org'))

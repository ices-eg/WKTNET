
## data.R 

library(icesTAF)
library(icesSD)
library(RDBEScore)
library(TAF)
library(icesSAG)

library(remotes)
library(data.table)
library(readxl)
library(RODBC)
library(icesVocab)
library(reshape2)
library(ggplot2)
library(dplyr)
library(readxl)


mkdir("data")


MasterTable <- read_excel("./boot/data/MasterTable.xlsx", sheet = "MasterTable")
source("./boot/data/Utilities.r")

CE <- createRDBESDataObject(input= file.path("boot/data/downloadRDBES", basename(CE_zip_pad)), listOfFileNames = list("CE" = read.csv("boot/data/downloadRDBES/CommercialEffort.csv")),verbose=TRUE)
CL <- createRDBESDataObject(input= file.path("boot/data/downloadRDBES", basename(CL_zip_pad)), listOfFileNames = list("CE" = read.csv("boot/data/downloadRDBES/CommercialLanding.csv")),verbose=TRUE)
CS <- createRDBESDataObject(input= file.path("boot/data/downloadRDBES", basename(CS_zip_pad)), listOfFileNames = list("DE" = "/boot/data/downloadRDBES/Design.csv",
                                                                                                                      "BV" = "/boot/data/downloadRDBES/BiologicalVariable.csv",
                                                                                                                      "SS" = "/boot/data/downloadRDBES/SpeciesSelection.csv",
                                                                                                                      "FO" = "/boot/data/downloadRDBES/FishingOperation.csv",
                                                                                                                      "FT" = "/boot/data/downloadRDBES/FishingTrip.csv",
                                                                                                                      "FM" = "/boot/data/downloadRDBES/FrequencyMeasure.csv",
                                                                                                                      "SA" = "/boot/data/downloadRDBES/Sample.csv",
                                                                                                                      "SD" = "/boot/data/downloadRDBES/SamplingDetails.csv"
                                                                                                                        ),verbose=TRUE)
saveRDS(CE, "data/CE.Rds")
saveRDS(CL, "data/CL.Rds")
saveRDS(CS, "data/CS.Rds")


# data script

# get RDBES CL, CE and CS  zip files
CL<-readRDS(file = paste0("data/CL.Rds"))
CE<-readRDS(file = paste0("data/CE.Rds"))
CS<-readRDS(file = paste0("data/CS.Rds"))

validateRDBESDataObject(CS, verbose = FALSE)
validateRDBESDataObject(CL, verbose = FALSE)
validateRDBESDataObject(CE, verbose = FALSE)


# change to column names to full names
fieldNameMapping <- getFieldNameMapping(downloadFromGitHub= FALSE, fileLocation = './boot/data/tableDefs/')   # file name for CS excel should be "RDBES Data Model", change!!!
CS <- changeFieldNames(RDBESdata = CS, fieldNameMap = fieldNameMapping, typeOfChange = "RtoDB")

library(icesSD)

# get 
stock_info <- getSD(year = 2025) |> 
  dplyr::select(StockKeyLabel, SpeciesScientificName, ExpertGroup)

# Prepare storage list for all stocks
all_inputs <- list()

MasterTable <- MasterTable %>% filter(YearQuarter_LAN != "XX")

# 2025 data are not yet available through api
MasterTable<-MasterTable[MasterTable$year %in% c("2024","2025"),]

irow<-8

  # Loop over selected rows in MasterTable
for (irow in seq(1,nrow(MasterTable),1)) {
  # Extract metadata from MasterTable
  extractMasterTableVariables(MasterTable, irow)
  iYQ_comb <- combine_iYQ(iYQ$Lan, iYQ$Dis)
  # Outputs: istock, imet, iareas, iFAO, iyear
  msg("Getting input data for raising of : ",istock,"-",iyear,"-",iYQ_comb,"-",imet)
  # Filter datasets
    CE_stock <- filter_CE(CE, iyear, iareas, imet)
    CL_stock <- filter_CL(CL, iyear, iareas, imet, iFAO)
    FT_stock <- filter_FT(CS, iyear, iareas, imet) 
    FO_stock <- filter_FO(CS, iyear, iareas, imet)
    
    SA_stock <- filter_SA(CS, iyear, iareas, imet, iFAO)
    FM_stock <- filter_FM(CS, iyear, iareas, imet, iFAO)
    BV_stock <- filter_BV(CS, iyear, iareas, imet, iFAO)

  
  NumLength_all <- list()
  NumAge_all <- list()
  NumWeight_all <- list()
  
  
  # Usage
  for (catchCat in names(iYQ)) {
    iYQ_vec <- iYQ[[catchCat]]
    
    for (iyq in iYQ_vec) {
      
      # msg(paste0("Calculating number of samples (Length, Weight and Age) in ", iyear, " for ", catchCat, " of ", istock, "\n"))
      
      if (iyq == "YEAR") {
        # Yearly aggregation
        NumLength <- safe_aggregate_simple(FM_stock, FMnumberAtUnit ~ SAcatchCat, catchCat = catchCat)
        NumAge    <- safe_aggregate_simple(BV_stock, BVnumberTotal ~ SAcatchCat, subset = "Age", catchCat = catchCat)
        NumWeight <- safe_aggregate_simple(BV_stock, BVnumberTotal ~ SAcatchCat, subset = "WeightLive", catchCat = catchCat)
        
        NumLength$Quarter <- "Year"
        NumAge$Quarter <- "Year"
        NumWeight$Quarter <- "Year"
        
      }  else {
        # Quarterly aggregation
        iquarters <- as.numeric(gsub("Q", "", unlist(regmatches(iyq, gregexpr("Q\\d+", iyq)))))
        
        NumLength <- do.call(rbind, lapply(iquarters, function(q) {
          df_q <- FM_stock[FM_stock$Quarter == q, ]
          agg <- safe_aggregate_simple(df_q, FMnumberAtUnit ~ SAcatchCat, catchCat = catchCat)
          agg$Quarter <- q
          agg
        }))
        
        NumAge <- do.call(rbind, lapply(iquarters, function(q) {
          df_q <- BV_stock[BV_stock$Quarter == q & BV_stock$BVtypeMeasured == "Age", ]
          agg <- safe_aggregate_simple(df_q, BVnumberTotal ~ SAcatchCat, subset = "Age", catchCat = catchCat)
          agg$Quarter <- q
          agg
        }))
        
        NumWeight <- do.call(rbind, lapply(iquarters, function(q) {
          df_q <- BV_stock[BV_stock$Quarter == q & BV_stock$BVtypeMeasured == "WeightLive", ]
          agg <- safe_aggregate_simple(df_q, BVnumberTotal ~ SAcatchCat, subset = "WeightLive", catchCat = catchCat)
          agg$Quarter <- q
          agg
        }))
      }
      
      # Store results
      NumLength_all[[paste0(catchCat, "_", iyq)]] <- NumLength
      NumAge_all[[paste0(catchCat, "_", iyq)]] <- NumAge
      NumWeight_all[[paste0(catchCat, "_", iyq)]] <- NumWeight
    }
  }
  
  # --- Combine Lan + Dis into single dataframes ---
  NumLength <- bind_rows(NumLength_all)
  NumAge    <- bind_rows(NumAge_all)
  NumWeight <- bind_rows(NumWeight_all)
  
  # Store trips (optional)
  Trips <- build_trips_df(FO_stock, FT_stock, SA_stock, FM_stock, BV_stock, iYQ = iYQ$Dis)
  
  # Save everything into a single nested list per stock
  stock_inputs <- list(
    metadata = list(
      istock = istock,
      iyear = iyear,
      iYQ = iYQ,
      iYQ_comb = iYQ_comb,
      iRaisingLevel = iRaisingLevel ,
      iLWK =  iweightparam ,
      iLWK_source =  iLWK_source ,
      iLWK_a =  iLWK_a ,
      iLWK_b =  iLWK_b ,
      iWG = iWG ,
      iWG_nice = iWG_nice ,
      ispecies = ispecies ,
      iAphiaID = iAphiaID ,
      iFAO= iFAO ,
      imet = imet,
      iareas = iareas,
      iiareas = iiareas
    ),
    CE_all = CE,
    CL_all = CL,
    CE_stock = CE_stock,
    CL_stock = CL_stock,
    FT_stock = FT_stock,
    FO_stock = FO_stock,
    SA_stock = SA_stock,
    FM_stock = FM_stock,
    BV_stock = BV_stock,
    NumLength = NumLength,
    NumAge = NumAge,
    NumWeight = NumWeight,
    Trips = Trips
  )
  
  all_inputs[[paste0(istock,"_",imet,"_",iyear,"_",iYQ_comb)]] <- stock_inputs
}

# Save the nested input list
save(all_inputs,
     file = paste0("data/input_data_AllStocks.RData"))

cat("✅ Input preparation completed. Nested input list saved.\n")




# model script
rm(list=ls())
gc()

library(remotes)
#install_github("ices-tools-dev/RDBEScore@dev")
library(RDBEScore)
library(icesTAF)
library(data.table)
library(readxl)
library(RODBC)
library(TAF)
library(icesVocab)
library(dplyr)
library(reshape2)
library(ggplot2)
library(FSA)
library(nnet)
library(tidyr)

#irepo <- "./TAF_national_raising/BEL_TAF_Combined_Raising/"
load(file = "data/input_data_AllStocks.Rdata")
source(paste0("boot/data/Utilities.R"))
#irepo_IC <- "../../1. NDGP/"

# Prepare storage list for all outputs
all_outputs <- list()
iraise<-"sol.27.7fg_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <- "her.27.25-2932_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <-"mac.27.nea_TBB_DEF_70-99_0_0_2024_YEAR" 
iraise <-"bll.27.3a47de_TBB_DEF_70-99_0_0_2024_YEAR" 
iraise <- "ple.27.420_TBB_DEF_70-99_0_0_2024_Q1Q4"

iraise<-"sol.27.8ab_TBB_DEF_70-99_0_0_2024_Q2"
# Loop over all stocks
for (iraise in names(all_inputs)) {
  
  # Extract the stock code (everything before the first "-")
  istock <- sub("_.*", "", iraise)
  
  msg("Processing raising of :", iraise, "\n")
  
  # Extract stock-specific data
  stock_data <- all_inputs[[iraise]]
  
  # Check if ALL core input tables are empty
  is_empty_stock <- all(
    nrow(stock_data$CE_stock) == 0,
    nrow(stock_data$CL_stock) == 0,
    nrow(stock_data$FT_stock) == 0,
    nrow(stock_data$FO_stock) == 0,
    nrow(stock_data$SA_stock) == 0,
    nrow(stock_data$FM_stock) == 0,
    nrow(stock_data$BV_stock) == 0
  )
    
  if (is_empty_stock) {
    msg("⚠️ No RDBES data available for raising of : ", iraise, " skipping raising but saving empty structure.\n")
      # next  # continue to next stock
  }
  # Extract metadata
  #### TEMPORAL DATA ####
  iyear  <- stock_data$metadata$iyear
  iYQ  <- stock_data$metadata$iYQ
  iYQ_comb <- stock_data$metadata$iYQ_comb
  #### METIER DATA ####
  imet   <- stock_data$metadata$imet
  
  #### WHAT TO RAISE DATA #### 
  ########  WEIGHTS/ LENGTHS /LWK/AGE / ALL
  iRaisingLevel   <- stock_data$metadata$iRaisingLevel
  ########  calc LWK or other_source LWK
  iLWK <- stock_data$metadata$iLWK
  iLWK_source <- stock_data$metadata$iLWK_source
  iLWK_a <- stock_data$metadata$iLWK_a
  iLWK_b <- stock_data$metadata$iLWK_b
  
  iareas <- stock_data$metadata$iareas
  iFAO   <- stock_data$metadata$iFAO
  
  # Extract datasets
  CE_stock <- stock_data$CE_stock
  CL_stock <- stock_data$CL_stock
  FT_stock <- stock_data$FT_stock
  FO_stock <- stock_data$FO_stock
  SA_stock <- stock_data$SA_stock
  FM_stock <- stock_data$FM_stock
  BV_stock <- stock_data$BV_stock
  
  NumLength <- stock_data$NumLength
  NumWeight    <- stock_data$NumWeight
  NumAge    <- stock_data$NumAge

  
  #########################################################
  # RAISING OF WEIGHTS ####################################
  #########################################################
  
  # With discard raising
  RW <- raiseWeights(SA_stock,  FO_stock, CL_stock,
                     iRaisingLevel = iRaisingLevel,
                     method = c("bootstrap"),
                     B = 2000,
                     seed = 123,
                     iYQ = iYQ_comb)
  
  LW <- RW$LW
  DW <- RW$DW  # Will be 0 if discards not raised
  
  # Check if discards were raised
  if (all(is.na(RW$DR))) {
    cat("Discards not raised - using landings only\n")
  } else {
    cat("Discards raised successfully\n")
  }
  
  #########################################################
  # RAISING OF LENGTHS ####################################
  #########################################################
  # Initialize a list to store outputs
  LF_list <- list()
  
  for(catchCat in names(iRaisingLevel)) {
    iRaisingLevel_Cat <- iRaisingLevel[[catchCat]]
    iYQ_Cat <- if(!is.null(iYQ[[catchCat]])) iYQ[[catchCat]] else "YEAR"
        if(iRaisingLevel_Cat %in% c("LENGTHS", "LWK", "ALK", "ALL")) {
          
      LF_full <- raiseLengths(SA_stock, FO_stock, RW_stock = RW , FM_stock, BV_stock, iRaisingLevel = iRaisingLevel_Cat, iYQ = iYQ_Cat,catchCat = catchCat )
      # Filter the result by catch category
      LF_list[[catchCat]] <- LF_full[LF_full$SAcatchCat == catchCat, ]
    } else {
      LF_list[[catchCat]] <- NULL
    }
  }
  
  # Bind all catch categories into a single dataframe
  LF <- do.call(rbind, LF_list)
  rownames(LF) <- NULL
  
  #########################################################
  # RAISING OF LENGTHS AT LENGTH AND AGE ##################
  #########################################################
  
  # Initialize lists to store results
  for(catchCat in names(iRaisingLevel)) {
    iRaisingLevel_Cat <- iRaisingLevel[[catchCat]]
    iYQ_Cat <- iYQ[[catchCat]]
    iLWK_Cat <- iLWK[[catchCat]]
    iLWK_a_Cat <- iLWK_a[[catchCat]]
    iLWK_b_Cat <- iLWK_b[[catchCat]]
    iLWK_source_Cat <- iLWK_source[[catchCat]]
    
    catShort <- ifelse(tolower(catchCat) == "lan", "L", "D")  # L = Lan, D = Dis
    
    if(iRaisingLevel_Cat %in% c("LWK", "ALK", "ALL")) {
      
      # Generate LWK for this catch category
      lwk_obj  <- generate_LWK(
        LF, BV_stock, catchCat = catchCat, 
        iYQ = iYQ_Cat,
        iLWK = iLWK_Cat,
        iLWK_a = iLWK_a_Cat,
        iLWK_b = iLWK_b_Cat,
        iLWK_source = iLWK_source_Cat
      )
      
      assign(paste0("LWK_", catShort), lwk_obj)
      
      weight_vec <- if (catchCat == "Lan") {
        LW
      } else {
        DW
      }
      # ----- SOP -----
      sop_obj <- SOP_correction_LF(
        LF = LF, LWK = lwk_obj, total_weight = weight_vec, catchCat = catchCat,
        iYQ = iYQ_Cat,
        iLWK = iLWK_Cat,
        bootstrap = FALSE, nboot = 1000
      )
      
      assign(paste0("SOP_", catShort), sop_obj)
      
      
     if(iRaisingLevel_Cat %in% c("ALK", "ALL")) {
        # ----- ALK -----
        alk_obj <- generate_ALK(
          LF, BV_stock, catchCat = catchCat,
          iYQ = iYQ_Cat, plot = TRUE
        )
        assign(paste0("ALK_", catShort), alk_obj)
        
        # ----- NAA -----
        naa_obj <- Apply_ALK(
          LF, lwk_obj, alk_obj, sop_obj,
          catchCat = catchCat,
          iYQ = iYQ_Cat
        )
        assign(paste0(catShort,"NAA"), naa_obj)
        
      } else {
        assign(paste0("ALK_", catShort), NULL)
        assign(paste0(catShort,"NAA"), NULL)
      }
      
    } else {
      # Fill placeholders for missing raising levels
      assign(paste0("LWK_", catShort), NULL)
      assign(paste0("SOP_", catShort), NULL)
      assign(paste0("ALK_", catShort), NULL)
      assign(paste0(catShort,"NAA"), NULL)
    }
  }
  

  #########################################################
  # Store outputs for this stock
  stock_outputs <- list(
    metadata = stock_data$metadata,  # Keep metadata handy
    RW = RW,
    LW = LW,
    DW = DW,
    LF = LF,
    LWK_L = LWK_L,
    SOP_L = SOP_L,
    LWK_D = LWK_D,
    SOP_D = SOP_D,
    ALK_LAN = ALK_L,
    LNAA = LNAA,
    ALK_DIS = ALK_D,
    DNAA = DNAA
  )
  
  all_outputs[[iraise]] <- stock_outputs
}
mkdir("model")

# Save all stocks' outputs
save(
  all_outputs,
  file = ("model/raised_outputs_AllStocks.Rdata")
)

cat("✅ All saisings processed and saved.\n")

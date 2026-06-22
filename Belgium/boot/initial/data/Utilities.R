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

extractMasterTableVariables <- function(MasterTable, irow) {
  istock  <<- MasterTable$stocks[irow]
  imet    <<- MasterTable$metier[irow]
  iFAO    <<- MasterTable$FAO[irow]
  iFAO <<- strsplit(iFAO, ", ")[[1]]
  iyear   <<- MasterTable$year[irow]
  
  iWG     <<- MasterTable$WG[irow]
  iWG_nice <<- MasterTable$WG_nice[irow]
  ispecies <<- MasterTable$SpeciesName[irow]
  ispecies <<- strsplit(ispecies, ", ")[[1]]
  iAphiaID <<- MasterTable$AphiaID[irow]
  #iAphiaID <<- strsplit(iAphiaID, ", ")[[1]]
  # --- NEW: pair LAN/DIS values ---
  iRaisingLevel <<- list(Lan = MasterTable$Raising_Level_LAN[irow],
                         Dis = MasterTable$Raising_Level_DIS[irow])
  iYQ <<- list(Lan = MasterTable$YearQuarter_LAN[irow],
               Dis = MasterTable$YearQuarter_DIS[irow])
  iLWK_a <<- list(Lan = MasterTable$LWK_A_LAN[irow],
                  Dis = MasterTable$LWK_A_DIS[irow])
  iLWK_b <<- list(Lan = MasterTable$LWK_B_LAN[irow],
                  Dis = MasterTable$LWK_B_DIS[irow])
  iLWK_source <<- list(Lan = MasterTable$LWK_source_LAN[irow],
                       Dis = MasterTable$LWK_source_DIS[irow])
  iweightparam <<- list(Lan = MasterTable$weightparam_LAN[irow],
                        Dis = MasterTable$weightparam_DIS[irow])
  iiareas <<- MasterTable[irow, ]$AllAreas
  iareas <<- strsplit(iiareas, ", ")[[1]]
}

combine_iYQ <- function(a, b) {
  # helper to extract quarter tokens or YEAR
  extract_tokens <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(character(0))
    x <- as.character(x)
    # find Q# tokens
    qtokens <- unlist(regmatches(x, gregexpr("Q\\d+", x)))
    # also consider YEAR (case-insensitive)
    year_flag <- any(grepl("\\bYEAR\\b", x, ignore.case = TRUE))
    tokens <- unique(qtokens)
    if (year_flag) tokens <- unique(c(tokens, "YEAR"))
    tokens
  }
  
  toks <- unique(c(extract_tokens(a), extract_tokens(b)))
  if (length(toks) == 0) return(character(0))
  
  # if YEAR present, return YEAR only (you can change that behavior if needed)
  if ("YEAR" %in% toks) return("YEAR")
  
  # sort tokens by numeric quarter
  ord <- order(as.numeric(sub("Q", "", toks)))
  toks_sorted <- toks[ord]
  
  # collapse without separator: Q1Q2Q3
  paste0(toks_sorted, collapse = "")
}

filter_CE<-function(CE, iyear, iareas, imet) {
  # Filter 
  CE<-CE$CE
  CE_stock <- CE %>%
    filter(CEyear == iyear) %>%
    filter(CEarea %in% iareas) %>%
    filter(CEmetier6 == imet)
}

filter_CL <- function(CL, iyear, iareas, imet, iFAO) {
  # Extract CL table
  CL <- CL$CL
  
  # Basic filter
  CL_stock <- CL %>%
    dplyr::filter(
      CLyear == iyear,
      CLarea %in% iareas,
      CLmetier6 == imet,
      CLspecFAO %in% iFAO
    )
  # --- Default: return full filtered table for the year ---
  return(CL_stock)
}


filter_FT <- function(CS, iyear, iareas, imet) {
  FT <- CS$FT
  FO_FT <- CS$FO
  
  FT$Year <- lubridate::year(FT$FTarrivalDate)
  FT$Quarter <- lubridate::quarter(FT$FTarrivalDate)
  # Get FishingTrips IDs for this stock x metier x area (do not change)
  metier_area_trips <- unique(FO_FT[FO_FT$FOmetier6 == imet & FO_FT$FOarea %in% iareas,]$FTid)
  
  # Filter FT by year and selected trips
  FT_stock <- FT %>%
    dplyr::filter(Year == iyear, FTid %in% metier_area_trips)
  
  return(FT_stock)
}


filter_FO <- function(CS, iyear, iareas, imet) {
  FO <- CS$FO
  FT_FO <- CS$FT
  
  FO$Year <- lubridate::year(FO$FOendDate)
  FO$Quarter <- lubridate::quarter(FO$FOendDate)
  FO$FTunitName <- FT_FO$FTunitName[match(FO$FTid, FT_FO$FTid)]
  
  # Filter FO by year, area, metier
  FO_stock <- FO %>%
    dplyr::filter(Year == iyear,
                  FOarea %in% iareas,
                  FOmetier6 == imet)
  return (FO_stock )
}

filter_SA<-function(CS, iyear, iareas, imet,iFAO , iYQ = "YEAR") {
  
  # get FishingTrip IDs for SS from FO table
  SS<-CS$SS
  FO_SA<-CS$FO
  FO_SA$Year<-year(FO_SA$FOendDate)
  FO_SA$Quarter<-quarter(FO_SA$FOendDate)
  
  FT_FO<-CS$FT
  SA<-CS$SA
  SS$FTid<-FO_SA$FTid[match(SS$FOid,FO_SA$FOid)]
  FO_SA$FTunitName<-FT_FO$FTunitName[match(FO_SA$FTid,FT_FO$FTid)]
  SS$FTunitName<-FO_SA$FTunitName[match(SS$FOid,FO_SA$FOid)]
  
  
  # get FishingTrip IDs for SA from SS table
  SA$FOid<-SS$FOid[match(SA$SSid,SS$SSid)]
  SA$FTid<-SS$FTid[match(SA$SSid,SS$SSid)]
  SA$FTunitName<-SS$FTunitName[match(SA$SSid,SS$SSid)]
  SA$Year<-FO_SA$Year[match(SA$FOid,FO_SA$FOid)]
  SA$Quarter<-FO_SA$Quarter[match(SA$FOid,FO_SA$FOid)]
  
  
  SA_stock <- SA %>% 
    filter(Year == iyear) %>% 
    filter(SAarea %in% iareas) %>% 
    filter(SAmetier6 == imet) %>% 
    filter(SAspeciesCodeFAO%in%iFAO)
  
  # Return only the filtered SA_stock
  return(SA_stock)
}



filter_FM<-function(CS, iyear, iareas, imet,iFAO) {
  SS<-CS$SS
  SA<-CS$SA
  FM<-CS$FM
  
  FO_SA<-CS$FO
  FO_SA$Year<-year(FO_SA$FOendDate)
  FO_SA$Quarter<-quarter(FO_SA$FOendDate)
  FT_FO<-CS$FT
  
  SS$FTid<-FO_SA$FTid[match(SS$FOid,FO_SA$FOid)]
  FO_SA$FTunitName<-FT_FO$FTunitName[match(FO_SA$FTid,FT_FO$FTid)]
  SS$FTunitName<-FO_SA$FTunitName[match(SS$FOid,FO_SA$FOid)]
  
  
  # get FishingTrip IDs for SA from SS table
  SA$FOid<-SS$FOid[match(SA$SSid,SS$SSid)]
  SA$FTid<-SS$FTid[match(SA$SSid,SS$SSid)]
  
  
  SA$FTunitName<-SS$FTunitName[match(SA$SSid,SS$SSid)]
  SA$Year<-FO_SA$Year[match(SA$FOid,FO_SA$FOid)]
  SA$Quarter<-FO_SA$Quarter[match(SA$FOid,FO_SA$FOid)]
  
  # get extra info into Frequency measure table
  FM$Year<-SA$Year[match(FM$SAid,SA$SAid)]
  FM$Quarter<-SA$Quarter[match(FM$SAid,SA$SAid)]
  FM$SAarea<-SA$SAarea[match(FM$SAid,SA$SAid)]
  FM$SAmetier6<-SA$SAmetier6[match(FM$SAid,SA$SAid)]
  FM$SAspeciesCodeFAO <-SA$SAspeciesCodeFAO [match(FM$SAid,SA$SAid)]
  FM$FTid<-SA$FTid[match(FM$SAid,SA$SAid)]
  FM$FTunitName<-SA$FTunitName[match(FM$SAid,SA$SAid)]
  FM$SAcatchCat<-SA$SAcatchCat[match(FM$SAid,SA$SAid)]
  
  # --- Filter FM table to stock
  FM_stock <- FM %>% 
    filter(Year == iyear) %>%
    filter(SAarea %in% iareas) %>% 
    filter(SAmetier6 == imet) %>% 
    filter(SAspeciesCodeFAO%in%iFAO)
  
  # Return only the filtered FM_stock
  return(FM_stock)
}



filter_BV<-function(CS, iyear, iareas, imet,iFAO) {
  
  SS<-CS$SS
  SA<-CS$SA
  FM<-CS$FM
  BV<-CS$BV
  
  FO_SA<-CS$FO
  FO_SA$Year<-year(FO_SA$FOendDate)
  FO_SA$Quarter<-quarter(FO_SA$FOendDate)
  FT_FO<-CS$FT
  SS$FTid<-FO_SA$FTid[match(SS$FOid,FO_SA$FOid)]
  FO_SA$FTunitName<-FT_FO$FTunitName[match(FO_SA$FTid,FT_FO$FTid)]
  SS$FTunitName<-FO_SA$FTunitName[match(SS$FOid,FO_SA$FOid)]
  
  
  # get FishingTrip IDs for SA from SS table
  SA$FOid<-SS$FOid[match(SA$SSid,SS$SSid)]
  SA$FTid<-SS$FTid[match(SA$SSid,SS$SSid)]
  SA$FTunitName<-SS$FTunitName[match(SA$SSid,SS$SSid)]
  SA$Year<-FO_SA$Year[match(SA$FOid,FO_SA$FOid)]
  SA$Quarter<-FO_SA$Quarter[match(SA$FOid,FO_SA$FOid)]  
  
  
  # get info from sample table into BV table
  BV$Year<-SA$Year[match(BV$SAid,SA$SAid)]
  BV$Quarter<-SA$Quarter[match(BV$SAid,SA$SAid)]
  BV$SAarea<-SA$SAarea[match(BV$SAid,SA$SAid)]
  BV$SAmetier6<-SA$SAmetier6[match(BV$SAid,SA$SAid)]
  BV$SAspeciesCodeFAO<-SA$SAspeciesCodeFAO[match(BV$SAid,SA$SAid)]
  BV$FTid<-SA$FTid[match(BV$SAid,SA$SAid)]
  BV$FTunitName<-SA$FTunitName[match(BV$SAid,SA$SAid)]
  BV$SAcatchCat<-SA$SAcatchCat[match(BV$SAid,SA$SAid)]
  
  # --- Filter BV table to stock
  BV_stock <- BV %>% 
    filter(Year == iyear) %>% 
    filter(SAarea %in% iareas) %>% 
    filter(SAmetier6 == imet) %>% 
    filter(SAspeciesCodeFAO%in%iFAO)
  
}
# --- Helper function ---
safe_aggregate_simple <- function(df, formula, subset = NULL, catchCat = NULL) {
  agg_col <- as.character(formula[[2]])
  
  # Filter by catch category (Lan or Dis)
  if (!is.null(catchCat)) {
    df <- df %>% filter(SAcatchCat == catchCat)
  }
  
  # Apply subset if provided (e.g. "Age", "WeightLive")
  if (!is.null(subset)) {
    df <- df %>% filter(BVtypeMeasured == subset)
  }
  
  # If empty, return row with 0 for that category
  if (nrow(df) == 0) {
    return(data.frame(
      SAcatchCat = catchCat,
      value = 0,
      stringsAsFactors = FALSE
    ) %>% rename(!!agg_col := value))
  }
  
  # Aggregate
  agg <- aggregate(formula, data = df, FUN = sum)
  return(agg)
}



build_trips_df <- function(FO_stock, FT_stock, SA_stock, FM_stock, BV_stock, iYQ = "YEAR") {
  
  make_trip_df <- function(df) {
    # Handle empty df or missing FTunitName
    if (is.null(df) || nrow(df) == 0 || !"FTunitName" %in% names(df)) {
      return(data.frame(Trip = character(0), Quarter = character(0)))
    }
    
    if (!is.null(iYQ) && toupper(iYQ) != "YEAR" && "Quarter" %in% names(df)) {
      # Create a data.frame with trip and quarter
      trips_df <- unique(df[, c("FTunitName", "Quarter")])
      names(trips_df) <- c("Trip", "Quarter")
      return(trips_df)
    } else {
      # No quarter info, just list trips
      trips_df <- data.frame(Trip = unique(df$FTunitName), Quarter = "Year")
      return(trips_df)
    }
  }
  
  Trips <- list(
    FO = make_trip_df(FO_stock),
    FT = make_trip_df(FT_stock),
    SA = make_trip_df(SA_stock),
    FM = make_trip_df(FM_stock),
    BV = make_trip_df(BV_stock)
  )
  
  return(Trips)
}

raiseWeights <- function(SA_stock, 
                         FO_stock, 
                         CL_stock,
                         iRaisingLevel,  # NEW: Add raising level parameter
                         method = c("bootstrap"),
                         B = 2000,     
                         seed = 123,
                         iYQ = "YEAR") {
  
  method <- match.arg(method)
  
  # Check if discards should be raised
  discard_available <- iRaisingLevel$Dis %in% c("WEIGHTS", "LENGTHS", "LWK", "ALK", "ALL")
  
  if (!discard_available) {
    cat("Discard raising level:", iRaisingLevel$Dis, "- Discards will not be raised. Only landings provided.\n")
  }
  
  raise_single <- function(SA_stock, FO_stock, CL_stock, method, B, seed, raise_discards = TRUE) {
    
    # Always calculate landings weight from official data
    LW <- sum(CL_stock$CLoffWeight, na.rm = TRUE)
    
    # If discards should not be raised, return only landings
    if (!raise_discards) {
      return(data.frame(
        DR = NA,
        LW = LW,
        DW = 0,  # No discards
        DW_Var = NA,
        DW_SD = NA,
        CI_low = NA,
        CI_high = NA
      ))
    }
    
    # Otherwise, proceed with normal discard raising
    SA_stock <- SA_stock[SA_stock$SAlowerHierarchy != "C", ]
    
    WeightByTrip <- aggregate(SAtotalWeightLive ~ SAcatchCategory + FTunitName,
                              data = SA_stock, sum)
    
    FO_stock$registered <- 1
    FO_stock$sampled <- 0
    FO_stock$sampled[FO_stock$FOcatchReg == "All"] <- 1
    
    SampledHauls <- aggregate(cbind(registered, sampled) ~ FTunitName, data = FO_stock, sum)
    SampledHauls$ratio <- SampledHauls$registered / SampledHauls$sampled
    
    WeightByTrip$ratio <- SampledHauls$ratio[match(WeightByTrip$FTunitName, SampledHauls$FTunitName)]
    WeightByTrip$Weight2 <- WeightByTrip$SAtotalWeightLive * WeightByTrip$ratio
    agg <- aggregate(Weight2 ~ SAcatchCategory, data = WeightByTrip, sum)
    
    byTrip <- tidyr::pivot_wider(
      WeightByTrip %>% dplyr::select(-SAtotalWeightLive, -ratio),
      names_from = "SAcatchCategory",
      values_from = "Weight2"
    )
    
    byTrip$Dis <- ifelse(is.na(byTrip$Dis), 0, byTrip$Dis)
    byTrip$Lan <- ifelse(is.na(byTrip$Lan), 0, byTrip$Lan)
    byTrip$DR  <- ifelse(byTrip$Lan > 0, byTrip$Dis / byTrip$Lan, 0)
    byTrip$DR[is.na(byTrip$DR)] <- 0
    
    DR <- agg[agg$SAcatchCategory == "Dis", "Weight2"] /
      agg[agg$SAcatchCategory == "Lan", "Weight2"]
    DR <- as.numeric(DR)
    DW <- LW * DR
    
    CI_low <- CI_high <- DW_Var <- DW_SD <- NA
    
    if (method == "bootstrap") {
      set.seed(seed)
      n <- nrow(byTrip)
      boot_DW <- numeric(B)
      for (b in 1:B) {
        idx <- sample.int(n, size = n, replace = TRUE)
        DR_b <- sum(byTrip$Dis[idx], na.rm = TRUE) / sum(byTrip$Lan[idx], na.rm = TRUE)
        if (is.na(DR_b) || is.infinite(DR_b)) DR_b <- 0
        boot_DW[b] <- LW * DR_b
      }
      CI <- quantile(boot_DW, probs = c(0.025, 0.975), na.rm = TRUE)
      CI_low <- max(0, as.numeric(CI[1]))
      CI_high <- as.numeric(CI[2])
      DW_Var <- var(boot_DW, na.rm = TRUE)
      DW_SD  <- sqrt(DW_Var)
    }
    
    data.frame(
      DR = DR,
      LW = LW,
      DW = DW,
      DW_Var = DW_Var,
      DW_SD  = DW_SD,
      CI_low = CI_low,
      CI_high = CI_high
    )
  }
  
  # ==== CASE 1: YEAR ====
  if (toupper(iYQ) == "YEAR") {
    result <- raise_single(SA_stock, FO_stock, CL_stock, method, B, seed, 
                           raise_discards = discard_available)
    result$Quarter <- "YEAR"
    return(result)
  }
  
  # ==== CASE 2: QUARTERLY ====
  quarters_in_data <- sort(unique(CL_stock$CLquar))
  quarters_requested <- as.numeric(gsub("Q", "", unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))))
  
  results <- list()
  
  for (q in quarters_in_data) {
    SA_q <- SA_stock[SA_stock$Quarter == q, ]
    FO_q <- FO_stock[FO_stock$Quarter == q, ]
    CL_q <- CL_stock[CL_stock$CLquar == q, ]
    
    if (q %in% quarters_requested) {
      if (nrow(SA_q) == 0 && nrow(FO_q) == 0 && nrow(CL_q) == 0) next
      res_q <- raise_single(SA_q, FO_q, CL_q, method, B, seed, 
                            raise_discards = discard_available)
    } else {
      # Quarter not requested for raising — report LW only
      res_q <- data.frame(
        DR = NA,
        LW = sum(CL_q$CLoffWeight, na.rm = TRUE),
        DW = NA,
        DW_Var = NA,
        DW_SD = NA,
        CI_low = NA,
        CI_high = NA
      )
    }
    res_q$Quarter <- q
    results[[paste0("Q", q)]] <- res_q
  }
  
  out_df <- do.call(rbind, results)
  rownames(out_df) <- NULL
  return(out_df)
}



raiseLengths <- function(SA_stock, FO_stock, RW_stock, FM_stock, BV_stock,
                               iRaisingLevel = "ALL", iYQ = "YEAR", 
                               catchCat = NULL, verbose = TRUE) {
  
  raise_single <- function(SA_stock, FO_stock, RW_stock, FM_stock, BV_stock,
                           iRaisingLevel, catchCat, verbose) {
    
    # ---- Remove BV-coupled SA records ----
    SA_stock <- SA_stock[SA_stock$SAlowerHierarchy != "C", ]
    
    # --------------------------------------------------------------
    # STEP 1: CORRECTION FACTORS
    # --------------------------------------------------------------
    total_trip <- aggregate(SAtotalWeightLive ~ SAcatchCategory + FTunitName,
                            data = SA_stock, sum)
    miss_trip  <- SA_stock[is.na(SA_stock$SAsampleWeightLive), ]
    
    if (nrow(miss_trip) > 0) {
      na_trip <- aggregate(SAtotalWeightLive ~ SAcatchCategory + FTunitName,
                           data = miss_trip, sum)
      names(na_trip)[3] <- "na_wt"
    } else {
      na_trip <- data.frame(SAcatchCategory = character(0),
                            FTunitName      = character(0),
                            na_wt           = numeric(0))
    }
    
    corr_trip <- merge(total_trip, na_trip,
                       by = c("SAcatchCategory", "FTunitName"), all.x = TRUE)
    corr_trip$na_wt[is.na(corr_trip$na_wt)] <- 0
    corr_trip$correction <- with(corr_trip, 1 - na_wt / SAtotalWeightLive)
    corr_trip$empty      <- corr_trip$correction == 0
    
    valid_data   <- merge(SA_stock,
                          corr_trip[, c("SAcatchCategory", "FTunitName", "empty")],
                          by = c("SAcatchCategory", "FTunitName"))
    valid_data   <- valid_data[valid_data$empty, ]
    
    total_global <- aggregate(SAtotalWeightLive ~ SAcatchCategory,
                              data = SA_stock, sum)
    miss_global  <- valid_data[is.na(valid_data$SAsampleWeightLive), ]
    
    if (nrow(miss_global) > 0) {
      na_global <- aggregate(SAtotalWeightLive ~ SAcatchCategory,
                             data = miss_global, sum)
      names(na_global)[2] <- "na_wt"
    } else {
      na_global <- data.frame(SAcatchCategory = character(0),
                              na_wt           = numeric(0))
    }
    
    corr_global <- merge(total_global, na_global,
                         by = "SAcatchCategory", all.x = TRUE)
    corr_global$na_wt[is.na(corr_global$na_wt)] <- 0
    corr_global$correction_global <- with(corr_global,
                                          1 - na_wt / SAtotalWeightLive)
    
    corr_final <- merge(corr_trip,
                        corr_global[, c("SAcatchCategory", "correction_global")],
                        by = "SAcatchCategory", all.x = TRUE)
    corr_final$correction_final <- ifelse(
      corr_final$empty,
      corr_final$correction_global,
      corr_final$correction
    )
    
    # --------------------------------------------------------------
    # STEP 2: HAUL-RAISING RATIOS
    # --------------------------------------------------------------
    FO_stock$registered <- 1
    FO_stock$sampled    <- ifelse(FO_stock$FOcatchReg == "All", 1, 0)
    
    SampledHauls        <- aggregate(cbind(registered, sampled) ~ FTunitName,
                                     data = FO_stock, sum)
    SampledHauls$ratio  <- SampledHauls$registered / SampledHauls$sampled
    
    # --------------------------------------------------------------
    # STEP 3: SUBSAMPLE WEIGHT RATIO
    # --------------------------------------------------------------
    SA_stock$ratiowt <- SA_stock$SAtotalWeightLive / SA_stock$SAsampleWeightLive
    
    # --------------------------------------------------------------
    # STEP 4: APPLY RATIOS TO FM DATA
    # --------------------------------------------------------------
    if (!is.null(catchCat)) {
      FM_stock <- FM_stock[FM_stock$SAcatchCat %in% catchCat, ]
    }
    
    FM_stock$ratiowt <- SA_stock$ratiowt[match(FM_stock$SAid, SA_stock$SAid)]
    FM_stock$ratio   <- SampledHauls$ratio[match(FM_stock$FTunitName,
                                                 SampledHauls$FTunitName)]
    
    FM_stock <- FM_stock %>%
      dplyr::left_join(
        corr_final %>% dplyr::select(FTunitName, SAcatchCategory, correction_final),
        by = c("FTunitName", "SAcatchCat" = "SAcatchCategory")
      )
    
    FM_stock$correction_final[is.na(FM_stock$correction_final)] <- 1
    FM_stock$correction_final[FM_stock$correction_final == 0]   <- NA
    
    FM_stock$lenNumtrp <- FM_stock$FMnumberAtUnit *
      FM_stock$ratiowt *
      FM_stock$ratio *
      (1 / FM_stock$correction_final)
    
    # --------------------------------------------------------------
    # STEP 5: AGGREGATE
    # --------------------------------------------------------------
    agg2 <- aggregate(lenNumtrp ~ FMclassMeasured + SAcatchCat,
                      data = FM_stock, sum)
    
    # --------------------------------------------------------------
    # STEP 6: DIAGNOSTICS
    # --------------------------------------------------------------
    if (verbose) {
      trip_issues <- corr_final[corr_final$correction != 1 & !corr_final$empty,
                                c("FTunitName", "SAcatchCategory", "correction")]
      if (nrow(trip_issues) == 0) {
        message("✅ No trip-level correction applied")
      } else {
        message("⚠️ Trip-level corrections applied:")
        print(unique(trip_issues))
        message("➡️ Total trip-level corrected groups: ", nrow(unique(trip_issues)))
      }
      
      fallback_rows <- corr_final[corr_final$empty == TRUE,
                                  c("FTunitName", "SAcatchCategory", "correction_final")]
      if (nrow(fallback_rows) == 0 || all(fallback_rows$correction_final == 1)) {
        message("✅ No fallback (catchCategory-level) correction applied")
      } else {
        message("⚠️ Fallback corrections applied (missing sampling cases):")
        message("➡️ Missing sampling detected for:")
        print(unique(fallback_rows[, c("FTunitName", "SAcatchCategory")]))
        message("➡️ Applied fallback correction per catchCategory:")
        print(unique(fallback_rows[, c("SAcatchCategory", "correction_final")]))
      }
    }
    
    # --------------------------------------------------------------
    # STEP 7: RECOVERY FACTOR + FLEET-LEVEL RAISING
    # --------------------------------------------------------------
    LW <- RW_stock$LW
    DW <- RW_stock$DW
    
    WeightByTrip         <- aggregate(SAtotalWeightLive ~ SAcatchCategory + FTunitName,
                                      data = SA_stock, sum)
    WeightByTrip$ratio   <- SampledHauls$ratio[match(WeightByTrip$FTunitName,
                                                     SampledHauls$FTunitName)]
    WeightByTrip$Weight2 <- WeightByTrip$SAtotalWeightLive * WeightByTrip$ratio
    agg_wt               <- aggregate(Weight2 ~ SAcatchCategory,
                                      data = WeightByTrip, sum)
    
    get_rf <- function(cat) {
      w <- agg_wt$Weight2[agg_wt$SAcatchCategory == cat]
      if (length(w) == 0 || is.na(w) || w == 0) return(0)
      if(cat == "Lan"){
        LW / (w / 1000)
      } else {
        DW / (w / 1000)
      }

    }
    RF_Lan <- get_rf("Lan")
    RF_Dis <- get_rf("Dis")
    
    agg2$lenNumfleet <- ifelse(
      agg2$SAcatchCat == "Lan",
      agg2$lenNumtrp * RF_Lan /1000,
      agg2$lenNumtrp * RF_Dis /1000
    )
    
    # Convert mm to cm
    agg2$FMclassMeasured <- agg2$FMclassMeasured / 10
    
    # --------------------------------------------------------------
    # STEP 8: RAW COUNTS + iRaisingLevel (BV merge)
    # --------------------------------------------------------------
    agg_numbers_raw <- FM_stock %>%
      dplyr::mutate(FMclassMeasured = as.numeric(FMclassMeasured) / 10) %>%
      dplyr::group_by(FMclassMeasured, SAcatchCat) %>%
      dplyr::summarise(number_raw_LFD = sum(FMnumberAtUnit, na.rm = TRUE),
                       .groups = "drop")
    
    if (iRaisingLevel %in% c("ALL", "ALK", "LWK") && nrow(BV_stock) > 0) {
      weightdata <- BV_stock[, c("BVnationalUniqueFishId", "BVtypeMeasured",
                                 "BVvalueMeasured", "SAcatchCat")]
      weightdata <- weightdata %>%
        tidyr::pivot_wider(
          names_from  = BVtypeMeasured,
          values_from = BVvalueMeasured
        )
      weightdata$FMclassMeasured <- as.numeric(as.character(weightdata$LengthTotal))
      
      agg_numbers_raw_BV <- weightdata %>%
        dplyr::mutate(FMclassMeasured = as.numeric(FMclassMeasured) / 10) %>%
        dplyr::count(FMclassMeasured, SAcatchCat, name = "number_raw_LWK")
      
      agg_numbers_raw <- merge(agg_numbers_raw, agg_numbers_raw_BV, all = TRUE)
    } else {
      agg_numbers_raw$number_raw_LWK <- 0
    }
    
    agg2 <- merge(agg2, agg_numbers_raw,
                  by = c("FMclassMeasured", "SAcatchCat"), all.x = TRUE)
    
    return(agg2)
  }
  
  # ================================================================
  # CASE 1: YEAR
  # ================================================================
  if (toupper(iYQ) == "YEAR") {

    result <- raise_single(SA_stock, FO_stock, RW_stock, FM_stock, BV_stock,
                           iRaisingLevel, catchCat, verbose)
    result$Quarter <- "YEAR"
    return(result)
  }
  
  # ================================================================
  # CASE 2: QUARTERS
  # ================================================================
  quarters <- as.numeric(gsub("Q", "", unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))))
  results  <- list()
  
  for (q in quarters) {
    SA_q <- SA_stock[SA_stock$Quarter == q, ]
    FO_q <- FO_stock[FO_stock$Quarter == q, ]
    RW__q <- RW_stock[RW_stock$Quarter  == q, ]
    FM_q <- FM_stock[FM_stock$Quarter  == q, ]
    BV_q <- BV_stock[BV_stock$Quarter  == q, ]
    
    if (all(c(nrow(SA_q), nrow(FO_q), nrow(RW__q), nrow(FM_q), nrow(BV_q)) == 0)) next
    
    if (verbose) message("--- Processing Q", q, " ---")
    
    res_q         <- raise_single(SA_q, FO_q, RW__q, FM_q, BV_q,
                                  iRaisingLevel, catchCat, verbose)
    res_q$Quarter <- q
    results[[paste0("Q", q)]] <- res_q
  }
  
  # ================================================================
  # COMBINE
  # ================================================================
  if (length(results) == 0) return(data.frame())
  
  out_df <- do.call(rbind, lapply(results, as.data.frame))
  rownames(out_df) <- NULL
  return(out_df)
}



LF_plot <- function(LF) {
  
  ggplot(data = LF, aes(x = FMclassMeasured, y = lenNumfleet, group = SAcatchCat, colour = factor(SAcatchCat))) +
    geom_line(size = 1) +
    xlab("Length (cm)") +
    ylab("Numbers") +
    scale_colour_discrete("Catch category") +
    theme(
      panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
      panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
      panel.border = element_rect(colour = "black", fill = NA),
      legend.key = element_rect(fill = "white", colour = "white")
    )
  
}

generate_LWK <- function(LF_stock, BV_stock,
                         catchCat = c("Lan", "Dis"),
                         iYQ = "YEAR",
                         plot = TRUE,
                         # --- NEW arguments ---
                         iLWK = NULL,
                         iLWK_a = NULL,
                         iLWK_b = NULL,
                         iLWK_source = NULL) {
  library(ggplot2)
  library(dplyr)
  library(reshape2)
  
  catchCat <- match.arg(catchCat)
  
  # --- Helper function for single run ---
  generate_LWK_single <- function(LF_stock, BV_stock, iLWK, catchCat, iLWK_a = NULL,iLWK_b = NULL,iLWK_source = NULL, q = NULL) {
    
    weightdata <- BV_stock[, c("BVnationalUniqueFishId", "BVtypeMeasured", "BVvalueMeasured", "SAcatchCat")]
    weightdata <- weightdata %>%pivot_wider(names_from = BVtypeMeasured,values_from = BVvalueMeasured )
    weightdata$Length_class <- as.numeric(weightdata$LengthTotal) / 10
    weightdata$Weight <- as.numeric(weightdata$WeightLive)
    
    weightdata_sel <- weightdata[weightdata$SAcatchCat == catchCat, ]
    weightdata_sel <- weightdata_sel[!is.na(weightdata_sel$Length_class) & !is.na(weightdata_sel$Weight), ]
    
    # === CASE 1: Data available ===
    if (nrow(weightdata_sel) > 0) {
      m <- lm(log(Weight) ~ log(Length_class), data = weightdata_sel)
      a <- exp(coef(m)[1])
      b <- coef(m)[2]
      model_used <- iLWK == "calc"
    } else {
      # === CASE 2: No data, use manual parameters ===
      if (is.null(iLWK_a) || is.null(iLWK_b)) {
        warning("No data available and no manual parameters provided. Returning NULL.")
        return(NULL)
      }
      a <- as.numeric(iLWK_a)
      b <- as.numeric(iLWK_b)
      m <- NULL
      model_used <- iLWK == "calc"
    }
    
    # --- Generate prediction curve ---
    L_range <- if (nrow(weightdata_sel) > 0) {
      seq(min(weightdata_sel$Length_class), max(weightdata_sel$Length_class), length.out = 100)
    } else {
      LF_stock_catch <- LF_stock[LF_stock$SAcatchCat == catchCat, ]
      L_range <-  seq(min(LF_stock_catch$FMclassMeasured ), max(LF_stock_catch$FMclassMeasured), 1)
    }
    
    pred_df <- data.frame(Length_class = L_range)
    pred_df <- pred_df %>%
      mutate(fit = a * Length_class^b)
    
    if (model_used && !is.null(m)) {
      pred <- predict(m, newdata = pred_df, se.fit = TRUE)
      pred_df <- pred_df %>%
        mutate(
          fit = exp(pred$fit),
          CI_low = exp(pred$fit - 1.96 * pred$se.fit),
          CI_high = exp(pred$fit + 1.96 * pred$se.fit)
        )
    } else {
      pred_df$CI_low <- NA
      pred_df$CI_high <- NA
    }
    
    # --- Create plot ---
    if (plot) {
      if (model_used) {
        hist_scale <- max(pred_df$fit, na.rm = TRUE) / max(table(weightdata_sel$Length_class))
        p <- ggplot(weightdata_sel, aes(x = Length_class)) +
          geom_histogram(aes(y = ..count.. * hist_scale), binwidth = 1, fill = "grey85", color = "grey70", alpha = 0.6) +
          geom_ribbon(data = pred_df, aes(ymin = CI_low, ymax = CI_high), fill = "skyblue2", alpha = 0.4) +
          geom_line(data = pred_df, aes(y = fit), color = "blue4", size = 1.2) +
          geom_point(data = weightdata_sel, aes(y = Weight), color = "black", alpha = 0.7, size = 2)
      } else {
        p <- ggplot(pred_df, aes(x = Length_class, y = fit)) +
          geom_line(color = "red", size = 1.2) +
          annotate("text", x = mean(L_range), y = max(pred_df$fit, na.rm = TRUE)*0.9,
                   label = paste0("Manual parameters:\na = ", signif(a, 4), ", b = ", signif(b, 4),
                                  if (!is.null(iLWK_source)) paste0("\n(", iLWK_source, ")")),
                   hjust = 0.5, color = "red4", size = 4)
      }
      
      p <- p +
        scale_x_continuous(name = "Length (cm)") +
        scale_y_continuous(name = "Weight (g)") +
        theme_minimal(base_size = 14) +
        labs(
          title = paste(ifelse(catchCat == "Lan", "Landings", "Discards"), "—",
                        ifelse(toupper(iYQ) == "YEAR", "YEAR", paste0("Q", q))),
          subtitle = ifelse(model_used, "Model fitted to data", "Manual parameters used")
        )
    } else {
      p <- NULL
    }
    
    return(list(
      model = m,
      a = a,
      b = b,
      raw = if (model_used) weightdata_sel else data.frame(),
      prediction = pred_df,
      plot = p,
      manual_parameters = !model_used,
      parameter_source = if (!model_used) iLWK_source else "Model fit"
    ))
  }
  
  # === CASE 1: YEAR ===
  if (toupper(iYQ) == "YEAR") {
    res <- generate_LWK_single(LF_stock, BV_stock, iLWK, catchCat, iLWK_a ,iLWK_b ,iLWK_source)
    if (!is.null(res)) res$Quarter <- "YEAR"
    return(res)
  }
  
  # === CASE 2: QUARTERS ===
  quarters <- as.numeric(gsub("Q", "", unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))))
  results <- list()
  
  for (q in quarters) {
    BV_q <- BV_stock[BV_stock$Quarter == q, ]
    LF_q <- LF_stock[LF_stock$Quarter == q, ]
    if (nrow(BV_q) == 0 && (is.null(iLWK_a) || is.null(iLWK_b))) next
    
    res_q <- generate_LWK_single(LF_q, BV_q, iLWK, catchCat, iLWK_a ,iLWK_b ,iLWK_source, q)
    if (!is.null(res_q)) {
      res_q$Quarter <- q
      results[[paste0("Q", q)]] <- res_q
    }
  }
  
  return(results)
}


# Determine correct weight per quarter
get_total_weight <- function(total_weight, q, iYQ) {
  if (toupper(iYQ) == "YEAR") {
    return(total_weight)          # single number
  } else {
    if (is.vector(total_weight)) {
      return(total_weight[q])     # pick by quarter index
    } else if (is.data.frame(total_weight)) {
      return(total_weight[total_weight$Quarter == q, "Weight"])
    } else {
      stop("total_weight must be numeric vector or data.frame with 'Weight' column for quarterly data")
    }
  }
}
# SOP_correction_LF(LF, LWK_L, LW,  catchCat = "Lan", iYQ = iYQ)
SOP_correction_LF <- function(LF, LWK, total_weight,catchCat = c("Lan", "Dis"),  iYQ = NULL, iLWK = NULL,
                              bootstrap = FALSE,
                              nboot = 1000) {
  
  library(MASS)
  catchCat <- match.arg(catchCat)
  
  # Determine correction factor name
  corr_name <- ifelse(catchCat == "Dis", "corrdis", "corrland")
  
  # --- Helper: pick correct total weight ---
  get_total_weight <- function(total_weight, q, iYQ) {
    if (toupper(iYQ) == "YEAR") {
      return(total_weight)
    } else {
      if (is.vector(total_weight)) {
        return(total_weight[q])
      } else if (is.data.frame(total_weight)) {
        return(total_weight[total_weight$Quarter == q, "Weight"])
      } else {
        stop("total_weight must be numeric vector or data.frame with 'Weight' column for quarterly data")
      }
    }
  }
  
  # --- Helper: parametric bootstrap prediction ---
  bootstrap_pred <- function(model, newdata, nboot = 10000) {
    coefs <- MASS::mvrnorm(n = nboot,
                           mu = coef(model),
                           Sigma = vcov(model))
    X <- model.matrix(~ log(Length_class), data = newdata)
    logW_pred <- X %*% t(coefs)
    W_pred <- exp(logW_pred)
    
    data.frame(
      fit = apply(W_pred, 1, median, na.rm = TRUE),
      CI_low = apply(W_pred, 1, quantile, probs = 0.025, na.rm = TRUE),
      CI_high = apply(W_pred, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
  }
  
  # --- Helper: single run ---
  
  SOP_single <- function(LF_stock, LWK_sub, total_weight_sub, Quarter_label, iLWK = iLWK, catchCat) {
    LFx <- LF_stock[LF_stock$SAcatchCat == catchCat, ]
    if (nrow(LFx) == 0) return(NULL)
    
    LFx <- LFx[, c(1, 4, 5, 6)]
    names(LFx) <- c("Length_class", "number", "number_raw_LFD", "number_raw_LWK")
    
    # --- Predict individual weight with optional bootstrap ---
    if (bootstrap) {
      pred_df <- data.frame(Length_class = LFx$Length_class + 0.5)
      boot_out <- bootstrap_pred(LWK_sub$model, newdata = pred_df, nboot = nboot)
      
      LFx$indW2 <- boot_out$fit
      LFx$CI_low <- boot_out$CI_low
      LFx$CI_high <- boot_out$CI_high
      
      # Approximate variance from bootstrapped distribution
      LFx$indW2_var <- ((LFx$CI_high - LFx$CI_low) / 3.92)^2
      
    } else if (iLWK == "calc") {
      pred <- predict(LWK_sub$model, newdata = data.frame(Length_class = LFx$Length_class + 0.5), se.fit = TRUE)
      LFx$indW2 <- exp(pred$fit)
      LFx$indW2_var <- (exp(pred$se.fit^2) - 1) * exp(2 * pred$fit + pred$se.fit^2)
      LFx$CI_low <- exp(pred$fit - 1.96 * pred$se.fit)
      LFx$CI_high <- exp(pred$fit + 1.96 * pred$se.fit)
      
      # --- Apply correction ---
      est_weight <- sum(LFx$indW2/1000 * LFx$number*1000)
      corr_factor <- total_weight_sub / est_weight
      
    } else {
      LFx$indW2 = LWK_sub$a * LFx$Length_class^LWK_sub$b
      LFx$indW2_var <- NA
      LFx$CI_low <- NA
      LFx$CI_high <- NA
      corr_factor <- 1
    }
    
    LFx$corr_numbers <- corr_factor * LFx$number
    
    MWAL <- data.frame(
      length = LFx$Length_class,
      indW2 = LFx$indW2,
      indW2_var = LFx$indW2_var,
      CI_low = LFx$CI_low,
      CI_high = LFx$CI_high,
      number_raw_LFD = LFx$number_raw_LFD,
      number_raw_LWK = LFx$number_raw_LWK,
      number = LFx$number ,
      corr_numbers = LFx$corr_numbers,
      Quarter = Quarter_label
    )
    
    return(list(MWAL = MWAL, corr_factor = corr_factor))
  }
  
  # --- YEAR case ---
  if (toupper(iYQ) == "YEAR") {
    
    res <- SOP_single(LF, LWK, total_weight, "YEAR", iLWK = iLWK, catchCat)
    
    out <- list(MWAL = res$MWAL)
    out[[corr_name]] <- res$corr_factor
    return(out)
  }
  
  # --- QUARTERS case ---
  quarters <- as.numeric(gsub("Q", "", unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))))
  
  all_results <- lapply(quarters, function(q) {
    LF_q <- LF[LF$Quarter == q, ]
    if (nrow(LF_q) == 0) return(NULL)
    
    quarter_name <- paste0("Q", q)
    LWK_q <- if (is.list(LWK) && !is.null(LWK[[quarter_name]])) LWK[[quarter_name]] else LWK
    
    total_weight_q <- get_total_weight(total_weight, q, iYQ)
    
    SOP_single(LF_q, LWK_q, total_weight_q, quarter_name, iLWK, catchCat)
  })
  
  MWAL_combined <- do.call(rbind, lapply(all_results, function(x) if (!is.null(x)) x$MWAL))
  corr_vector <- sapply(all_results, function(x) if (!is.null(x)) x$corr_factor else NA)
  
  out <- list(MWAL = MWAL_combined)
  out[[corr_name]] <- corr_vector
  
  return(out)
}




generate_ALK <- function(LF, BV, catchCat = c("Lan", "Dis"), iYQ = "YEAR", plot = TRUE) {
  library(reshape2)
  library(dplyr)
  library(nnet)
  
  # Ensure catchCat is single
  if (length(catchCat) != 1) stop("catchCat must be a single value: 'Lan' or 'Dis'")
  catchCat <- catchCat[1]
  
  # --- Inner function: single ALK calculation ---
  generate_ALK_single <- function(LF_stock, BV_stock, catchCat, plot = TRUE, quarter_label = NULL) {
    plot <- as.logical(plot)  # ensure logical
    # Reshape BV data
    BV_stock <- BV_stock[, c("BVnationalUniqueFishId", "BVtypeMeasured", "BVvalueMeasured", "SAcatchCat")]
    BV_stock <- dcast(BV_stock, BVnationalUniqueFishId + SAcatchCat ~ BVtypeMeasured, value.var = "BVvalueMeasured")
    BV_stock$Length_class <- as.numeric(BV_stock$LengthTotal) / 10
    BV_sel <- BV_stock[BV_stock$SAcatchCat == catchCat, ]
    
    sp.age <- data.frame(tl = BV_sel$Length_class, age = BV_sel$Age)
    sp.age <- sp.age %>% mutate(tl = as.numeric(tl), age = as.numeric(age)) %>% filter(!is.na(age))
    
    # Length categories
    sp.age <- sp.age %>% mutate(lcat = floor(tl))
    
    # Count by age
    total_num_age <- nrow(sp.age)
    age_counts <- aggregate(tl ~ age, data = sp.age, FUN = length)
    names(age_counts)[2] <- "number_raw"
    num_age <- list(total_num_age = total_num_age, age_counts = age_counts)
    
    # Observed ALK
    raw_matrix <- xtabs(~ lcat + age, data = sp.age)
    ALK.obs <- prop.table(raw_matrix, margin = 1)
    
    # Multinomial model
    mlr <- multinom(age ~ lcat, data = sp.age, maxit = 500)
    
    # Prediction length intervals
    LF_stock_sel <- LF_stock[LF_stock$SAcatchCat == catchCat, c(1, 4)]
    names(LF_stock_sel) <- c("length", "number")
    lens <- seq(min(LF_stock_sel$length), max(LF_stock_sel$length), 1)
    
    ALK.sm <- predict(mlr, data.frame(lcat = lens), type = "probs", se.fit = TRUE)
    
    # Als ALK.sm een vector is (bij 2 leeftijden), zet het om naar een matrix
    if (is.null(dim(ALK.sm))) {
      ALK.sm <- matrix(ALK.sm, ncol = 1, dimnames = list(lens, "Age_2"))
      # OF als je beide kansen wilt behouden (Age_1 en Age_2):
      # ALK.sm <- cbnd(1 - ALK.sm, ALK.sm)
      # rownames(ALK.sm) <- lens
    } else {
      rownames(ALK.sm) <- lens
    }
    
    # Plotting
    alk_plots <- NULL
    if (plot) {
      make_plot <- function(ALKdata, main_title) {
        old_par <- par(no.readonly = TRUE)
        on.exit(par(old_par))
        par(mar = c(4, 4, 1, 1), oma = c(2, 0, 2, 0))
        alkPlot(ALKdata, xlab = "Total Length (cm)", ylab = "Proportion")
        mtext(main_title, side = 3, outer = TRUE, adj = 0.1, cex = 1.2, font = 2)
        
        # Capture the plot as a recorded plot
        recordPlot()
      }
      
      title_prefix <- ifelse(catchCat == "Lan", "Landings", "Discards")
      title_suffix <- ifelse(!is.null(quarter_label), quarter_label, 
                             ifelse(toupper(iYQ) == "YEAR", "YEAR", ""))
      
      alk_plots <- list(
        raw   = function() make_plot(ALK.obs, paste(title_prefix, "raw —", title_suffix)),
        model = function() make_plot(ALK.sm,  paste(title_prefix, "modelled —", title_suffix))
      )
    }
    return(list(num_age = num_age, ALK.obs = ALK.obs, ALK.sm = ALK.sm, plot = alk_plots))
  }
  
  # --- CASE 1: YEAR ---
  if (toupper(iYQ) == "YEAR") {
    res <- generate_ALK_single( LF, BV, catchCat, plot = plot, quarter_label = "YEAR")
    res$Quarter <- "YEAR"
    return(res)
  }
  
  # --- CASE 2: QUARTERS ---
  quarters <- as.numeric(gsub("Q", "", unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))))
  results <- list()
  
  for (q in quarters) {
    BV_q <- BV[BV$Quarter == q, ]
    LF_q <- LF[LF$Quarter == q, ]
    
    if (nrow(BV_q) == 0) next
    
    res_q <- generate_ALK_single(LF_q,BV_q,  catchCat, plot = plot, quarter_label = paste0("Q", q))
    res_q$Quarter <- paste0("Q", q)
    results[[paste0("Q", q)]] <- res_q
  }
  
  return(results)
}

Apply_ALK <- function(LF, LWK, ALK, SOP, catchCat = c("Lan", "Dis"), iYQ = "YEAR") {
  
  catchCat <- match.arg(catchCat)
  
  # --- Helper function for a single run ---
  # --- Helper function for a single run ---
  Apply_ALK_single <- function(LF_stock, LWK_sub, ALK_sub, corr_factor, Quarter_label) {
    LFx <- LF_stock[LF_stock$SAcatchCat == catchCat, ]
    if (nrow(LFx) == 0) return(NULL)
    
    LFx <- LFx[, c(1, 4, 5)]
    names(LFx) <- c("Length_class", "number", "number_raw")
    
    # Predict individual weight (and SE)
    pred <- predict(LWK_sub$model, newdata = data.frame(Length_class = LFx$Length_class + 0.5), se.fit = TRUE)
    LFx$indW2 <- exp(pred$fit)   # predicted weight (kg)
    
    # Variance of predicted weight (on natural scale)
    LFx$indW2_var <- (exp(pred$se.fit^2) - 1) * exp(2 * pred$fit + pred$se.fit^2) 
    
    LFx$totW <- LFx$indW2 * LFx$number
    names(LFx) <- c("length", "number", "number_raw", "indW2", "indW2_var", "totW")
    
    # Fill missing length classes
    empty <- data.frame(lcat = seq(min(LFx$length), max(LFx$length)))
    empty$number <- LFx$number[match(empty$lcat, LFx$length)]
    empty$totW <- LFx$totW[match(empty$lcat, LFx$length)]
    empty$indW2_var <- LFx$indW2_var[match(empty$lcat, LFx$length)]
    empty[is.na(empty)] <- 0
    
    # Prepare vectors for ALK
    len.n <- xtabs(number ~ lcat, data = empty)
    w.n <- xtabs(totW ~ lcat, data = empty)
    w.n.var <- xtabs(indW2_var ~ lcat, data = empty)
    
    tmpn <- sweep(ALK_sub$ALK.sm, MARGIN = 1, FUN = "*", STATS = len.n)
    tmpw <- sweep(ALK_sub$ALK.sm, MARGIN = 1, FUN = "*", STATS = w.n)
    tmpwvar <- sweep(ALK_sub$ALK.sm, MARGIN = 1, FUN = "*", STATS = w.n.var)
    
    # Aggregate
    naa <- colSums(tmpn)
    naaw <- colSums(tmpw)
    waa.var <- colSums(tmpwvar, na.rm = TRUE)  # variance of total weight
    
    # Mean weight at age
    MWAA <- (naaw / naa)  # kg
    
    # --- NEW: Confidence intervals for MWAA ---
    # Assuming normal distribution of MWAA
    MWAA_sd <- sqrt(waa.var) 
    MWAA_CI_low <- MWAA - 1.96 * MWAA_sd
    MWAA_CI_high <- MWAA + 1.96 * MWAA_sd
    
    # SOP-corrected numbers
    NAx <- naa 
    SopCorrNAA <- NAx * corr_factor
    
    # Age vector
    ages <- as.numeric(colnames(ALK_sub$ALK.sm))
    
    # Combine results
    df <- data.frame(
      Age = ages,
      number_raw = ALK_sub$num_age$age_counts$number_raw,
      SopCorrNAA = SopCorrNAA,
      MWAA = MWAA,
      MWAA_var = waa.var,
      CI_low = MWAA_CI_low,
      CI_high = MWAA_CI_high,
      Quarter = Quarter_label
    )
    
    return(df)
  }
  
  # --- YEAR case ---
  if (toupper(iYQ) == "YEAR") {
    corr_factor <- if (catchCat == "Lan") SOP$corrland else SOP$corrdis
    res <- Apply_ALK_single(LF, LWK, ALK, corr_factor, "YEAR")
    return(res)
  }
  
  # --- QUARTERS case ---
  quarters <- unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))
  all_results <- lapply(seq_along(quarters), function(idx) {
    q <- quarters[idx]
    LF_q <- LF[LF$Quarter == gsub("Q", "", q), ]
    if (nrow(LF_q) == 0) return(NULL)
    
    LWK_q <- if (is.list(LWK) && !is.null(LWK[[q]])) LWK[[q]] else LWK
    ALK_q <- if (is.list(ALK) && !is.null(ALK[[q]])) ALK[[q]] else ALK
    
    # Correct SOP factor for this quarter
    corr_factor <- if (catchCat == "Lan") SOP$corrland[idx] else SOP$corrdis[idx]
    
    Apply_ALK_single(LF_q, LWK_q, ALK_q, corr_factor, q) 
  })
  
  all_results <- do.call(rbind, all_results)
  rownames(all_results) <- NULL
  return(all_results)
}



plot_MWAA <- function(MWAA_list, type = c("Lan", "Dis")) {
  library(ggplot2)
  library(dplyr)
  
  type <- match.arg(type)
  
  # Extract components
  MWAA <- if(type == "Lan") MWAA_list$L_MWAA else MWAA_list$D_MWAA
  MWAA_var <- if(type == "Lan") MWAA_list$L_MWAA_var else MWAA_list$D_MWAA_var
  CorrNums <- if(type == "Lan") MWAA_list$SopCorrLNAA else MWAA_list$SopCorrDNAA
  
  # Prepare data frame for plotting
  df <- data.frame(
    age = seq_along(MWAA),
    MWAA = MWAA,
    MWAA_var = MWAA_var,
    corr_numbers = CorrNums
  )
  
  # Compute 95% CI
  df$CI_low <- df$MWAA - 1.96 * sqrt(df$MWAA_var)
  df$CI_high <- df$MWAA + 1.96 * sqrt(df$MWAA_var)
  df$CI_low[df$CI_low < 0] <- 0
  
  # Scale bars for sample numbers
  max_y <- max(df$CI_high, na.rm = TRUE)
  scale_factor <- max_y / max(df$corr_numbers, na.rm = TRUE)
  
  # Plot
  p <- ggplot(df, aes(x = age)) +
    geom_bar(aes(y = corr_numbers * scale_factor), stat = "identity", fill = "grey85", alpha = 0.7) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = "skyblue2", alpha = 0.4) +
    geom_line(aes(y = MWAA), color = "blue4", size = 1.2) +
    geom_point(aes(y = MWAA), color = "blue4", fill = "blue3", shape = 21, stroke = 0.8, size = 2) +
    scale_y_continuous(
      name = "Mean weight-at-age (kg)",
      sec.axis = sec_axis(~./scale_factor, name = "Number of fish")
    ) +
    scale_x_continuous(name = "Age (years)", breaks = seq(min(df$age), max(df$age), 1)) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y.right = element_text(color = "grey40"),
      plot.title = element_blank()
    )
  
  # print(p)
  return(p)
}

plot_MWAL <- function(MWAL_list, type = c("Lan", "Dis")) {
  library(ggplot2)
  library(dplyr)
  
  type <- match.arg(type)
  
  # Extract components based on type
  MWAL <- if(type == "Lan") MWAL_list$L_MWAL else MWAL_list$D_MWAL
  CorrNums <- if(type == "Lan") MWAL_list$SopCorrLNAL else MWAL_list$SopCorrDNAL
  
  # Merge MWAL with corrected numbers
  df <- MWAL %>%
    left_join(CorrNums, by = "length") %>%
    rename(Length = length, IndW = indW2, CorrNumbers = corr_numbers)
  
  # Compute 95% CI
  df <- df %>%
    mutate(
      CI_low = pmax(IndW - 1.96 * sqrt(indW2_var), 0),
      CI_high = IndW + 1.96 * sqrt(indW2_var)
    )
  
  # Scale bars for sample numbers
  max_y <- max(df$CI_high, na.rm = TRUE)
  scale_factor <- max_y / max(df$CorrNumbers, na.rm = TRUE)
  
  # Plot
  p <- ggplot(df, aes(x = Length)) +
    geom_bar(aes(y = CorrNumbers * scale_factor), stat = "identity", fill = "grey85", alpha = 0.7) +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = "skyblue2", alpha = 0.4) +
    geom_line(aes(y = IndW), color = "blue4", size = 1.2) +
    geom_point(aes(y = IndW), color = "blue4", fill = "blue3", shape = 21, stroke = 0.8, size = 2) +
    scale_y_continuous(
      name = "Individual weight (kg)",
      sec.axis = sec_axis(~./scale_factor, name = "Corrected number of fish")
    ) +
    scale_x_continuous(name = "Length (cm)", breaks = df$Length) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.y.right = element_text(color = "grey40"),
      plot.title = element_text(face = "bold", size = 16)
    )
  
  return(p)
}


Compare_LNAA_plot <- function(irepo, iWG, istock, iyear, LNAA) {
  
  library(tidyverse)
  
  # --- Read current-year LNAA data ---
  orig <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear, "FINAL", paste0(istock, "_SopCorr_L_NAA.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  # orig <- orig[, -1]
  names(orig) <- c("age", "LNAA_orig")
  
  # --- Read current-year mean weight-at-age ---
  orig_w <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear, "FINAL", paste0(istock, "_L_MWAA.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  names(orig_w) <- c("age", "LMWAA_orig")
  # orig_w <- orig_w[, -1]
  orig <- merge(orig, orig_w)
  
  # --- Read previous-year LNAA and weight-at-age ---
  iyear_1 <- as.character(as.numeric(iyear) - 1)
  orig_1 <- read_delim(
    paste0(irepo, iWG, "/OUTPUT/", istock, "/", iyear_1, "/FINAL/", istock, "_SopCorr_L_NAA.csv"),
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE
  )
  # orig_1 <- as.data.frame(orig_1)[, -1]
  names(orig_1) <- c("age", paste0("LNAA_", iyear_1))
  
  orig_w_1 <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear_1, "FINAL", paste0(istock, "_L_MWAA.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  names(orig_w_1) <- c("age",  paste0("LMWAA_", iyear_1))
  # orig_w <- orig_w[, -1]
  orig_1 <- merge(orig_1, orig_w_1)
  
  # --- Read LNAA and LMWAA from SOP_L object ---
  rdbesLNAA <- as.data.frame(LNAA$SopCorrLNAA)
  rdbesLNAA$age <- as.numeric(rownames(rdbesLNAA))
  names(rdbesLNAA) <- c("LNAA_rdbes", "age")
  
  rdbesLMWAA <- as.data.frame(LNAA$L_MWAA)
  rdbesLMWAA$age <- as.numeric(rownames(rdbesLMWAA))
  names(rdbesLMWAA) <- c( "LMWAA_rdbes", "age")
  
  # --- Merge all sources ---
  orig <- merge(orig, rdbesLNAA, by = "age", all = TRUE)
  orig <- merge(orig, orig_1, by = "age", all = TRUE)
  orig <- merge(orig, rdbesLMWAA, by = "age", all = TRUE)
  
  # --- Replace NA only in LNAA columns ---
  lnaa_cols <- grep("^LNAA", names(orig), value = TRUE)
  orig <- orig %>% mutate(across(all_of(lnaa_cols), ~replace_na(.x, 0)))
  
  # --- Pivot to long format ---
  orig_long <- orig %>%
    pivot_longer(
      cols = matches("^(LNAA|LMWAA)"),
      names_to = c("metric", "source"),
      names_pattern = "(LNAA|LMWAA)_(.*)",
      values_to = "value"
    )
  
  # --- Define aesthetics ---
  cols <- hue_pal()(length(unique(orig_long$source)))
  linetypes <- c("orig" = "dashed", "rdbes" = "solid")
  linetypes[iyear_1] <- "solid"
  linesizes <- c("orig" = 1.5, "rdbes" = 1)
  linesizes[iyear_1] <- 1
  
  # --- Facet labels for Y-axis ---
  facet_labels <- c(
    "LNAA" = "Number at age (x 1000 ind.)",
    "LMWAA" = "Average weight at age (kg)"
  )
  
  # --- Plot ---
  ggplot(orig_long, aes(x = age, y = value, color = source,
                        linetype = source, size = source)) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = linetypes) +
    scale_size_manual(values = linesizes, guide = "none") +
    scale_x_continuous(
      name = "Age (years)",
      breaks = seq(floor(min(orig_long$age, na.rm = TRUE)),
                   ceiling(max(orig_long$age, na.rm = TRUE)),
                   by = 1)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    facet_wrap(
      ~ metric,
      scales = "free_y",
      ncol = 1,
      labeller = as_labeller(facet_labels)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

Compare_LNAL_plot <- function(irepo, iWG,istock,iyear, SOP_L) {
  library(tidyverse)
  orig <- read_delim(
    file.path(irepo,iWG,"OUTPUT", istock, iyear,"FINAL",paste0(istock, "_SopCorr_L_NAL.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  orig <- orig[,-1]
  
  orig_w <- read_delim(
    file.path(irepo,iWG,"OUTPUT", istock, iyear,"FINAL",paste0(istock, "_L_MWAL.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  orig_w <- orig_w[,-1]
  
  orig <- merge(orig, orig_w)
  names(orig) <- c("length", "LNAL_orig", "LMWAL_orig")
  
  iyear_1<-as.character(as.numeric(iyear)-1)
  orig_1 <- read_delim(paste0(irepo ,iWG,"/OUTPUT/",istock,"/",iyear_1,"/FINAL/",istock,"_SopCorr_L_NAL.csv"), 
                       ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                       trim_ws = TRUE)
  
  orig_1<-as.data.frame(orig_1)[,-1]
  
  orig_w_1 <- read_delim(
    file.path(irepo,iWG,"OUTPUT", istock, iyear_1,"FINAL",paste0(istock, "_L_MWAL.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  orig_w_1 <- orig_w_1[,-1]
  
  orig_1 <- merge(orig_1, orig_w_1)
  
  names(orig_1)<-c("length",paste0("LNAL_",iyear_1),paste0("LMWAL_",iyear_1))
  
  rdbesLNAL<-as.data.frame(SOP_L$SopCorrLNAL)
  rdbesLMWAL<-as.data.frame(SOP_L$L_MWAL)[,c(1:2)]
  
  names(rdbesLNAL)<-c("length","LNAL_rdbes")
  names(rdbesLMWAL)<-c("length","LMWAL_rdbes")
  
  
  orig<-merge(orig,rdbesLNAL, by = "length", all = TRUE)
  orig<-merge(orig,orig_1, by = "length", all = TRUE)
  orig<-merge(orig,rdbesLMWAL, by = "length", all = TRUE)
  
  # Identify the LNAL columns
  lnal_cols <- grep("^LNAL", names(orig), value = TRUE)
  
  # Replace NAs with 0 only in LNAL columns
  orig <- orig %>%
    mutate(across(all_of(lnal_cols), ~replace_na(.x, 0)))
  
  orig_long <- orig %>%
    pivot_longer(
      cols = matches("^(LNAL|LMWAL)"),
      names_to = c("metric", "source"),
      names_pattern = "(LNAL|LMWAL)_(.*)",
      values_to = "value"
    )
  
  # --- Step 2: Define aesthetics
  cols <- hue_pal()(length(unique(orig_long$source)))
  
  linetypes <- c("orig" = "dashed", "rdbes" = "solid")
  linetypes[iyear_1] <- "solid"
  
  linesizes <- c("orig" = 1.5, "rdbes" = 1)
  linesizes[iyear_1] <- 1
  
  # --- Step 3: Custom facet labels for y-axis
  facet_labels <- c(
    "LNAL" = "Number at length (x 1000 ind.)",
    "LMWAL" = "Average weight at length (kg)"
  )
  
  # --- Step 4: Faceted plot with fixed axes, no title
  ggplot(orig_long, aes(x = length, y = value, color = source,
                        linetype = source, size = source)) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = linetypes) +
    scale_size_manual(values = linesizes, guide = "none") +
    scale_x_continuous(
      name = "Length (cm)",
      breaks = seq(floor(min(orig_long$length, na.rm = TRUE)),
                   ceiling(max(orig_long$length, na.rm = TRUE)),
                   by = 5)
    ) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    facet_wrap(
      ~ metric,
      scales = "free_y",
      ncol = 1,
      labeller = as_labeller(facet_labels)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}


Compare_DNAA_plot <- function(irepo, iWG, istock, iyear, DNAA) {
  
  library(tidyverse)
  
  # --- Read current-year DNAA data ---
  orig <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear, "FINAL", paste0(istock, "_SopCorr_D_NAA.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  names(orig) <- c("age", "DNAA_orig")
  
  # --- Read current-year mean weight-at-age ---
  orig_w <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear, "FINAL", paste0(istock, "_D_MWAA.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  names(orig_w) <- c("age", "DMWAA_orig")
  orig <- merge(orig, orig_w)
  
  # --- Read previous-year DNAA and weight-at-age ---
  iyear_1 <- as.character(as.numeric(iyear) - 1)
  orig_1 <- read_delim(
    paste0(irepo, iWG, "/OUTPUT/", istock, "/", iyear_1, "/FINAL/", istock, "_SopCorr_D_NAA.csv"),
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE
  )
  names(orig_1) <- c("age", paste0("DNAA_", iyear_1))
  
  orig_w_1 <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear_1, "FINAL", paste0(istock, "_D_MWAA.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  names(orig_w_1) <- c("age", paste0("DMWAA_", iyear_1))
  orig_1 <- merge(orig_1, orig_w_1)
  
  # --- Read DNAA and DMWAA from SOP_D object ---
  rdbesDNAA <- as.data.frame(DNAA$SopCorrDNAA)
  rdbesDNAA$age <- as.numeric(rownames(rdbesDNAA))
  names(rdbesDNAA) <- c("DNAA_rdbes", "age")
  
  rdbesDMWAA <- as.data.frame(DNAA$D_MWAA)
  rdbesDMWAA$age <- as.numeric(rownames(rdbesDMWAA))
  names(rdbesDMWAA) <- c("DMWAA_rdbes", "age")
  
  # --- Merge all sources ---
  orig <- merge(orig, rdbesDNAA, by = "age", all = TRUE)
  orig <- merge(orig, orig_1, by = "age", all = TRUE)
  orig <- merge(orig, rdbesDMWAA, by = "age", all = TRUE)
  
  # --- Replace NA only in DNAA columns ---
  dnaa_cols <- grep("^DNAA", names(orig), value = TRUE)
  orig <- orig %>% mutate(across(all_of(dnaa_cols), ~replace_na(.x, 0)))
  
  # --- Pivot to long format ---
  orig_long <- orig %>%
    pivot_longer(
      cols = matches("^(DNAA|DMWAA)"),
      names_to = c("metric", "source"),
      names_pattern = "(DNAA|DMWAA)_(.*)",
      values_to = "value"
    )
  
  # --- Define aesthetics ---
  cols <- hue_pal()(length(unique(orig_long$source)))
  linetypes <- c("orig" = "dashed", "rdbes" = "solid")
  linetypes[iyear_1] <- "solid"
  linesizes <- c("orig" = 1.5, "rdbes" = 1)
  linesizes[iyear_1] <- 1
  
  # --- Facet labels for Y-axis ---
  facet_labels <- c(
    "DNAA" = "Number of discards at age (x 1000 ind.)",
    "DMWAA" = "Average weight of discards at age (kg)"
  )
  
  # --- Plot ---
  ggplot(orig_long, aes(x = age, y = value, color = source,
                        linetype = source, size = source)) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = linetypes) +
    scale_size_manual(values = linesizes, guide = "none") +
    scale_x_continuous(
      name = "Age (years)",
      breaks = seq(floor(min(orig_long$age, na.rm = TRUE)),
                   ceiling(max(orig_long$age, na.rm = TRUE)),
                   by = 1)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    facet_wrap(
      ~ metric,
      scales = "free_y",
      ncol = 1,
      labeller = as_labeller(facet_labels)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}


Compare_DNAL_plot <- function(irepo, iWG, istock, iyear, SOP_D) {
  
  library(tidyverse)
  
  # --- Read current-year DNAL data ---
  orig <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear, "FINAL", paste0(istock, "_SopCorr_D_NAL.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  orig <- orig[, -1]
  names(orig) <- c("length", "DNAL_orig")
  
  # --- Read current-year discard weight-at-length ---
  orig_w <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear, "FINAL", paste0(istock, "_D_MWAL.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  orig_w <- orig_w[, -1]
  orig <- merge(orig, orig_w)
  names(orig)[2:3] <- c("DNAL_orig", "DMWAL_orig")
  
  # --- Read previous-year DNAL and weight data ---
  iyear_1 <- as.character(as.numeric(iyear) - 1)
  orig_1 <- read_delim(
    paste0(irepo, iWG, "/OUTPUT/", istock, "/", iyear_1, "/FINAL/", istock, "_SopCorr_D_NAL.csv"),
    ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE
  )
  orig_1 <- as.data.frame(orig_1)[, -1]
  
  orig_w_1 <- read_delim(
    file.path(irepo, iWG, "OUTPUT", istock, iyear_1, "FINAL", paste0(istock, "_D_MWAL.csv")),
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ","),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  orig_w_1 <- orig_w_1[, -1]
  orig_1 <- merge(orig_1, orig_w_1)
  names(orig_1) <- c("length", paste0("DNAL_", iyear_1), paste0("DMWAL_", iyear_1))
  
  # --- Read DNAL and DMWAL from SOP_D object ---
  rdbesDNAL <- as.data.frame(SOP_D$SopCorrDNAL)
  names(rdbesDNAL) <- c("length", "DNAL_rdbes")
  
  rdbesDMWAL <- as.data.frame(SOP_D$D_MWAL)[, 1:2]
  names(rdbesDMWAL) <- c("length", "DMWAL_rdbes")
  
  # --- Merge all sources ---
  orig <- merge(orig, rdbesDNAL, by = "length", all = TRUE)
  orig <- merge(orig, orig_1, by = "length", all = TRUE)
  orig <- merge(orig, rdbesDMWAL, by = "length", all = TRUE)
  
  # --- Replace NA only in DNAL columns ---
  dnal_cols <- grep("^DNAL", names(orig), value = TRUE)
  orig <- orig %>% mutate(across(all_of(dnal_cols), ~replace_na(.x, 0)))
  
  # --- Pivot to long format ---
  orig_long <- orig %>%
    pivot_longer(
      cols = matches("^(DNAL|DMWAL)"),
      names_to = c("metric", "source"),
      names_pattern = "(DNAL|DMWAL)_(.*)",
      values_to = "value"
    )
  
  # --- Define aesthetics ---
  cols <- hue_pal()(length(unique(orig_long$source)))
  
  linetypes <- c("orig" = "dashed", "rdbes" = "solid")
  linetypes[iyear_1] <- "solid"
  
  linesizes <- c("orig" = 1.5, "rdbes" = 1)
  linesizes[iyear_1] <- 1
  
  # --- Facet labels for Y-axis ---
  facet_labels <- c(
    "DNAL" = "Number at length (x 1000 ind.)",
    "DMWAL" = "Average weight at length (kg)"
  )
  
  # --- Plot ---
  ggplot(orig_long, aes(x = length, y = value, color = source,
                        linetype = source, size = source)) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = linetypes) +
    scale_size_manual(values = linesizes, guide = "none") +
    scale_x_continuous(
      name = "Length (cm)",
      breaks = seq(floor(min(orig_long$length, na.rm = TRUE)),
                   ceiling(max(orig_long$length, na.rm = TRUE)),
                   by = 5)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    facet_wrap(
      ~ metric,
      scales = "free_y",
      ncol = 1,
      labeller = as_labeller(facet_labels)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}




getFieldNameMapping<-  function(downloadFromGitHub= TRUE, gitHubDirectory = "https://api.github.com/repos/ices-tools-dev/RDBES/contents/Documents",  fileLocation){
  
  # For testing
  #downloadFromGitHub = TRUE
  #fileLocation <- './tableDefs/'
  #gitHubDirectory <- "https://api.github.com/repos/ices-tools-dev/RDBES/contents/Documents"
  
  
  if (downloadFromGitHub){
    
    myDataModelFiles <- NULL
    myResponse <- GET(gitHubDirectory)
    filesOnGitHub <- content(myResponse)
    
    for (myFile in filesOnGitHub){
      myGitHubFile <- data.frame(fileName = myFile$name, downloadURL = myFile$download_url)
      if (is.null(myDataModelFiles)){
        myDataModelFiles <- myGitHubFile 
      } else {
        myDataModelFiles <- rbind(myDataModelFiles,myGitHubFile)
      }
    }
    # Sub-set to the files we are interested in
    myDataModelFiles <- myDataModelFiles[grepl('^.*Data Model.*xlsx$',myDataModelFiles$fileName),]
    
    print(paste("Downloading ",nrow(myDataModelFiles), " files from GitHub", sep =""))
    
    # Download our files
    for (i in 1:nrow(myDataModelFiles)){
      aDataModelFile <- httr::GET(myDataModelFiles[i,'downloadURL'])
      aDataModelFileContent <- content(aDataModelFile, "raw")
      # save the file locally
      myFileConnection = file(paste(fileLocation,myDataModelFiles[i,'fileName'], sep = ""), "wb")
      writeBin(aDataModelFileContent, myFileConnection)
      aDataModelFile <- NA
      aDataModelFileContent <- NA
      close(myFileConnection)
    }
    
    print("Finished downloading")
    
  }
  
  # Now we'll read the spreadsheets
  # (Need to find the names of the files again in case we haven't dowloaded them in this function call)
  
  filesToRead <- list.files(path = fileLocation, pattern = "*.xlsx", recursive = FALSE, full.names = FALSE)
  
  dataModel <- list()
  
  # get the contents of each relevent worksheet in our spreadsheets
  for (myfile in filesToRead){
    
    myFileLocation <- paste(fileLocation,myfile, sep = "")
    myFileSheets <- excel_sheets(myFileLocation)
    
    # CE CL
    if (grepl('^.*CL CE.*xlsx$',myfile)){
      
      print("Loading CL CE names")
      # Add the sheets to the dataModel list
      dataModel[['CE']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*CE.*",myFileSheets)])
      dataModel[['CL']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*CL.*",myFileSheets)])
    } 
    # VD SL
    else if (grepl('^.*VD SL.*xlsx$',myfile)){
      
      print("Loading VD SL names")
      # Add the sheets to the dataModel list
      dataModel[['VD']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*Vessel.*",myFileSheets)])
      dataModel[['SL']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*Species List.*",myFileSheets)])
      dataModel[['IS']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*Individual Species.*",myFileSheets)])
      
    } 
    # CS
    else if (grepl('^.*CS.xlsx$',myfile)){
      #else if (myfile == "RDBES Data Model.xlsx"){
      
      for (aFileSheet in myFileSheets) {
        if (!grepl(".*Model.*",aFileSheet)){
          print(paste("Loading ", aFileSheet, " names", sep = ""))
          myDataModel <- read_excel(myFileLocation,sheet = aFileSheet)
          dataModel[[aFileSheet]] <- myDataModel
        }
        
      }
    }
  }
  
  # Put the field names and R names from each entry in the list into a single data frame
  
  myNameMappings <- NULL
  
  for (i in 1:length(dataModel)){
    
    myDataModelEntry <- dataModel[[i]]
    validColumnNames <- names(myDataModelEntry)[names(myDataModelEntry) %in% c("Field Name","R Name")]
    
    if (length(validColumnNames == 2)) {
      aNameMapping <- myDataModelEntry[,c("Field Name","R Name")]
      
      # Add in the Table Name - based on the first 2 letters of the first entry
      if (nrow(aNameMapping)>0) {
        tableName <- substr(aNameMapping[1,1],1,2)
      } else {
        tableName <- NA
      }
      aNameMapping$TableName <- tableName
      
      myNameMappings <- rbind(myNameMappings,aNameMapping)
    } else {
      print(paste("Not including ",names(dataModel)[i], " in mapping due to invalid column names",sep=""))
    }
    
  }
  
  # Remove any NAs
  if (!is.null(myNameMappings)){
    myNameMappings <- myNameMappings[!is.na(myNameMappings[,"Field Name"]) & !is.na(myNameMappings[,"R Name"]),]
  }
  
  myNameMappings
  
}




changeFieldNames<-  function(RDBESdata, fieldNameMap, typeOfChange){
  
  if (!typeOfChange %in% c("RtoDB", "DBtoR")) stop("typeOfChange parameter should be either RtoDB or DBtoR")
  
  for (i in 1:length(RDBESdata)){
    
    if (!is.null(RDBESdata[[i]]) && nrow(RDBESdata[[i]]) > 0) {
      RDBESdata[[i]] <- changeFieldNamesForFrame(
        frameToRename = RDBESdata[[i]],
        fieldNameMap = fieldNameMap,
        typeOfChange = typeOfChange
      )
    }
    
  }
  
  RDBESdata
  
}  




#' changeFieldNamesForFrame Change the field names of an RDBES data frame either from database names to R names or vice versa
#'
#' @param frameToRename The RDBES data frame we want to rename
#' @param fieldNameMap The data frame holding our names mappings
#' @param typeOfChange Either RtoDB or DBtoR
#'
#' @return
#' @export
#'
#' @examples changeFieldNames(frameToRename = x, fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR")
changeFieldNamesForFrame <- function(frameToRename, fieldNameMap, typeOfChange){
  
  # For testing
  #frameToRename <- myRDBESData[["BV"]]
  #fieldNameMap <- fieldNameMapping
  #typeOfChange <- "DBtoR"
  
  if (!typeOfChange %in% c("RtoDB", "DBtoR")) stop("typeOfChange parameter should be either RtoDB or DBtoR")
  
  # Get the current column names into a data frame
  myDF <- data.frame(name = names(frameToRename), stringsAsFactors = FALSE)
  
  # Assume the first entry is always the primary key of the table and extract the table name
  myTableName <- substr(myDF[1,"name"],1,2)
  
  # IF change DB to R
  if (typeOfChange == "DBtoR"){
    
    # Left join our current names against the replacement names (match to DB names)
    myMapping <- left_join(myDF,fieldNameMap[fieldNameMap$TableName==myTableName,c("Field Name","R Name")], by=c("name" = "Field Name"))
    
    # Make sure we don't have any NAs in the list of names we'll use
    myMapping[,"R Name"] <- ifelse(is.na(myMapping[,"R Name"]),myMapping$name,myMapping[,"R Name"])
    
    # set the new names
    myNewNames <- myMapping[,"R Name"]
    
  } else if (typeOfChange == "RtoDB"){
    
    # Left join our current names against the replacement names (match to R names)
    myMapping <- left_join(myDF,fieldNameMap[fieldNameMap$TableName==myTableName,c("Field Name","R Name")], by=c("name" = "R Name"))
    
    # Make sure we don't have any NAs in the list of names we'll use
    myMapping[,"Field Name"] <- ifelse(is.na(myMapping[,"Field Name"]),myMapping$name,myMapping[,"Field Name"])
    
    # set the new names
    myNewNames <- myMapping[,"Field Name"]
    
  }
  
  # Change the names of our data frame
  names(frameToRename) <- myNewNames
  
  # return the data frame with the new names
  frameToRename
  
}

convert_metier_labels <- function(df) {
  
  # Detect which column exists: CLmetier6 or CEmetier6
  
  metier_col <- if ("CLmetier6" %in% names(df)) {
    
    "CLmetier6"
    
  } else if ("CEmetier6" %in% names(df)) {
    
    "CEmetier6"
    
  } else {
    
    stop("Neither 'CLmetier6' nor 'CEmetier6' column found in the data frame.")
    
  }
  
  
  
  df %>%
    mutate(Fleet = case_when(
      .data[[metier_col]] %in% c(
      "DRB_MOL_0_0_0", "FPO_MCD_0_0_0", "LHP_DEF_0_0_0", "DRB_MOL_>0_0_0","PTM_SPF_0_0_0", "LLS_DEF_0_0_0", "SDN_DEF_>0_0_0") ~ "MIS_MIS_0_0_0",
      .data[[metier_col]] == "GNS_DEF_>=220_0_0" ~ "GNS_DEF_>=220_0_0_all",
      .data[[metier_col]] == "GNS_DEF_110-156_0_0" ~ "GNS_DEF_all_0_0_all",
      .data[[metier_col]] == "GNS_DEF_100-119_0_0" ~ "GNS_DEF_100-119_0_0_all",
      .data[[metier_col]] == "GNS_DEF_120-219_0_0" ~ "GNS_DEF_120-219_0_0_all",
      .data[[metier_col]] == "GNS_DEF_90-99_0_0" ~ "GNS_DEF_90-99_0_0_all",
      .data[[metier_col]] == "GTR_DEF_0_0_0" ~ "GTR_DEF_all_0_0_all",
      .data[[metier_col]] == "GTR_DEF_90-99_0_0" ~ "GTR_DEF_90-99_0_0_all",
      .data[[metier_col]] == "GTR_DEF_100-119_0_0" ~ "GTR_DEF_100-119_0_0_all",
      .data[[metier_col]] == "OTB_CRU_16-31_0_0" ~ "OTB_CRU_16-31_0_0_all",
      .data[[metier_col]] == "OTB_DEF_>=120_0_0" ~ "OTB_DEF_>=120_0_0_all",
      .data[[metier_col]] == "OTB_DEF_100-119_0_0" ~ "OTB_DEF_100-119_0_0_all",
      .data[[metier_col]] == "OTB_DEF_70-99_0_0" ~ "OTB_DEF_70-99_0_0_all",
      .data[[metier_col]] == "OTB_MCD_70-99_0_0" ~ "OTB_CRU_70-99_0_0_all",
      .data[[metier_col]] == "SSC_DEF_>=120_0_0" ~ "SSC_DEF_>=120_0_0_all",
      .data[[metier_col]] == "SSC_DEF_100-119_0_0" ~ "SSC_DEF_100-119_0_0_all",
      .data[[metier_col]] == "SSC_DEF_70-99_0_0" ~ "SSC_DEF_70-99_0_0_all",
      .data[[metier_col]] == "SSC_DEF_32-69_0_0" ~ "SSC_DEF_32-69_0_0_all",
      .data[[metier_col]] == "TBB_CRU_16-31_0_0" ~ "TBB_CRU_16-31_0_0_all",
      .data[[metier_col]] == "TBB_DEF_>=120_0_0" ~ "TBB_DEF_>=120_0_0_all",
      .data[[metier_col]] == "TBB_DEF_70-99_0_0" ~ "TBB_DEF_70-99_0_0_all",
      .data[[metier_col]] == "TBB_DEF_100-119_0_0" ~ "TBB_DEF_100-119_0_0_all",
      .data[[metier_col]] == "OTB_DEF_32-69_0_0" ~ "OTB_DEF_32-69_0_0_all",
      TRUE ~ .data[[metier_col]]
    ))
  }

#Catch table: landings and discard estimates
make_catch_TBB <- function(iWG_nice, istock, iAphiaID, imet, iyear, catchCategory, RW, SA_stock, CL_stock, iYQ = "YEAR", iRaisingLevel) {
  
  recordType <- "CN"
  vesselFlagCountry <- "BE"
  workingGroup <- iWG_nice
  stock <- istock
  speciesCode <- iAphiaID
  areaType <- "ICESArea"
  fisheriesManagementUnit <- ""
  fleetType <- "Fleet"
  fleetValue <- paste0(imet, "_all")
  originType <- "WGEstimate"
  variableType <- "WeightLive"
  variableUnit <- "t"

  comment <- ""
  
  # --- Determine if YEAR or QUARTERS ---
  if (toupper(iYQ) == "YEAR") {
    
    # Check if catchCategory is "Lan" - if so, split by quarters and areas
    if (catchCategory == "Lan") {
      
      quarters_in_data <- sort(unique(CL_stock$CLquar))
      results <- list()
      
      
      if((iRaisingLevel$Lan != "XX" & iRaisingLevel$Dis != "XX")) {      # Common domain values for all quarters (YEAR-level domains)
        domainCatchDis <- paste(iyear, "All", fleetValue, sep = "_")
        domainCatchBMS <- ""
        domainBiology <- paste(iyear, "All", fleetValue, catchCategory, sep = "_")
      } else {
        domainCatchDis <- ""
        domainCatchBMS <- ""
        domainBiology <- ""
        }

      
      for (q in quarters_in_data) {
        seasonType <- "Quarter"
        seasonValue <- q
        
        SA_q <- SA_stock[SA_stock$Quarter == q, ]
        numPSUs <- ""
        numTrips <- ""
        PSU <- ""
        # Get quarterly landings by area
        CL_stock_quarter <- CL_stock %>% 
          filter(CLquar == q) %>% 
          group_by(CLarea) %>% 
          summarise(LW = sum(CLsciWeight) / 1000, .groups = 'drop')
        
        # Create a row for each area
        for (i in 1:nrow(CL_stock_quarter)) {
          total <- CL_stock_quarter$LW[i]
          areaValue <- CL_stock_quarter$CLarea[i]
          variance <- ""
          
          
          df_q <- data.frame(
            recordType, vesselFlagCountry, year = iyear, workingGroup, stock, speciesCode, catchCategory,
            seasonType, seasonValue, areaType, areaValue, fisheriesManagementUnit,
            metier6 = imet, fleetType, fleetValue, domainCatchDis, domainCatchBMS,
            domainBiology, originType, variableType, variableUnit, total, variance, PSU,
            numPSUs, numTrips, comment, stringsAsFactors = FALSE
          )
          
          results[[paste0("Q", q, "_", areaValue)]] <- df_q
        }
      }
      
      return(do.call(rbind, results))
      
    } else {
      
      areaValue <- paste(sort(unique(CL_stock$CLarea)), collapse = ";")
      # Original YEAR logic for non-landings (Discards)
      seasonType <- "Year"
      seasonValue <- iyear
      
      domainCatchDis <- paste(iyear, "All", fleetValue, sep = "_")
      domainCatchBMS <- ""
      domainBiology <- paste(iyear, "All", fleetValue, catchCategory, sep = "_")
      
      # Total weight
      total <- if (catchCategory == "Dis") round(RW$DW/1000,3) else NA
      variance <- if (catchCategory == "Dis") round(RW$DW_Var/1000000) else ""
      PSU <- "FishingTrip"
      # Number of PSUs/trips
      numPSUs <- length(unique(SA_stock$FTunitName))
      numTrips <- length(unique(SA_stock$FTunitName))
      numPSUs <- length(unique(SA_stock$FTunitName[SA_stock$SAcatchCategory == catchCat]))
      numTrips <- length(unique(SA_stock$FTunitName[SA_stock$SAcatchCategory == catchCat]))
      
      df <- data.frame(
        recordType, vesselFlagCountry, year = iyear, workingGroup, stock, speciesCode, catchCategory,
        seasonType, seasonValue, areaType, areaValue, fisheriesManagementUnit,
        metier6 = imet, fleetType, fleetValue, domainCatchDis, domainCatchBMS,
        domainBiology, originType, variableType, variableUnit, total, variance, PSU,
        numPSUs, numTrips, comment, stringsAsFactors = FALSE
      )
      
      return(df)
    }
    
  } else {
    
    if (catchCategory == "Lan") {
      
      quarters_in_data <- sort(unique(CL_stock$CLquar))
      quarters_requested <- as.numeric(gsub("Q", "", unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))))
      results <- list()
      
      for (q in quarters_in_data) {
        seasonType <- "Quarter"
        seasonValue <- q
        
        SA_q <- SA_stock[SA_stock$Quarter == q, ]
        numPSUs <- ""
        numTrips <- ""
        
        # Get quarterly landings by area
        CL_stock_quarter <- CL_stock %>% 
          filter(CLquar == q) %>% 
          group_by(CLarea) %>% 
          summarise(LW = sum(CLsciWeight) / 1000, .groups = 'drop')
        
        # Create a row for each area
        for (i in 1:nrow(CL_stock_quarter)) {
          total <- CL_stock_quarter$LW[i]
          areaValue <- CL_stock_quarter$CLarea[i]
          variance <- ""
          PSU <- ""
          
          if(q %in% quarters_requested) {
            domainCatchDis <- paste0(iyear, "_Q", q, "_", "All", "_", fleetValue)
            domainCatchBMS <- ""
            domainBiology  <- paste0(iyear, "_Q", q, "_", "All", "_", fleetValue, "_", catchCategory)
          } else{
            domainCatchDis <- ""
            domainCatchBMS <- ""
            domainBiology  <- ""
          }
          
          df_q <- data.frame(
            recordType, vesselFlagCountry, year = iyear, workingGroup, stock, speciesCode, catchCategory,
            seasonType, seasonValue, areaType, areaValue, fisheriesManagementUnit,
            metier6 = imet, fleetType, fleetValue, domainCatchDis, domainCatchBMS,
            domainBiology, originType, variableType, variableUnit, total, variance, PSU,
            numPSUs, numTrips, comment, stringsAsFactors = FALSE
          )
          
          results[[paste0("Q", q, "_", areaValue)]] <- df_q
        }
      }
      
      return(do.call(rbind, results))
      
    } else {
      areaValue <- paste(sort(unique(CL_stock$CLarea)), collapse = ";")
      PSU <- "FishingTrip"
      # ==== CASE 2: QUARTERLY ====
      quarters_requested <- as.numeric(gsub("Q", "", unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))))
      results <- list()
      
      for (q in quarters_requested) {
        seasonType <- "Quarter"
        seasonValue <- q
        
        SA_q <- SA_stock[SA_stock$Quarter == q, ]
        numPSUs <- length(unique(SA_q$FTunitName))
        numTrips <- length(unique(SA_q$FTunitName))
        
        if (q %in% quarters_requested) {
          # Requested quarter → include discard estimates and domains
          total <- if (catchCategory == "Lan") RW$LW[RW$Quarter == q]/1000 
          else 
            if (catchCategory == "Dis") RW$DW[RW$Quarter == q]/1000  else NA
          variance <- if (catchCategory == "Dis") round(RW$DW_Var[RW$Quarter == q]/1000000, 2) else ""
          
          domainCatchDis <- paste0(iyear, "_Q", q, "_", "All", "_", fleetValue)
          domainCatchBMS <- ""
          domainBiology  <- paste0(iyear, "_Q", q, "_", "All", "_", fleetValue, "_", catchCategory)
          
        } else {
          # Not requested → report LW only, no domains, no variance
          total <- if (catchCategory == "Lan") RW$LW[RW$Quarter == q] else NA
          variance <- ""
          domainCatchDis <- ""
          domainCatchBMS <- ""
          domainBiology  <- ""
        }
        
        df_q <- data.frame(
          recordType, vesselFlagCountry, year = iyear, workingGroup, stock, speciesCode, catchCategory,
          seasonType, seasonValue, areaType, areaValue, fisheriesManagementUnit,
          metier6 = imet, fleetType, fleetValue, domainCatchDis, domainCatchBMS,
          domainBiology, originType,variableType, variableUnit, total, variance, PSU,
          numPSUs, numTrips, comment, stringsAsFactors = FALSE
        )
        
        results[[paste0("Q", q)]] <- df_q
      }
      
      return(do.call(rbind, results))
    }
    }
    }


make_catch_noTBB <- function(iWG_nice, istock, ispecies, iyear, iquarter, imet, ifleetValue, iareaValue, total) {
  
  recordType <- "CN"
  vesselFlagCountry <- "BE"
  year <- iyear
  workingGroup <- iWG_nice
  stock <- istock
  speciesCode <- ispecies
  catchCategory <- "Lan"         
  seasonType <- "Quarter"
  seasonValue <- iquarter
  areaType <- "ICESArea"
  areaValue <- iareaValue
  fisheriesManagementUnit <- ""
  metier6 <- imet
  fleetType <- "Fleet"
  fleetValue <- ifleetValue
  domainCatchDis <- ""
  domainCatchBMS <- ""
  domainBiology <- ""
  originType <- "WGEstimate"
  variableType <- "WeightLive"
  variableUnit <- "t"
  variance <- ""
  PSU <- ""
  numPSUs <- ""
  numTrips <- ""
  comment <- ""
  
  # Make dataframe in RCEF
  data.frame(
    recordType = recordType,
    vesselFlagCountry = vesselFlagCountry,
    year = year,
    workingGroup = workingGroup,
    stock = istock,
    speciesCode = speciesCode,
    catchCategory = catchCategory,
    seasonType = seasonType,
    seasonValue = seasonValue,
    areaType = areaType,
    areaValue = areaValue,
    fisheriesManagementUnit = fisheriesManagementUnit,
    metier6 = metier6,
    fleetType = fleetType,
    fleetValue = fleetValue,
    domainCatchDis = domainCatchDis,
    domainCatchBMS = domainCatchBMS,
    domainBiology = domainBiology,
    originType = originType,
    variableType = variableType,
    variableUnit = variableUnit,
    total = total/1000,
    variance = variance,
    PSU = PSU,
    numPSUs = numPSUs,
    numTrips = numTrips,
    comment = comment,
    stringsAsFactors = FALSE
  )
}

empty_catch_df <- function() {
  
  data.frame(
    recordType = character(),
    vesselFlagCountry = character(),
    year = integer(),
    workingGroup = character(),
    stock = character(),
    speciesCode = integer(),
    catchCategory = character(),
    seasonType = character(),
    seasonValue = integer(),
    areaType = character(),
    areaValue = character(),
    fisheriesManagementUnit = character(),
    metier6 = character(),
    fleetType = character(),
    fleetValue = character(),
    domainCatchDis = character(),
    domainCatchBMS = character(),
    domainBiology = character(),
    originType = character(),
    variableType = character(),
    variableUnit = character(),
    total = numeric(),
    variance = numeric(),
    PSU = character(),
    numPSUs = numeric(),
    numTrips = numeric(),
    comment = character(),
    
    stringsAsFactors = FALSE
  )
}

############################################################
# Helper function to build each tibble consistently
############################################################
make_distribution_tibble <- function(
    iyear, iYQ = "YEAR", iWG_nice, istock, iAphiaID, catchCat, domainBiology,
    distributionType, variableType, SA_stock, df, df_raw, has_weight = TRUE, LWK_source= ""
) {
  
  if (toupper(iYQ) == "YEAR") {
    period_labels <- "YEAR"
    
  } else {
    period_labels <- unlist(regmatches(iYQ, gregexpr("Q\\d+", iYQ)))

  }
  
  distributions <- vector("list", length(period_labels))
  
  domainBiology_df <- data.frame(domainBiology = domainBiology) %>%
    dplyr::mutate(
      Year = stringr::str_extract(domainBiology, "^\\d{4}"),
      Quarter = stringr::str_extract(domainBiology, "(?<=_)Q\\d"),
      CatchCat = stringr::str_extract(domainBiology, ".{3}$")
    )
  
  # Helper: select df table
  select_df <- function(catchCat, distributionType, variableType, df) {
    switch(paste(catchCat, distributionType, variableType, sep = "_"),
           "Lan_LengthTotal_WeightLive" = df,
           "Dis_LengthTotal_WeightLive" = df,
           "Lan_LengthTotal_Number" = df,
           "Dis_LengthTotal_Number" = df,
           "Lan_Age_WeightLive" = df,
           "Dis_Age_WeightLive" = df,
           "Lan_Age_Number" = df,
           "Dis_Age_Number" = df,
           stop("Unknown combination"))
  }
  
  df_table <- select_df(catchCat, distributionType, variableType, df)
  

  for (i in seq_along(period_labels)) {
    period <- period_labels[i]
    
    # --- Filter by period ---
    if (period == "YEAR") {
      SA_period <- SA_stock
      df_period <- df_table
      # Filter df_raw by catchCat to avoid duplication
      if (!is.null(has_weight) && has_weight) {
        df_raw <- df_raw %>% dplyr::filter(SAcatchCat == catchCat)
      } else {
        df_period <- NULL
      }
      seasonType <- "Year"
      seasonValue <- iyear
    } else {
      qnum <- as.numeric(gsub("Q", "", period))
      SA_period <- SA_stock[SA_stock$Quarter == qnum, ]
      if ("Quarter" %in% names(df_table)) {
        df_period <- df_table[df_table$Quarter == period, ]
      } else {
        df_period <- df_table
      }
      if (!is.null(has_weight) && has_weight) {
        if ("Quarter" %in% names(df_raw)) {
          df_raw <- df_raw %>%
            dplyr::filter(SAcatchCat == catchCat, Quarter == period)
        } else {
          df_raw <- df_raw %>%
            dplyr::filter(SAcatchCat == catchCat)
        }
      } else {
        df_period <- NULL
      }
      seasonType <- "Quarter"
      seasonValue <- qnum
    }
    

    # --- Pick distributionClass, value, variance, numMeasurements ---
    if (distributionType == "LengthTotal") {
      
      if (!is.null(has_weight) && has_weight && variableType == "Number" && !is.null(df_period) && nrow(df_period) > 0) {
        # Only override for LengthTotal + Number, filtered to catchCat
        distributionClass_vec <- df_period$length * 10
        value_vec <- df_period$corr_numbers
        variance_vec <- ""
        numMeasurements_vec <- df_period$number_raw_LFD
        numPSUs <- length(unique(SA_period$FTunitName[SA_period$SAcatchCategory == catchCat]))
        numTrips <- length(unique(SA_period$FTunitName[SA_period$SAcatchCategory == catchCat]))

      } else {
        # Default behavior
        distributionClass_vec <- df_period$length * 10
        if (variableType == "WeightLive") {
          value_vec <- df_period$indW2 
          variance_vec <- round(as.numeric(df_period$indW2_var)) 
          numMeasurements_vec <- df_period$number_raw_LWK    
          if (LWK_source == "other_source") {
            numPSUs <- 0
            numTrips <- 0
          } else {
            numPSUs <- length(unique(SA_period$FTunitName))
            numTrips <- length(unique(SA_period$FTunitName))
          }
          
          if (is.null(has_weight) && !has_weight && variableType != "Number") {
            numMeasurements_vec <- ""
          }
        } else { # Number
          value_vec <- df_period$corr_numbers
          variance_vec <- ""
          numMeasurements_vec <- df_period$number_raw_LFD
        }
      }
      
    } else { # Age
      if (variableType == "WeightLive") {
        distributionClass_vec <- as.numeric(df_period$Age)
        value_vec <- df_period$MWAA 
        variance_vec <- round(as.numeric(df_period$MWAA_var)) 
        numMeasurements_vec <- df_period$number_raw
        if (LWK_source == "other_source") {
          numPSUs <- 0
          numTrips <- 0
        } else {
        numPSUs <- length(unique(SA_period$FTunitName[SA_period$SAcatchCategory == catchCat]))
        numTrips <- length(unique(SA_period$FTunitName[SA_period$SAcatchCategory == catchCat]))
        }
        if (is.null(has_weight) && !has_weight && variableType != "Number") {
          numMeasurements_vec <- ""
        }
      } else {
        distributionClass_vec <- as.numeric(df_period$Age)
        value_vec <- df_period$SopCorrNAA      
        variance_vec <- ""
        numMeasurements_vec <- df_period$number_raw 
        numPSUs <- length(unique(SA_period$FTunitName[SA_period$SAcatchCategory == catchCat]))
        numTrips <- length(unique(SA_period$FTunitName[SA_period$SAcatchCategory == catchCat]))
        if (is.null(has_weight) && !has_weight && variableType != "Number") {
          numMeasurements_vec <- ""
        } 
      }
    }

    if (period == "YEAR") {
      domainBiology_i <- unique(
        domainBiology_df$domainBiology[
          domainBiology_df$Year == iyear &
            domainBiology_df$CatchCat == catchCat
        ]
      )
    } else {
      domainBiology_i <- unique(
        domainBiology_df$domainBiology[
          domainBiology_df$Year == iyear &
            domainBiology_df$Quarter == period &
            domainBiology_df$CatchCat == catchCat
        ]
      )
    }
    
    # --- Safety: skip if no data ---
    if (length(distributionClass_vec) == 0 || length(value_vec) == 0) next
    
    distributions[[i]] <- tibble(
      
      recordType = "DN",
      vesselFlagCountry = "BE",
      year = iyear,
      workingGroup = iWG_nice,
      stock = istock,
      speciesCode = iAphiaID,
      catchCategory = catchCat,
      domainBiology = domainBiology_i,
      distributionType = distributionType,
      distributionUnit = as.character(ifelse(distributionType == "LengthTotal", "mm", "y")),
      distributionClass = distributionClass_vec,
      ageGroupPlus = "",
      attributeType ="",
      attributeValue = "",
      variableType = variableType,
      variableUnit = as.character(ifelse(variableType == "WeightLive", "g", "NE3")),
      valueType = as.character(ifelse(variableType == "WeightLive", "Mean", "Total")),
      value =  as.numeric(ifelse(variableType == "WeightLive", round(value_vec,3), round(value_vec,3))),
      variance = as.numeric(ifelse(variableType == "WeightLive", round(as.numeric(variance_vec)), "")),
      PSU = "FishingTrip",
      numPSUs = numPSUs,
      numTrips = numTrips,
      numMeasurements =  ifelse(is.na(numMeasurements_vec), 0, numMeasurements_vec)
    )
  }
  
  bind_rows(distributions)
}

empty_distribution_df <- function() {
  
  data.frame(
    recordType = character(),
    vesselFlagCountry = character(),
    year = integer(),
    workingGroup = character(),
    stock = character(),
    speciesCode = integer(),
    catchCategory = character(),
    domainBiology = character(),
    distributionType = character(),
    distributionUnit = character(),
    distributionClass = numeric(),
    ageGroupPlus = logical(),
    attributeType = character(),
    attributeValue = character(),
    variableType = character(),
    variableUnit = character(),
    valueType = character(),
    value = numeric(),
    variance = numeric(),
    PSU = character(),
    numPSUs = numeric(),
    numTrips = numeric(),
    numMeasurements = numeric(),
    
    stringsAsFactors = FALSE
  )
}

make_effort <- function(total, iWG, imet, ifleetValue, iyear, iquarter, iareaValue, CE_summary) {
  
  recordType = "EN"
  vesselFlagCountry <- "BE"
  year <- iyear
  workingGroup <- iWG_nice
  catchCategory <- "Lan"         
  seasonType <- "Quarter"
  seasonValue <- iquarter
  areaType <- "ICESArea"
  areaValue <- iareaValue
  fisheriesManagementUnit <- ""
  metier6 <- imet
  fleetType <- "Fleet"
  fleetValue <- ifleetValue
  originType <- "WGEstimate"
  variableType <- "kWd-at-sea"
  
  # Make dataframe in RCEF
  data.frame(
    recordType = recordType,
    vesselFlagCountry = vesselFlagCountry,
    year = year,
    workingGroup = workingGroup,
    seasonType = seasonType,
    seasonValue = seasonValue,
    areaType = areaType,
    areaValue = areaValue,
    fisheriesManagementUnit = fisheriesManagementUnit,
    metier6 = metier6,
    fleetType = fleetType,
    fleetValue = fleetValue,
    originType = originType,
    variableType = variableType,
    total = total,
    stringsAsFactors = FALSE
  )
}

empty_effort_df <- function() {
  
  data.frame(
    recordType = character(),
    vesselFlagCountry = character(),
    year = integer(),
    workingGroup = character(),
    seasonType = character(),
    seasonValue = integer(),
    areaType = character(),
    areaValue = character(),
    fisheriesManagementUnit = character(),
    metier6 = character(),
    fleetType = character(),
    fleetValue = character(),
    originType = character(),
    variableType = character(),
    total = numeric(),
    
    stringsAsFactors = FALSE
  )
}


library(ggplot2)
library(dplyr)
library(ggpubr)


plot_sop_general <- function(df, corr_values = NULL, title_prefix = "", mode = c("number", "weight"), ncol= NULL) {
  
  mode <- match.arg(mode)
  
  # Ensure Quarter exists
  if (!"Quarter" %in% names(df)) {
    df <- df %>% mutate(Quarter = "Year")
  }
  
  # --- Create annotation dataframe (for correction factors) ---
  annot_df <- NULL
  if (!is.null(corr_values) && mode == "number") {
    quarters <- unique(df$Quarter)
    n_corr <- min(length(quarters), length(corr_values))
    annot_df <- data.frame(
      Quarter = quarters[1:n_corr],
      corr_value = corr_values[1:n_corr],
      stringsAsFactors = FALSE
    ) %>%
      rowwise() %>%
      mutate(
        x_pos = max(df$length[df$Quarter == Quarter], na.rm = TRUE),
        y_pos = max(df$number[df$Quarter == Quarter], na.rm = TRUE) * 1.05
      )
  }
  
  # --- Build the plot ---
  p <- ggplot(df, aes(x = length))
  
  if (mode == "number") {
    # 🟦 Numbers mode: Raw vs Corrected
    p <- p +
      geom_line(aes(y = number, color = "Raw Number"), size = 1.2) +
      geom_point(aes(y = number, color = "Raw Number"), shape = 21, fill = "steelblue", size = 2) +
      geom_line(aes(y = corr_numbers, color = "Corrected Number"), size = 1.2) +
      geom_point(aes(y = corr_numbers, color = "Corrected Number"), shape = 21, fill = "tomato", size = 2) +
      scale_color_manual(values = c("Raw Number" = "steelblue", "Corrected Number" = "tomato")) +
      scale_y_continuous(name = "Number (x1000)")
    
  } else if (mode == "weight") {
    # ⚖️ Mean Weight mode: with CI ribbons
    p <- p +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = "gray70", alpha = 0.3) +
      geom_line(aes(y = indW2, color = "Mean Weight"), size = 1.2) +
      geom_point(aes(y = indW2, color = "Mean Weight"), shape = 21, fill = "steelblue", size = 2) +
      scale_color_manual(values = c("Mean Weight" = "steelblue")) +
      scale_y_continuous(name = "Individual Weight (g)")
  }
  
  # --- Add correction text only for number mode ---
  if (!is.null(annot_df) && mode == "number") {
    p <- p +
      geom_text(
        data = annot_df,
        aes(x = x_pos, y = y_pos, label = paste0("Corr = ", round(corr_value, 3))),
        inherit.aes = FALSE,
        hjust = 1, vjust = 0,
        size = 3.5
      )
  }
  
  # --- Final formatting ---
  p <- p +
    scale_x_continuous(name = "Length (cm)", breaks = df$length) +
    labs(title = title_prefix) +
    theme_bw(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    facet_wrap(~Quarter, ncol = ncol)
  
  return(p)
}

library(ggplot2)
library(dplyr)
library(ggpubr)

library(ggplot2)
library(dplyr)
library(ggpubr)

plot_alk <- function(df, mode = c("number", "weight"), title_prefix = "", ncol=1) {
  
  mode <- match.arg(mode)
  
  # Ensure Quarter exists
  if (!"Quarter" %in% names(df)) {
    df <- df %>% mutate(Quarter = "Year")
  }
  
  # Start ggplot
  p <- ggplot(df, aes(x = Age))
  
  if (mode == "number") {
    # Bars + line for corrected number
    p <- p +
      geom_line(aes(y = SopCorrNAA, color = "Corrected Number"), size = 1.2) +
      geom_point(aes(y = SopCorrNAA , color = "Corrected Number"), size = 2) +
      scale_fill_manual(values = c("Corrected Number" = "tomato")) +
      scale_color_manual(values = c("Corrected Number" = "tomato")) +
      labs(y = "Number (1000)")
    
  } else if (mode == "weight") {
    # Line + ribbon for weight with CI
    p <- p +
      geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = "steelblue", alpha = 0.3) +
      geom_line(aes(y = MWAA, color = "Mean Weight"), size = 1.2) +
      geom_point(aes(y = MWAA , color = "Mean Weight"), size = 2, fill = "steelblue", shape = 21) +
      scale_color_manual(values = c("Mean Weight" = "steelblue")) +
      labs(y = "Individual Weight (g)")
  }
  
  # Common formatting
  p <- p +
    labs(title = title_prefix, x = "Age") +
    scale_x_continuous(breaks = df$Age) +
    facet_wrap(~Quarter, ncol = ncol) +
    theme_bw(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  return(p)
}


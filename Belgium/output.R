rm(list=ls())
gc()

source("boot/data/Utilities.R")

mkdir("output/")


# Load nested inputs and model outputs
load(file ="data/input_data_AllStocks.Rdata")
load(file = "model/raised_outputs_AllStocks.Rdata")

# Storage for all stocks
all_catch <- list()
all_distribution <- list()
all_effort <- list()

iraise<-"sol.27.7fg_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <- "her.27.25-2932_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <-"mac.27.nea_TBB_DEF_70-99_0_0_2024_YEAR" 
iraise <- "bll.27.3a47de_TBB_DEF_70-99_0_0_2024_YEAR"
iraise <- "ple.27.420_TBB_DEF_70-99_0_0_2024_Q1Q4"
iraise <- "whg.27.7b-ce-k_TBB_DEF_70-99_0_0_2024_YEAR"

# Loop over each stock
for (iraise in names(all_outputs)) {
  
  # Extract the stock code (everything before the first "-")
  istock <- sub("_.*", "", iraise)
  
  msg("Processing raising of :", iraise, "\n")
  
  stock_output <- all_outputs[[iraise]]
  stock_input  <- all_inputs[[iraise]]
  
  #### TEMPORAL DATA ####
  iyear   <- stock_input$metadata$iyear
  iYQ  <- stock_input$metadata$iYQ
  iYQ_comb <- stock_input$metadata$iYQ_comb
  #### METIER DATA ####
  imet   <- stock_input$metadata$imet
  
  #### WHAT TO RAISE DATA #### 
  ########  WEIGHTS/ LENGTHS /LWK/AGE / ALL
  iRaisingLevel   <- stock_input$metadata$iRaisingLevel
  ########  calc LWK or other_source LWK
  iLWK <- stock_input$metadata$iLWK
  iLWK_source <- stock_input$metadata$iLWK_source
  iLWK_a <- stock_input$metadata$iLWK_a
  iLWK_b <- stock_input$metadata$iLWK_b
  
  iareas <- stock_input$metadata$iareas
  iFAO   <- stock_input$metadata$iFAO
  iWG_nice   <- stock_input$metadata$iWG_nice
  iAphiaID   <- stock_input$metadata$iAphiaID
  
  # Datasets
  CE_All <-  stock_input$CE_all$CE
  CL_All <- stock_input$CL_all$CL
  CL_stock <- stock_input$CL_stock
  SA_year <- stock_input$SA_stock
  
  # Model outputs
  RW   <- stock_output$RW
  LF   <- stock_output$LF
  MWAL_NAL_L <- stock_output$SOP_L$MWAL
  MWAL_NAL_D <- stock_output$SOP_D$MWAL
  corrlan <- stock_output$SOP_L$corrland
  corrdis <- stock_output$SOP_D$corrdis
  MWAA_NAA_L <- stock_output$LNAA
  MWAA_NAA_D <- stock_output$DNAA

  
  # --- Check if all are empty ---
  all_empty <- all(
    is.null(RW) || nrow(RW) == 0,
    is.null(LF) || nrow(LF) == 0,
    is.null(MWAL_NAL_L) || nrow(MWAL_NAL_L) == 0,
    is.null(MWAL_NAL_D) || nrow(MWAL_NAL_D) == 0,
    is.null(corrlan) || nrow(corrlan) == 0,
    is.null(corrdis) || nrow(corrdis) == 0,
    is.null(MWAA_NAA_L) || nrow(MWAA_NAA_L) == 0,
    is.null(MWAA_NAA_D) || nrow(MWAA_NAA_D) == 0
  )
  
  if (all_empty) {
    msg("⚠️ No raising outputs available for raising of :", iraise, " skipping generation of RCEF Tables.\n")
    next
  }
  
  all_records <- list()  # store results here
  
  for (catchCat in names(iRaisingLevel)) {
    iRaisingLevel_Cat <- iRaisingLevel[[catchCat]]
    
    # Check if raising is requested for this catch category
    if (!iRaisingLevel_Cat %in% c("WEIGHTS", "LENGTHS", "LWK", "ALK", "ALL")) {
      cat("No discard sampling - Not providing discard estimate.\n")
      next  # Skip to next iteration
    }
    
    iYQ_Cat <- if (!is.null(iYQ[[catchCat]])) iYQ[[catchCat]] else "YEAR"
    
    record_df <- make_catch_TBB(iWG_nice, istock, iAphiaID, imet, iyear, catchCategory = catchCat,  RW, SA_stock = SA_year, CL_stock = CL_stock ,iYQ = iYQ_Cat, iRaisingLevel= iRaisingLevel )
    
    # Store result
    all_records[[catchCat]] <- record_df
  }
  
  # Combine all datasets into one data frame
  all_records <- do.call(rbind, all_records)
  rownames(all_records) <- NULL
  
  # ---- CATCH ----

  CL_extra <- CL_All %>%
    filter(CLyear == iyear, CLarea %in% iareas, CLspecFAO == iFAO, CLmetier6 != imet)
  
  CL_summary <- CL_extra %>%
    group_by(CLarea, CLmetier6, CLquar) %>%
    summarise(CLoffWeight = sum(CLoffWeight), .groups = "drop")
  
  if (nrow(CL_summary) > 0) {
    CL_summary <- convert_metier_labels(CL_summary)
    CL_summary$Fleet_noAll <- gsub("_all$", "", CL_summary$Fleet)
    
    records_CL <- do.call(rbind, lapply(1:nrow(CL_summary), function(i) {
      make_catch_noTBB(
        iWG_nice = iWG_nice,
        total = CL_summary$CLoffWeight[i],
        istock = istock,
        ispecies = iAphiaID,
        imet = CL_summary$Fleet_noAll[i],
        ifleetValue = CL_summary$Fleet[i],
        iyear = iyear,
        iquarter = CL_summary$CLquar[i],
        iareaValue = CL_summary$CLarea[i]
      )
    }))
  } else {
    # Create an empty data frame with the same structure expected by make_catch_noTBB output
    records_CL <- all_records[0, ]
  }
  catch_output <- rbind(all_records, records_CL)
  
  if (is.null(catch_output) || nrow(catch_output) == 0) {
    catch_output <- empty_catch_df()
  }
  
  all_catch[[iraise]] <- catch_output
  
  # ---- LENGTH-BASED BIOLOGICAL VARIABLES ----
  
  # Domains are already prepared, one per quarter if quarterly raising
  idomainBiology_Lan <- catch_output$domainBiology[catch_output$metier6 == imet & catch_output$catchCategory == "Lan" & catch_output$domainBiology != ""]
  idomainBiology_Dis <- catch_output$domainBiology[catch_output$metier6 == imet & catch_output$catchCategory == "Dis" & catch_output$domainBiology != ""]
  
  MWAL_list <- list()
  NAL_list  <- list()
  MWAA_list <- list()
  NAA_list  <- list()
  
  for (catchCat in names(iRaisingLevel)) {
    
    iRaisingLevel_Cat <- iRaisingLevel[[catchCat]]
    iYQ_Cat <- if (!is.null(iYQ[[catchCat]])) iYQ[[catchCat]] else "YEAR"
    has_weight <- iRaisingLevel_Cat %in% c("ALL", "LWK", "AGE")
    LWK_source   <-  iLWK[[catchCat]]
    # Select domainBiology and df objects based on catchCat
    if (catchCat == "Lan") {
      domainBiology <- idomainBiology_Lan
      df_MWAL <- MWAL_NAL_L
      df_MWAA <- MWAA_NAA_L
    } else {
      domainBiology <- idomainBiology_Dis
      df_MWAL <- MWAL_NAL_D
      df_MWAA <- MWAA_NAA_D
    }
  
  # ---- Landings: Mean weight at length ----
    # ---- Mean weight at length ----
    MWAL <- make_distribution_tibble(
      iyear, iYQ = iYQ_Cat, iWG_nice = iWG_nice, istock = istock, iAphiaID = iAphiaID,
      catchCat = catchCat, domainBiology = domainBiology,
      distributionType = "LengthTotal", variableType = "WeightLive",
      SA_stock = SA_year, df = df_MWAL, df_raw = LF, has_weight = has_weight, LWK_source= LWK_source
    )
    MWAL_list[[catchCat]] <- MWAL
    
    # ---- Number at length ----
    NAL <- make_distribution_tibble(
      iyear, iYQ_Cat, iWG_nice = iWG_nice, istock = istock, iAphiaID = iAphiaID,
      catchCat = catchCat, domainBiology = domainBiology,
      distributionType = "LengthTotal", variableType = "Number",
      SA_stock = SA_year, df = df_MWAL, df_raw= LF, has_weight = has_weight, LWK_source= LWK_source
    )
    
    NAL_list[[catchCat]] <- NAL
    
    # ---- Mean weight at age ----
    MWAA <- make_distribution_tibble(
      iyear, iYQ_Cat, iWG_nice = iWG_nice, istock = istock, iAphiaID = iAphiaID,
      catchCat = catchCat, domainBiology = domainBiology,
      distributionType = "Age", variableType = "WeightLive",
      SA_stock = SA_year, df = df_MWAA, df_raw= LF, has_weight = has_weight, LWK_source= LWK_source
    )
    
    MWAA_list[[catchCat]] <- MWAA
    
    # ---- Number at age ----
    NAA <- make_distribution_tibble(
      iyear, iYQ_Cat, iWG_nice = iWG_nice, istock = istock, iAphiaID = iAphiaID,
      catchCat = catchCat, domainBiology = domainBiology,
      distributionType = "Age", variableType = "Number",
      SA_stock = SA_year, df = df_MWAA, df_raw= LF, has_weight = has_weight, LWK_source= LWK_source
    )
    
    NAA_list[[catchCat]] <- NAA
  }
  
  # Optionally bind all results into unified data frames
  MWAL_all <- do.call(rbind, MWAL_list)
  NAL_all  <- do.call(rbind, NAL_list)
  MWAA_all <- do.call(rbind, MWAA_list)
  NAA_all  <- do.call(rbind, NAA_list)

   # ---- Combine all ----
  distribution_stock <- bind_rows(
    do.call(rbind, MWAL_list),
    do.call(rbind, NAL_list),
    do.call(rbind, MWAA_list),
    do.call(rbind, NAA_list)
  )
  
  if (is.null(distribution_stock) || nrow(distribution_stock) == 0) {
    distribution_stock <- empty_distribution_df()
  }
  
  all_distribution[[iraise]] <- distribution_stock

    # ---- EFFORT ----
  CE_extra <- CE_All %>%
    filter(CEyear == iyear, CEarea %in% iareas)
  
  CE_summary <- CE_extra %>%
    group_by(CEarea, CEmetier6, CEquar) %>%
    summarise(CEoffkWDaySea = sum(CEoffkWDaySea), .groups = "drop")
  
  CE_summary <- convert_metier_labels(CE_summary)
  CE_summary$Fleet_noAll <- gsub("_all$", "", CE_summary$Fleet)
  
  if (is.null(CE_summary) || nrow(CE_summary) == 0) {
    records_effort <- empty_effort_df()
  }else {
    records_effort <- do.call(rbind, lapply(1:nrow(CE_summary), function(i) {
      make_effort(
        total = CE_summary$CEoffkWDaySea[i],
        iWG = iWG_nice,
        imet = CE_summary$Fleet_noAll[i],
        ifleetValue = CE_summary$Fleet[i],
        iyear = iyear,
        iquarter = CE_summary$CEquar[i],
        iareaValue = CE_summary$CEarea[i]
      )
    }))
  }

  all_effort[[iraise]] <- records_effort

}
# Master list to hold everything by stock
all_tables <- list()

mkdir("output")

for (iraise in names(all_catch)) {
  
  # Combine all three outputs for this stock into a sublist
  all_tables[[iraise]] <- list(
    catch = all_catch[[iraise]],
    distribution = all_distribution[[iraise]],
    effort = all_effort[[iraise]]
  )
  
  
  hni_file     <- paste0("output/HNI_", iraise, ".csv")
  catch_file   <- paste0("output/catch_", iraise, ".csv")
  distrib_file <- paste0("output/distribution_", iraise, ".csv")
  effort_file  <- paste0("output/HEN_", iraise, ".csv")
  
  all_catch[[iraise]]$total<-round(all_catch[[iraise]]$total,3)  
  all_catch[[iraise]]$variance <-round(as.numeric(all_catch[[iraise]]$variance))  
  
    # --- Save each table to CSV ---
  write.csv(all_catch[[iraise]], catch_file, row.names = FALSE)
  write.csv(all_distribution[[iraise]], distrib_file, row.names = FALSE)
  write.table(all_effort[[iraise]], effort_file, 
              row.names = FALSE, 
              col.names = FALSE, 
              sep = ",",quote = FALSE,na = "")
  
  write.table(all_catch[[iraise]], hni_file, sep = ",", row.names = FALSE, col.names = FALSE,quote = FALSE,na = "")
  write.table(all_distribution[[iraise]], hni_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE,quote = FALSE,na = "")
  
  
  msg("✅ Saved CSVs for: raising of : ", iraise, "\n")
}

# --- Save all_tables as an RData file ---
save(all_tables, file = "output/all_tables.RData")

msg("🎉 All raising CSV's saved successfully.\n")


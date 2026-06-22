library(stringr)
library(tidyverse)
library(rmarkdown)
library(knitr)
library(gridExtra)

# --- INSTELLINGEN PADEN ---

wd<-getwd()


 
output_dir <- file.path(wd, "output")

validation_dir <- file.path(wd, "Validation")
if(!dir.exists(validation_dir)) dir.create(validation_dir, recursive = TRUE)


# --- CONFIGURATIES ---
configs <- list(
  list(id="L_MWAL", cat="Lan", bio="LengthTotal", var="WeightLive", type="Mean",  old_suffix="_L_MWAL.csv",     unit_mult=10, val_mult=1000, lab="Mean Weight (g)"),
  list(id="D_MWAL", cat="Dis", bio="LengthTotal", var="WeightLive", type="Mean",  old_suffix="_D_MWAL.csv",     unit_mult=10, val_mult=1000, lab="Mean Weight (g)"),
  list(id="L_MWAA", cat="Lan", bio="Age",         var="WeightLive", type="Mean",  old_suffix="_L_MWAA.csv",     unit_mult=1,  val_mult=1000, lab="Mean Weight (g)"),
  list(id="D_MWAA", cat="Dis", bio="Age",         var="WeightLive", type="Mean",  old_suffix="_D_MWAA.csv",     unit_mult=1,  val_mult=1000, lab="Mean Weight (g)"),
  list(id="L_NAL",  cat="Lan", bio="LengthTotal", var="Number",     type="Total", old_suffix="_SopCorr_L_NAL.csv", unit_mult=10, val_mult=1,    lab="Numbers"),
  list(id="D_NAL",  cat="Dis", bio="LengthTotal", var="Number",     type="Total", old_suffix="_SopCorr_D_NAL.csv", unit_mult=10, val_mult=1,    lab="Numbers"),
  list(id="L_NAA",  cat="Lan", bio="Age",         var="Number",     type="Total", old_suffix="_SopCorr_L_NAA.csv", unit_mult=1,  val_mult=1,    lab="Numbers"),
  list(id="D_NAA",  cat="Dis", bio="Age",         var="Number",     type="Total", old_suffix="_SopCorr_D_NAA.csv", unit_mult=1,  val_mult=1,    lab="Numbers")
)

read_old <- function(full_path) {
  read.csv2(full_path, row.names = 1)
}



# --- HOOFDLOOP ---
# select stocks with distribution
load(file = "model/raised_outputs_AllStocks.Rdata")
has_lf <- sapply(all_outputs, function(x) !is.null(x$LF))
iraise_values <- names(all_outputs)[has_lf]


iraise<-"bll.27.3a47de_TBB_DEF_70-99_0_0_2024_YEAR"

for (iraise in (iraise_values)) {
  
  distrib_file <- file.path(output_dir, paste0("distribution_", iraise, ".csv"))
  if(!file.exists(distrib_file)) next
  
  dist <- read.csv(distrib_file, stringsAsFactors = FALSE)
  istock <- sub("_.*", "", iraise)
  imesh  <- str_split_i(iraise, "_", 4)
  iyear  <- str_split_i(iraise, "_", 7)
  
  target_folder <- paste0("../../../ndgp.eu.newfdi/STOCKDATA/", istock, "/", iyear, "/", imesh, "/")
  
  
  
  split_domains <- strsplit(dist$domainBiology, "_")
  years             <- sapply(split_domains, `[`, 1)
  extracted_seasons <- sapply(split_domains, `[`, 2)
  dist$seasonValue <- ifelse(extracted_seasons == "All", 
                             years, 
                             gsub("Q", "", extracted_seasons))
  dist$seasonValue <- as.numeric(dist$seasonValue)
  
  unique_extracted <- unique(extracted_seasons)
  is_quarterly     <- any(unique_extracted != "All")
  
  seasons <- if (is_quarterly) {
    quarters <- unique_extracted[unique_extracted != "All"]
    sort(as.numeric(gsub("Q", "", quarters)))
  } else {
    unique(years)
  }
  
  
  
  
  
  stock_results <- list() 
  
  for (sea in seasons) {
    stock_results[[as.character(sea)]] <- list()
    
    for (conf in configs) {
      try({
        if(is_quarterly) {
          suffix_q <- str_replace(conf$old_suffix, "\\.csv$", paste0("_Q", sea, ".csv"))
          old_file <- paste0(istock, suffix_q)
        } else {
          old_file <- paste0(istock, conf$old_suffix)
        }
        
        full_path <- paste0(target_folder, old_file)
        
        # Fallback voor bestanden zonder SopCorr
        if(!file.exists(full_path) && str_detect(old_file, "_SopCorr_")) {
          full_path <- str_replace(full_path, "_SopCorr_", "_")
        }
        
        if(file.exists(full_path)) {
          old_data <- read_old(full_path)
          
          if("length" %in% colnames(old_data)) {
            old_clean <- old_data %>% dplyr::select(unit = length, value_old = 2)
          } else {
            old_clean <- old_data %>% rownames_to_column("unit") %>% dplyr::select(unit, value_old = 2)
          }
          
          old_clean <- old_clean %>%
            mutate(unit = as.numeric(as.character(unit)) * conf$unit_mult,
                   value_old = as.numeric(as.character(value_old)) * conf$val_mult)
          
          new_clean <- dist %>%
            filter(catchCategory == conf$cat, distributionType == conf$bio, 
                   variableType  == conf$var, valueType == conf$type)
          
          if(is_quarterly) new_clean <- new_clean %>% filter(seasonValue == sea)
          
          new_clean <- new_clean %>%
            dplyr::select(unit = distributionClass, value_new = value) %>%
            mutate(unit = as.numeric(as.character(unit)),
                   value_new = as.numeric(as.character(value_new)))
          
          # HIER ZAT DE FOUT: filter(!is.na(value_new) | !is.na(value_old))
          comparison <- full_join(new_clean, old_clean, by = "unit") %>%
            mutate(diff_pct = round(((value_new - value_old) / value_old) * 100, 3)) %>%
            filter(!is.na(value_new) | !is.na(value_old)) %>%
            arrange(unit)
          
          if(nrow(comparison) > 0) {
            stock_results[[as.character(sea)]][[conf$id]] <- list(data = comparison, label = conf$lab, bio = conf$bio)
          }
        }
      }, silent = TRUE)
    }
  }
  
  stock_results <- Filter(function(x) length(x) > 0, stock_results)
  
  if(length(stock_results) > 0) {
    report_full_path <- file.path(validation_dir, paste0("Validation_", iraise, ".html"))
    rmd_file <- tempfile(fileext = ".Rmd")
    
    cat("---\n", file = rmd_file)
    cat(paste0("title: 'Validatie: ", istock, "'\n"), file = rmd_file, append = TRUE)
    cat(paste0("subtitle: 'Run: ", iraise, "'\n"), file = rmd_file, append = TRUE)
    cat(paste0("date: '", Sys.time(), "'\n"), file = rmd_file, append = TRUE)
    cat("output:\n  html_document:\n    toc: true\n    toc_float: true\n    theme: cosmo\n---\n\n", file = rmd_file, append = TRUE)
    
    
    
    
    cat("```{r setup, include=FALSE}\nlibrary(ggplot2)\nlibrary(tidyr)\nlibrary(dplyr)\nknitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)\n```\n\n", file = rmd_file, append = TRUE)
    
    for(sea in names(stock_results)) {
      cat(paste0("# ", ifelse(sea == "Year", "Jaarverdeling", paste("Kwartaal", sea)), " {.tabset}\n\n"), file = rmd_file, append = TRUE)
      
      for(id in names(stock_results[[sea]])) {
        res <- stock_results[[sea]][[id]]
        cat(paste0("## ", id, "\n\n"), file = rmd_file, append = TRUE)
        cat("```{r}\n", file = rmd_file, append = TRUE)
        cat(paste0("df_temp <- ", paste(capture.output(dput(res$data)), collapse=""), "\n"), file = rmd_file, append = TRUE)
        cat("df_long <- df_temp %>% pivot_longer(cols = c(value_new, value_old), names_to = 'Bron', values_to = 'Waarde')\n", file = rmd_file, append = TRUE)
        cat(paste0("ggplot(df_long, aes(x = unit, y = Waarde, color = Bron)) + 
              geom_line(size = 1) + geom_point() + 
              scale_color_manual(values = c(value_new = 'blue', value_old = 'red')) + 
              theme_minimal() + labs(y = '", res$label, "', x = '", ifelse(res$bio == "Age", "Leeftijd", "Lengte (mm)"), "')\n"), file = rmd_file, append = TRUE)
        cat(paste0("knitr::kable(df_temp, caption = 'Tabel: ", id, " (", sea, ")')\n"), file = rmd_file, append = TRUE)
        cat("```\n\n", file = rmd_file, append = TRUE)
      }
      cat("\n***\n\n", file = rmd_file, append = TRUE)
    }
    
    rmarkdown::render(input = rmd_file, 
                      output_file = report_full_path, 
                      envir = new.env(),
                      quiet = TRUE)
    
    message(paste("✅ Rapport succesvol gegenereerd:", report_full_path))
  }
}

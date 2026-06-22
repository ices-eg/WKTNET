
library(dplyr)
library(stringr)


output_dir <- "output"

files_to_upload <- list.files(
  path = output_dir, 
  pattern = "^(HNI|HEN).*\\.csv$", 
  full.names = TRUE
)

if (length(files_to_upload) == 0) {
  message("No HNI of HEN found in this folder: ", output_dir)
} else {
  

    for (file_path in files_to_upload) {
    
    file_name <- basename(file_path)
    
    # Hierarchy HNI or HEN
    hierarchy_type <- substr(file_name, 1, 3)
    
    message("Uploading: ", file_name, " (Hierarchy: ", hierarchy_type, ")...")
    
    result <- rdbes_upload_data(file_path, hierarchy = hierarchy_type)
  
  }
  
  message("uploaded all HNI and HEN files!")
}
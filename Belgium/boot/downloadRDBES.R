

library(icesRDBES)

my_payload <- list(
  dataType          = "CL",
  format            = "CsvFilePerTable",
  includeDisclaimer = TRUE,
  hierarchies       = list("HCL"),
  clFilters         = list(
    clVesselFlagCountry = list("BE"), # Mandatory for Permissions
   # clYear              = list("2024")  # Available Optional Filter
    clYear              = list("2024","2025")  # Available Optional Filter
    # clArea              = list(),      # Available Optional Filter
    # clSpeciesCode       = list()       # Available Optional Filter
  )
)
CL_zip_pad<-rdbes_download_data(payload = my_payload)


my_payload <- list(
  dataType          = "CE",
  format            = "CsvFilePerTable",
  includeDisclaimer = TRUE,
  hierarchies       = list("HCE"),
  ceFilters         = list(
    ceVesselFlagCountry = list("BE"), # Mandatory for Permissions
#    ceYear              = list("2024")  # Available Optional Filter
   ceYear              = list("2024","2025")  # Available Optional Filter
    
        # ceArea              = list()       # Available Optional Filter
  )
)

CE_zip_pad<-rdbes_download_data(payload = my_payload)


my_payload <- list(
  dataType          = "CS",
  format            = "CsvFilePerTable",
  includeDisclaimer = TRUE,
  hierarchies       = list("H2"),
  csFilters         = list(
    sdCountry         = list("BE"),  # Mandatory for Permissions
  #  deYear            = list("2024") # Available Optional Filter
   deYear            = list("2024","2025") # Available Optional Filter
    # deSamplingScheme  = list(),       # Available Optional Filter
    # deStratumName     = list(),     # Available Optional Filter
    # saSpeciesCode     = list(),       # Available Optional Filter
    # foArea            = list(“27.2.a”, “27.8.a”),       # Available Optional Filter
    # leArea            = list(“27.2.a”, “27.8.a”)        # Available Optional Filter
  )
)

CS_zip_pad<-rdbes_download_data(payload = my_payload)

# CS_zip_pad <- "export_16e21dad-0dc5-4386-8cca-6219fbf4bf81.zip"
# CE_zip_pad <- "export_389b7a7e-78d4-497f-a514-3766fcc225da.zip"
# CL_zip_pad <- "export_2dea9ed5-e03b-475b-8e22-ca5b4f90ed5d.zip"
# CS_zip_pad
# CE_zip_pad
# CL_zip_pad

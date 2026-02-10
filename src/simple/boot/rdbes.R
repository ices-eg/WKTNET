library(AzureAuth)
library(httr)

# we will package this into a function,
if (FALSE) {
  download.rdbes.data(year, country, datatype, exportformat, cshierarchy, filename)
}

# Authenticate and get token
az <- get_azure_token(
  resource = "api://18ab5ebb-1794-4e83-83f1-8fbd3dd5b152/rdbes.api.access",
  tenant = "e0b220ce-5735-4468-91df-05cae5ff1fdc",
  app = "b6347a7e-5f73-463a-81b1-3781d163de19",
  version = 2
)
AzureAuth::decode_jwt(az)


# Extract the access token
access_token <- az$credentials$access_token

# Specify the base endpoint to export data from
# https://sboxrdbes.ices.dk/api/taf/export/data
base_url <- "https://sboxrdbes.ices.dk/api/taf/export/data"

# Specify year
# This will be passed as a query parameter to the API
year <- "2022"
year_qry <- paste0("?year=", year)

# Specify country
# This will be passed as a query parameter to the API
country <- "ZW"
country_qry <- paste0("&country=", country)

# Specify the data type you want to download
# This will be passed as a query parameter to the API
datatype <- "CS"
datatype_qry <- paste0("&datatype=", datatype)

# Specify the export format.  UploadFormat or TableWithIdsFormat
# This will be passed as a query parameter to the API
exportformat <- "uploadformat"
exportformat_qry <- paste0("&exportformat=", exportformat)


# Specify optional parameter for sub type of CS data
# This will be passed as a query parameter to the API
cshierarchy <- "H2"
cshierarchy_qry <- paste0("&cshierarchy=", cshierarchy)

# Construct Mandatory query string parameters
mandatoryparams <- paste0(year_qry, country_qry, datatype_qry, exportformat_qry)

# Construct Optional query string parameters
optionalparams <- paste0(cshierarchy_qry)

url <- paste0(base_url, mandatoryparams, optionalparams)

# Make the API call
response <- GET(
  url = url,
  add_headers(Authorization = paste("Bearer", access_token))
)


# Save the ZIP file if successful
if (status_code(response) == 200) {
  downloadfilename <- paste0(datatype, "_", country, "_", year, "_download.zip")
  writeBin(content(response, "raw"), downloadfilename)
  cat("Downloaded:", downloadfilename, "\n")
  cat("  Status code      :", status_code(response), "\n")
  cat("  Http status      :", http_status(response)$reason, "\n")

} else {
  cat("Failed to download:\n")
  cat("  Status code      :", status_code(response), "\n")
  cat("  Http status      :", http_status(response)$reason, "\n")
  cat("  Detailed message :", content(response, "text"), "\n")
}

####################################

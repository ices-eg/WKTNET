

library(jsonlite)

doi <- "https://doi.org/10.17895/ices.pub.30939734"
id <- sub("https://doi.org/10.17895/ices.pub.", "", doi)

datacall <- read_json(paste0("https://api.figshare.com/v2/articles/", id))

annex1_info <-
  datacall$files[[
    grep("Annex_1", sapply(datacall$files, "[[", "name"))
  ]]
annex1_info

TAF::download(annex1_info$download_url, destfile = annex1_info$name)

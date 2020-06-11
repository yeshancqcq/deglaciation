x = paste("~/Downloads/lipd/lipd_data/",filenames[19],sep="")
D = readLipd(path = toString(x))

library(lipdR)

filenames <- list.files(path = "~/Downloads/lipd/lipd_data", pattern=NULL, all.files=FALSE,
                        full.names=FALSE)

metadata = data.frame(id = 1:length(filenames),
                      lat = NA,
                      lon = NA,
                      sitename = NA,
                      filename = NA,
                      elevation = NA,
                      country = NA,
                      gcmd_location = NA,
                      author = NA,
                      pub_year = NA,
                      title = NA,
                      journal = NA,
                      archive_type = NA,
                      min_age = NA,
                      max_age = NA,
                      proxy = NA,
                      num_record = NA,
                      num_control = NA,
                      url = NA)

for(i in 1:length(filenames)){
  x = paste("~/Downloads/lipd/lipd_data/",filenames[i],sep="")
  D = readLipd(path = toString(x))
  metadata$lat[i] = D[["geo"]][["latitude"]]
  metadata$lon[i] = D[["geo"]][["longitude"]]
  metadata$elevation[i] = D[["geo"]][["elevation"]]
  metadata$sitename[i] = D[["geo"]][["siteName"]]
  if(length(D[["geo"]][["countryOcean"]])>0){
    metadata$country[i] = D[["geo"]][["countryOcean"]]
  }
  if(length(D[["geo"]][["gcmdLocation"]])>0){
    metadata$gcmd_location[i] = D[["geo"]][["gcmdLocation"]]
  }
  metadata$filename[i] = toString(filenames[i])
  if(length(D[["pub"]][[1]][["author"]][[1]][["name"]])>0){
    metadata$author[i] = D[["pub"]][[1]][["author"]][[1]][["name"]]
  }
  if(length(D[["pub"]][[1]][["year"]])>0){
    metadata$pub_year[i] = D[["pub"]][[1]][["year"]]
  }
  if(length(D[["pub"]][[1]][["title"]])>0){
    metadata$title[i] = D[["pub"]][[1]][["title"]]
  }
  if(length(D[["pub"]][[1]][["journal"]])>0){
    metadata$journal[i] = D[["pub"]][[1]][["journal"]]
  }
  if(length(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["proxy"]])>0){
    metadata$proxy[i] = D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["proxy"]]
  }else if(length(D[["paleoData"]][[1]][["measurementTable"]])>=2){
    if(!(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["proxy"]]))){
      metadata$proxy[i] = D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["proxy"]]
    }
  }else{
    metadata$proxy[i] = NA
  }
  if(length(D[["archiveType"]])>0){
    metadata$archive_type[i] = D[["archiveType"]]
  }
  if(!(is.null(D[["originalDataUrl"]]))){
    metadata$url[i] = D[["originalDataUrl"]]
  }
  if(length(D[["maxYear"]])>0){
    metadata$max_age[i] = D[["maxYear"]]
  }
  if(length(D[["minYear"]])>0){
    metadata$min_age[i] = D[["minYear"]]
  }
  if(!(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]]))){
    metadata$num_record[i] = length(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])
  }else if(length(D[["paleoData"]][[1]][["measurementTable"]])>=2){
    if(!(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]]))){
      metadata$num_record[i] = length(D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])
    }
  }else{
    metadata$num_record[i] = 0
  }
  if(!(is.null(D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]]))){
    metadata$num_control[i] = length(D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]])
  }
  cat("finishing", i, "of 455 or so","\n")
}
write.csv(metadata, file="~/Documents/github/paleo_data_spatial/data/lipd_metadata_1.csv",fileEncoding="UTF-8")


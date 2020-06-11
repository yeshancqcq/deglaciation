setwd("~/Documents/github/paleo_data_spatial")

library(lipdR)

setwd("~/Downloads/lipd/lipd_data")

filenames <- list.files(path = "~/Downloads/lipd/lipd_data", pattern=NULL, all.files=FALSE,
                        full.names=FALSE)

#teststring = paste("~/Downloads/lipd/lipd_data/",filenames[2],sep="")

for(i in 1:length(filenames)){
  x = paste("~/Downloads/lipd/lipd_data/",filenames[i],sep="")
  D = readLipd(path = toString(x))
  #level 1
  if(length(D[["paleoData"]][[1]][["measurementTable"]])==1){
    if(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]]) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]]))){
      paleodata = data.frame(age = D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])
    } else if (
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])) &&
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]]))
    ){
      paleodata = data.frame(depth = D[["paleoData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]],
                             age = D[["paleoData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[1]][["temperature"]][["values"]])
    }  else {
      paleodata = data.frame(depth = NA)
    }
  } else if(length(D[["paleoData"]][[1]][["measurementTable"]])==2){
    if(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["depth"]][["values"]]) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])) &&
       !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]]))){
      paleodata = data.frame(age = D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])
    } else if (
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])) &&
      !(is.null(D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]]))
    ){
      paleodata = data.frame(depth = D[["paleoData"]][[1]][["measurementTable"]][[2]][["depth"]][["values"]],
                             age = D[["paleoData"]][[1]][["measurementTable"]][[2]][["age"]][["values"]],
                             temperature = D[["paleoData"]][[1]][["measurementTable"]][[2]][["temperature"]][["values"]])
    }  else {
      paleodata = data.frame(depth = NA)
    }
  } else {
    paleodata = data.frame(depth = NA)
  }
  
  
  save_dir = paste("~/Downloads/lipd/lipd2csv/",filenames[i],".csv",sep="")
  write.csv(paleodata, file=toString(save_dir),fileEncoding="UTF-8")
  cat("finishing", i, "of 456","\n")
}

D = readLipd(path = toString(x))

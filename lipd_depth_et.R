setwd("~/Documents/github/paleo_data_spatial")

library(lipdR)

setwd("~/Downloads/lipd/lipd_data")

filenames <- list.files(path = "~/Downloads/lipd/lipd_data", pattern=NULL, all.files=FALSE,
                        full.names=FALSE)

#teststring = paste("~/Downloads/lipd/lipd_data/",filenames[2],sep="")

for(i in 1:length(filenames)){
  x = paste("~/Downloads/lipd/lipd_data/",filenames[i],sep="")
  D = readLipd(path = toString(x))
  len = length(D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]][["values"]])
  if(len>0){
    output <- data.frame(id = 1:len, 
                         age_type = NA, 
                         depth = NA,
                         depth_unit = " ",
                         age = NA,
                         age_err = NA, 
                         age_unit = " ",
                         age_high = NA,
                         age_high_unit = " ",
                         age_low = NA,
                         age_low_unit = " ",
                         reservoir_age = NA, 
                         reservoir_age_unit = " ",
                         lab_id = NA,
                         sd = NA,
                         sd_unit = " ",
                         material = NA,
                         thickness = NA,
                         thickness_unit = " ",
                         notes = NA)
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]])>0){
      output$depth = D[["chronData"]][[1]][["measurementTable"]][[1]][["depth"]][["values"]]
      output$depth_unit = toString(D[["chronData"]][[1]][["measurementTable"]][[1]][["depth"]][["units"]])
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["depthMid"]][["values"]])>0){
      output$depth = D[["chronData"]][[1]][["measurementTable"]][[1]][["depthMid"]][["values"]]
      output$depth_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["depthMid"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]][["values"]])>0){
      output$age_type = D[["chronData"]][[1]][["measurementTable"]][[1]][["age_type"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["labID"]][["values"]])>0){
      output$lab_id = D[["chronData"]][[1]][["measurementTable"]][[1]][["labID"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]])>0){
      output$age = D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["values"]]
      if(length( D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["units"]])>0){
        output$age_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["age"]][["units"]]
      }
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["ageHi"]][["values"]])>0){
      output$age_high = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageHi"]][["values"]]
      output$age_high_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageHi"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["ageLow"]][["values"]])>0){
      output$age_low = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageLow"]][["values"]]
      output$age_low_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageLow"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["thickness"]][["values"]])>0){
      output$thickness = D[["chronData"]][[1]][["measurementTable"]][[1]][["thickness"]][["values"]]
      output$thickness_unit[1] = D[["chronData"]][[1]][["measurementTable"]][[1]][["thickness"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["1SD"]][["values"]])>0){
      output$sd = D[["chronData"]][[1]][["measurementTable"]][[1]][["1SD"]][["values"]]
      output$sd_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["1SD"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["SD"]][["values"]])>0){
      output$sd = D[["chronData"]][[1]][["measurementTable"]][[1]][["SD"]][["values"]]
      output$sd_unit = D[["chronData"]][[1]][["measurementTable"]][[1]][["SD"]][["units"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["material"]][["values"]])>0){
      output$material = D[["chronData"]][[1]][["measurementTable"]][[1]][["material"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["measurmentMaterial"]][["values"]])>0){
      output$material = D[["chronData"]][[1]][["measurementTable"]][[1]][["measurmentMaterial"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["notes"]][["values"]])>0){
      output$notes = D[["chronData"]][[1]][["measurementTable"]][[1]][["notes"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["ageError"]][["values"]])>0){
      output$age_err = D[["chronData"]][[1]][["measurementTable"]][[1]][["ageError"]][["values"]]
    }
    if(length(D[["chronData"]][[1]][["measurementTable"]][[1]][["reservoirAge"]][["values"]])>0){
      output$reservoir_age = D[["chronData"]][[1]][["measurementTable"]][[1]][["reservoirAge"]][["values"]]
      output$reseervoir_age_unit = toString(D[["chronData"]][[1]][["measurementTable"]][[1]][["reservoirAge"]][["units"]])
    }
    
  } else {
    output <- data.frame(id = 1, 
                         age_type = NA, 
                         depth = NA,
                         depth_unit = NA,
                         age = NA,
                         age_err = NA, 
                         age_unit = NA,
                         material = NA)
  }
  
  save_dir = paste("~/Downloads/lipd/lipd_chron/",filenames[i],"_chronData.csv",sep="")
  write.csv(output, file=toString(save_dir),fileEncoding="UTF-8")
  cat("finishing", i, "of 456","\n")
}

D = readLipd(path = toString(x))

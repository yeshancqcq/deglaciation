library(readr)
library(dplyr)
metadata <- read_csv("data/metadata.csv")
helper <- read_csv("data/helper.csv")
Control <- read_csv("anomaly_interpolated/117_723A.Godad.2011.lpd.csv")
ctr <- t(Control)

# combining============

helper$name =""

files <- list.files(path="anomaly_interpolated", 
                    pattern="*.csv", full.names=TRUE, recursive=FALSE)

lapply(files, function(x){
  temp_data <- read_csv("data/temperature_metadata.csv", 
                           col_types = cols(X1 = col_skip()))
  anomaly_data <- read_csv("data/anomaly_metadata.csv", 
                               col_types = cols(X1 = col_skip()))
  data <- read.csv(x, skip=0)
  filename <- basename(x)
  cat("processing", filename,"\n")
  anomaly_vector = data$Anomaly
  anomaly_vector <- append(anomaly_vector,filename,after=length(anomaly_vector))
  anomaly_data <- rbind(anomaly_data,anomaly_vector)
  temp_vector = data$Temperature
  temp_vector <- append(temp_vector,filename,after=length(temp_vector))
  temp_data <- rbind(temp_data,temp_vector)
  write.csv(temp_data, file="data/temperature_metadata.csv")
  write.csv(anomaly_data, file="data/anomaly_metadata.csv")
  
})

#joining========================

library(readr)
t_helper <- read_csv("data/temperature_helper.csv")
a_helper <- read_csv("data/anomaly_helper.csv")
View(t_helper)

metadata$name <- paste0(metadata$filename,".csv")
t_join <- full_join(metadata, t_helper, by = c("name","name"), copy = FALSE, suffix = c(".x", ".y"))



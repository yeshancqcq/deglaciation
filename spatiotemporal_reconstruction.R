library(readr)
grid_anomaly <- read_csv("data/grid_anomaly_helper.csv")
anomaly_data <- read_csv("data/anomaly_all_compile.csv")
grid_temperature <- read_csv("data/grid_temperature_helper.csv")
temperature_data <- read_csv("data/temperature_all_compile.csv")

#===anomaly====

for(i in 1:nrow(anomaly_data)){
  for(j in 1:nrow(grid_anomaly)){
    if(anomaly_data$lat[i] < grid_anomaly$y_max[j] &&
       anomaly_data$lat[i] >= grid_anomaly$y_min[j] &&
       anomaly_data$lon[i] < grid_anomaly$x_max[j] &&
       anomaly_data$lon[i] >= grid_anomaly$x_min[j]){
      grid_anomaly$num_record[j] <- grid_anomaly$num_record[j] + 1
      for(x in 29:ncol(anomaly_data)){
        y <- x - 19
        if(is.na(anomaly_data[i,x])){
          grid_anomaly[j,y] <- grid_anomaly[j,y] + 0
        } else {
          grid_anomaly[j,y] <- grid_anomaly[j,y] + as.numeric(anomaly_data[i,x])
        }
      }
    }
  }
  cat("finishing", i, "\n")
}
#write.csv(grid_anomaly,file="data/test.csv")

# === anomaly weighting ====
for(i in 1:nrow(grid_anomaly)){
  if(grid_anomaly$num_record[i] > 0){
    for(j in 10:ncol(grid_anomaly)){
      grid_anomaly[i,j] <- (grid_anomaly[i,j]/grid_anomaly$num_record[i]) * grid_anomaly$a_weight[i]
    }
  }
  cat("finishing", i, "\n")
}

write.csv(grid_anomaly,file="data/grid_anomaly.csv")

# ===== temperature====
for(i in 1:nrow(temperature_data)){
  for(j in 1:nrow(grid_temperature)){
    if(temperature_data$lat[i] < grid_temperature$y_max[j] &&
       temperature_data$lat[i] >= grid_temperature$y_min[j] &&
       temperature_data$lon[i] < grid_temperature$x_max[j] &&
       temperature_data$lon[i] >= grid_temperature$x_min[j]){
      grid_temperature$num_record[j] <- grid_temperature$num_record[j] + 1
      for(x in 29:ncol(temperature_data)){
        y <- x - 19
        if(is.na(temperature_data[i,x])){
          grid_temperature[j,y] <- grid_temperature[j,y] + 0
        } else {
          grid_temperature[j,y] <- grid_temperature[j,y] + as.numeric(temperature_data[i,x])
        }
      }
    }
  }
  cat("finishing", i, "\n")
}

#=====temperature weighting======
for(i in 1:nrow(grid_temperature)){
  if(grid_temperature$num_record[i] > 0){
    for(j in 10:ncol(grid_temperature)){
      grid_temperature[i,j] <- (grid_temperature[i,j]/grid_temperature$num_record[i]) * grid_temperature$a_weight[i]
    }
  }
  cat("finishing", i, "\n")
}
write.csv(grid_temperature,file="data/grid_temperature.csv")

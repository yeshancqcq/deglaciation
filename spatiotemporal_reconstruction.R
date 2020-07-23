library(readr)
grid_anomaly <- read_csv("data/grid_anomaly_helper.csv")
anomaly_data <- read_csv("data/anomaly_all_compile.csv")
#grid_temperature <- read_csv("data/grid_temperature_helper.csv")
#temperature_data <- read_csv("data/temperature_all_compile.csv")

#===anomaly====
for(x in 29:ncol(anomaly_data)){
  for(j in 1:nrow(grid_anomaly)){
    for(i in 1:nrow(anomaly_data)){
      if(anomaly_data$lat[i] < grid_anomaly$y_max[j] &&
         anomaly_data$lat[i] >= grid_anomaly$y_min[j] &&
         anomaly_data$lon[i] < grid_anomaly$x_max[j] &&
         anomaly_data$lon[i] >= grid_anomaly$x_min[j]){
        y <- x - 19
        if(is.na(anomaly_data[i,x])){
          grid_anomaly[j,y] <- grid_anomaly[j,y] + 0
        } else {
          grid_anomaly$num_record[j] <- grid_anomaly$num_record[j] + 1
          grid_anomaly[j,y] <- grid_anomaly[j,y] + as.numeric(anomaly_data[i,x])
        }
      }
    }
    if(grid_anomaly$num_record[j] > 0){
      grid_anomaly[j,y] <- (grid_anomaly[j,y] / grid_anomaly$num_record[j]) * grid_anomaly$a_weight[i]
    }
  }
  grid_anomaly$num_record <- 0
  cat("finishing", x-28, "\n")
}

#write.csv(grid_anomaly,file="data/test.csv")

write.csv(grid_anomaly,file="data/grid_anomaly_2.csv")



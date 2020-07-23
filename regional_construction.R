library(readr)
library(ggplot2)
library(stats)

model_anomaly <- read_csv("data/Feng_model_anomaly.csv")
model_anomaly <- model_anomaly[-1]
model_data <- model_anomaly

proxy_data <- read_csv("data/grid_anomaly.csv")
#proxy_data <- proxy_data[-1]

proxy_data2 <- proxy_data[,12:230]
proxy_data2$sum <- rowSums (proxy_data2, na.rm = TRUE, dims = 1)
proxy_data$sum <- proxy_data2$sum

# O79 1080
# W72 1656

proxy_region <- data.frame(
  (matrix(vector(), 220, 32, dimnames=list(
    c(), 
    c("time",
      "ne_labrador",
      "se_usa",
      "midwest",
      "rocky_mt",
      "w_usa",
      "c_canada",
      "alaska_yukon",
      "greenland",
      "mediterranean",
      "scandinavia",
      "uk",
      "c_north_europe",
      "iceland",
      "siberia",
      "e_china_japan",
      "tibet",
      "se_asia",
      "australia",
      "new_zealand",
      "n_indian_ocean",
      "e_africa",
      "s_africa",
      "w_africa",
      "c_atlantic",
      "s_atlantic",
      "e_pacific",
      "chile",
      "antarctica",
      "russian_far_east",
      "c_canada",
      "carribean"
      )
    ))
   )
)

model_region <- data.frame(
  (matrix(vector(), 220, 32, dimnames=list(
    c(), 
    c("time",
      "ne_labrador",
      "se_usa",
      "midwest",
      "rocky_mt",
      "w_usa",
      "c_canada",
      "alaska_yukon",
      "greenland",
      "mediterranean",
      "scandinavia",
      "uk",
      "c_north_europe",
      "iceland",
      "siberia",
      "e_china_japan",
      "tibet",
      "se_asia",
      "australia",
      "new_zealand",
      "n_indian_ocean",
      "e_africa",
      "s_africa",
      "w_africa",
      "c_atlantic",
      "s_atlantic",
      "e_pacific",
      "chile",
      "antarctica",
      "russian_far_east",
      "c_canada",
      "carribean"
    )
  ))
  )
)

#time
model_region$time = seq(100,22000,100)
proxy_region$time = seq(100,22000,100)

# grids
alaska_yukon  = 24
antarctica = 13
australia  =  3
c_atlantic  =  6
c_canada  = 15
c_north_europe =  19
carribean   = 3
chile  =  7
e_africa  = 11
e_china_japan =  13
e_pacific  = 10
greenland =  14
iceland  =  7
mediterranean  = 21
midwest  = 13
n_indian_ocean =   8
ne_labrador=   22
new_zealand  =  8
rocky_mt   = 5
russian_far_east =   8
s_africa  =  7
s_atlantic  =  8
scandinavia=   21
se_asia =  22
se_brazil =   2
se_usa  =  9
siberia =  14
tibet  =  8
uk  =  8
w_africa  =  5
w_usa  =  6



#new zealand ==============================
x = new_zealand
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) &
    model_data$regional[i] == "new_zealand"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  #change here
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", row, "; ")
}

for(i in 1:220){
  model_region$new_zealand[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) &
     proxy_data$regional[i] == "new_zealand"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+11]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", row, "; ")
}

for(i in 1:220){
  #change here
  proxy_region$new_zealand[i]<-temp[i,x+2]
}

#alaska_yukon =====================
x=alaska_yukon
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="alaska_yukon"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", row, "; ")
}

for(i in 1:220){
  model_region$alaska_yukon[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="alaska_yukon"){
    for(j in 1:220){
      temp[j,k]<-proxy_data[i,j+11]
    }
    k <- 1+k
    cat("finishing ", proxy_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", row, "; ")
}

for(i in 1:220){
  proxy_region$alaska_yukon[i]<-temp[i,x+2]
}

# greenland =====================
x=greenland
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="greenland"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+5]
    }
    k <- 1+k
    cat("finishing ", model_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", row, "; ")
}

for(i in 1:220){
  model_region$greenland[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="greenland"){
    for(j in 1:220){
      temp[j,k]<-proxy_data[i,j+11]
    }
    k <- 1+k
    cat("finishing ", proxy_data$PageName[i] )
  }
}
#average
for(row in 1:nrow(temp)){
  ct <- 0
  sum <- 0
  for(col in 2:(x+1)){
    sum <- sum + temp[row,col]
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", row, "; ")
}

for(i in 1:220){
  proxy_region$greenland[i]<-temp[i,x+2]
}



write.csv(proxy_region,"data/proxy_region.csv")
write.csv(model_region,"data/model_region.csv")

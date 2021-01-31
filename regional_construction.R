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
      temp[j,k]<-model_data[i,j+7]
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
      temp[j,k]<-model_data[i,j+7]
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
      temp[j,k]<-model_data[i,j+7]
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

# ne_labrador =====================
x=ne_labrador
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="ne_labrador"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$ne_labrador[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="ne_labrador"){
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
  proxy_region$ne_labrador[i]<-temp[i,x+2]
}

# midwest =====================
x=midwest
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="midwest"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$midwest[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="midwest"){
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
  proxy_region$midwest[i]<-temp[i,x+2]
}
# c_canada =====================
x=c_canada
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="c_canada"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$c_canada[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="c_canada"){
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
  proxy_region$c_canada[i]<-temp[i,x+2]
}
# antarctica =====================
x=antarctica
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="antarctica"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$antarctica[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="antarctica"){
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
  proxy_region$antarctica[i]<-temp[i,x+2]
}
# se_asia =====================
x=se_asia
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="se_asia"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$se_asia[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="se_asia"){
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
  proxy_region$se_asia[i]<-temp[i,x+2]
}
# russian_far_east =====================
x=russian_far_east
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="russian_far_east"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$russian_far_east[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="russian_far_east"){
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
  proxy_region$russian_far_east[i]<-temp[i,x+2]
}
# siberia =====================
x=siberia
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="siberia"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$siberia[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="siberia"){
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
  proxy_region$siberia[i]<-temp[i,x+2]
}
# s_africa =====================
x=s_africa
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="s_africa"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$s_africa[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="s_africa"){
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
  proxy_region$s_africa[i]<-temp[i,x+2]
}
# w_africa =====================
x=w_africa
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="w_africa"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$w_africa[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="w_africa"){
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
  proxy_region$w_africa[i]<-temp[i,x+2]
}
# e_africa =====================
x=e_africa
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="e_africa"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$e_africa[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="e_africa"){
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
  proxy_region$e_africa[i]<-temp[i,x+2]
}
# chile =====================
x=chile
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="chile"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$chile[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="chile"){
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
  proxy_region$chile[i]<-temp[i,x+2]
}
# w_usa =====================
x=w_usa
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="w_usa"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$w_usa[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="w_usa"){
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
  proxy_region$w_usa[i]<-temp[i,x+2]
}
# rocky_mt =====================
x=rocky_mt
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="rocky_mt"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$rocky_mt[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="rocky_mt"){
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
  proxy_region$rocky_mt[i]<-temp[i,x+2]
}
# carribean =====================
x=carribean
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="carribean"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$carribean[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="carribean"){
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
  proxy_region$carribean[i]<-temp[i,x+2]
}
# e_china_japan =====================
x=e_china_japan
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="e_china_japan"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$e_china_japan[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="e_china_japan"){
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
  proxy_region$e_china_japan[i]<-temp[i,x+2]
}
# se_brazil =====================
x=se_brazil
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="se_brazil"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$se_brazil[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="se_brazil"){
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
  proxy_region$se_brazil[i]<-temp[i,x+2]
}
# c_north_europe =====================
x=c_north_europe
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="c_north_europe"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$c_north_europe[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="c_north_europe"){
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
  proxy_region$c_north_europe[i]<-temp[i,x+2]
}
# scandinavia =====================
x=scandinavia
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="scandinavia"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$scandinavia[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="scandinavia"){
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
  proxy_region$scandinavia[i]<-temp[i,x+2]
}
# tibet =====================
x=tibet
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="tibet"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$tibet[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="tibet"){
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
  proxy_region$tibet[i]<-temp[i,x+2]
}
# mediterranean =====================
x=mediterranean
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="mediterranean"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$mediterranean[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="mediterranean"){
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
  proxy_region$mediterranean[i]<-temp[i,x+2]
}
# iceland =====================
x=iceland
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="iceland"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$iceland[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="iceland"){
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
  proxy_region$iceland[i]<-temp[i,x+2]
}
# e_pacific =====================
x=e_pacific
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="e_pacific"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$e_pacific[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="e_pacific"){
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
  proxy_region$e_pacific[i]<-temp[i,x+2]
}
# s_atlantic =====================
x=s_atlantic
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="s_atlantic"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$s_atlantic[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="s_atlantic"){
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
  proxy_region$s_atlantic[i]<-temp[i,x+2]
}
# c_atlantic =====================
x=c_atlantic
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="c_atlantic"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$c_atlantic[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="c_atlantic"){
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
  proxy_region$c_atlantic[i]<-temp[i,x+2]
}
# uk =====================
x=uk
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="uk"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$uk[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="uk"){
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
  proxy_region$uk[i]<-temp[i,x+2]
}
# n_indian_ocean =====================
x=n_indian_ocean
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="n_indian_ocean"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$n_indian_ocean[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="n_indian_ocean"){
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
  proxy_region$n_indian_ocean[i]<-temp[i,x+2]
}
# australia =====================
x=australia
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="australia"){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+7]
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
  model_region$australia[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="australia"){
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
  proxy_region$australia[i]<-temp[i,x+2]
}




write.csv(proxy_region,"data/proxy_region_new.csv")
write.csv(model_region,"data/model_region_new.csv")

library(readr)
library(ggplot2)
library(stats)

model_anomaly <- read_csv("data/model_sealand.csv")
model_anomaly <- model_anomaly[-1]
model_data <- model_anomaly

proxy_data <- read_csv("data/proxy_sealand.csv")
#proxy_data <- proxy_data[-1]

proxy_data2 <- proxy_data[,16:236]
proxy_data2$sum <- rowSums (proxy_data2, na.rm = TRUE, dims = 1)
proxy_data$sum <- proxy_data2$sum

proxy_hemi <- data.frame(
  (matrix(vector(), 220, 8, dimnames=list(
    c(), 
    c("time",
      "NH", 
      "NM",
      "Tropical", 
      "SM",
      "SH",
      "AN",
      "AS"
    )
  ))
  )
)

model_hemi <- data.frame(
  (matrix(vector(), 220, 8, dimnames=list(
    c(), 
    c("time",
      "NH", 
      "NM",
      "Tropical", 
      "SM",
      "SH",
      "AN",
      "AS"
    )
  ))
  )
)
diff_hemi <- data.frame(
  (matrix(vector(), 220, 8, dimnames=list(
    c(), 
    c("time",
      "NH", 
      "NM",
      "Tropical", 
      "SM",
      "SH",
      "AN",
      "AS"
    )
  ))
  )
)
model_hemi$time = seq(100,22000,100)
proxy_hemi$time = seq(100,22000,100)

NH <- 0
NM <- 0
SM <- 0
Tropical <- 0
SH <- 0
AN <- 0
AS <- 0
grid_NH <- vector()
grid_Tropical <- vector()
grid_SH <- vector()
grid_SM <- vector()
grid_NM <- vector()
grid_AN <- vector()
grid_AS <- vector()

for(i in 1:nrow(proxy_data)){
  if(proxy_data$sum[i] != 0 && proxy_data$lat_band[i] == 'HN'){
    NH <- NH + 1
    grid_NH <- c(grid_NH, proxy_data$PageName[i])
    AN <- AN + 1
    grid_AN <- c(grid_AN, proxy_data$PageName[i])
  } else if (proxy_data$sum[i] != 0 && proxy_data$lat_band[i] == 'Trop') {
    Tropical <- Tropical + 1
    grid_Tropical <- c(grid_Tropical, proxy_data$PageName[i])
  } else if (proxy_data$sum[i] != 0 && proxy_data$lat_band[i] == 'SH') {
    SH <- SH + 1
    grid_SH <- c(grid_SH, proxy_data$PageName[i])
    AS <- AS + 1
    grid_AS <- c(grid_AS, proxy_data$PageName[i])
  } else if (proxy_data$sum[i] != 0 && proxy_data$lat_band[i] == 'SM') {
    SM <- SM + 1
    grid_SM <- c(grid_SM, proxy_data$PageName[i])
    AS <- AS + 1
    grid_AS <- c(grid_AS, proxy_data$PageName[i])
  } else if (proxy_data$sum[i] != 0 && proxy_data$lat_band[i] == 'NM') {
    NM <- NM + 1
    grid_NM <- c(grid_NM, proxy_data$PageName[i])
    AN <- AN + 1
    grid_AN <- c(grid_AN, proxy_data$PageName[i])
  }
}
proxy_data <- proxy_data[1:(length(proxy_data)-1)]
# AN =================================
x = AN
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_AN
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  model_hemi$AN[i]<-temp[i,x+2]
}

#proxy
x = AN
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
#starting from the 2nd col
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_AN
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
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
  proxy_hemi$AN[i]<-temp[i,x+2]
}

# AS =================================
x = AS
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_AS
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  model_hemi$AS[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_AS
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_hemi$AS[i]<-temp[i,x+2]
}


# NH====================================
x = NH
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_NH
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_hemi$NH[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_NH
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_hemi$NH[i]<-temp[i,x+2]
}

# Tropical ====================================
x = Tropical
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_Tropical
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
    cat(temp[row,col],' ')
    if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_hemi$Tropical[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_Tropical
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_hemi$Tropical[i]<-temp[i,x+2]
}

# SH====================================
x = SH
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_SH
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_hemi$SH[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_SH
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_hemi$SH[i]<-temp[i,x+2]
}

# SM====================================
x = SM
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_SM
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_hemi$SM[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_SM
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_hemi$SM[i]<-temp[i,x+2]
}

# NM====================================
x = NM
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_NM
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_hemi$NM[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_NM
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_hemi$NM[i]<-temp[i,x+2]
}


#======global======================

x = NH + SH + Tropical
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(model_data$PageName[i] %in% grid_NH||
     model_data$PageName[i] %in% grid_SH||
     model_data$PageName[i] %in% grid_Tropical
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
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  model_hemi$Global[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(proxy_data$PageName[i] %in% grid_NH||
     proxy_data$PageName[i] %in% grid_SH||
     proxy_data$PageName[i] %in% grid_Tropical
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
    if(is.na(temp[row,col])){
      ct <- ct
    }else if(temp[row,col]!=0){
      ct <- ct + 1
    }
  }
  if(ct==0){
    #change here
    temp[row,x+2]<-NA
  } else{
    temp[row,x+2]<-sum/ct
  }
  cat("finishing row ", i, "; ")
}

for(i in 1:220){
  #change here
  proxy_hemi$Global[i]<-temp[i,x+2]
}

#++++++++++++++plot+++++++++++++++


diff_hemi$Global <- 0
diff_hemi$time <- proxy_hemi$time
for(row in 1:220){
  for(col in 2:5){
    diff_hemi[row,col] <- as.numeric(model_hemi[row,col]) - as.numeric(proxy_hemi[row,col])
  }
}
write.csv(diff_hemi,"data/diff_hemi.csv")

# plot hemi diff +++++++++++++++++++++++++++++++++++


plot_data <- data.frame(time=proxy_hemi$time, proxy_n=proxy_hemi$AN, model_n=model_hemi$AN, proxy_s=proxy_hemi$AS, model_s=model_hemi$AS)
plot <- ggplot()+
  annotate("rect", fill = "light blue", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           alpha = .2)+
  annotate("rect", fill = "pink", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 11000, y = 4, label = "Hemispheric differences (NH - SH)", size = 6)+
  annotate("text", x = 1000, y = 3, label = "NH warmer", size = 4)+
  annotate("text", x = 1000, y = -3, label = "SH warmer", size = 4)+
  annotate("text", x = 12400, y = -4, label = "YD", size = 4)+
  geom_line(data=plot_data,aes(time, proxy_n - proxy_s, colour = "Proxy"), size = 2)+
  geom_line(data=plot_data,aes(time, model_n - model_s, colour = "Model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Proxy' = 'blue',
    'Model' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Temperature Anomaly Difference (°C)"), limits = c(-4, 4),  breaks = scales::pretty_breaks(n = 20))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25Â°S ~ 90Â°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        #axis.text = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        #axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.66,0.2),
        #legend.spacing.x = unit(10, 'pt'),
        #legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/hemi_diff.jpg", width = 6, height = 4)

# plot NH ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$NH, model=model_hemi$NH)
plot <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 11000, y = 5, label = "60°N ~ 90°N", size = 8)+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"), size = 2)+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "orange2") +
  geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-10, 5),  breaks = scales::pretty_breaks(n = 20))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25Â°S ~ 90Â°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        #axis.text = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        #axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.66,0.2),
        #legend.spacing.x = unit(10, 'pt'),
        #legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/NH_lat_band.jpg", width = 6, height = 4)

# plot NM ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$NM, model=model_hemi$NM)
plot <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 11000, y = 5, label = "20°N ~ 60°N", size = 8)+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"), size = 2)+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "orange2") +
  geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-10, 5),  breaks = scales::pretty_breaks(n = 20))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25Â°S ~ 90Â°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        #axis.text = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        #axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.66,0.2),
        #legend.spacing.x = unit(10, 'pt'),
        #legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/NM_lat_band.jpg", width = 6, height = 4)

# plot SM ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$SM, model=model_hemi$SM)
plot <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 11000, y = 5, label = "20°S ~ 60°S", size = 8)+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"), size = 2)+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "orange2") +
  geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-10, 5),  breaks = scales::pretty_breaks(n = 20))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25Â°S ~ 90Â°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        #axis.text = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        #axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.66,0.2),
        #legend.spacing.x = unit(10, 'pt'),
        #legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/SM_lat_band.jpg", width = 6, height = 4)

# plot SH ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$SH, model=model_hemi$SH)
plot <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 11000, y = 5, label = "60°S ~ 90°S", size = 8)+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"), size = 2)+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "orange2") +
  geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-10, 5),  breaks = scales::pretty_breaks(n = 20))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25Â°S ~ 90Â°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        #axis.text = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        #axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.66,0.2),
        #legend.spacing.x = unit(10, 'pt'),
        #legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/SH_lat_band.jpg", width = 6, height = 4)

# plot Tropical ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$Tropical, model=model_hemi$Tropical)
plot <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 11000, y = 5, label = "20°S ~ 20°N", size = 8)+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"), size = 2)+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "orange2") +
  geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-10, 5),  breaks = scales::pretty_breaks(n = 20))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25Â°S ~ 90Â°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        #axis.text = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        #axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0.66,0.2),
        #legend.spacing.x = unit(10, 'pt'),
        #legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/Tropical_lat_band.jpg", width = 6, height = 4)


# ===============================Sea Land===========================================
land = 177
sea = 148
all = land + sea

model_hemi$land = NA
proxy_hemi$land = NA
model_hemi$sea = NA
proxy_hemi$sea = NA
model_hemi$all = NA
proxy_hemi$all = NA

#land ==============================
x = land
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$sea_land[i])) &
     model_data$sea_land[i] == "land"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  model_hemi$land[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$sea_land[i])) &
     proxy_data$sea_land[i] == "land"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
  proxy_hemi$land[i]<-temp[i,x+2]
}

# ========================sea=========================

x = sea
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$sea_land[i])) &
     model_data$sea_land[i] == "sea"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  model_hemi$sea[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$sea_land[i])) &
     proxy_data$sea_land[i] == "sea"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
  proxy_hemi$sea[i]<-temp[i,x+2]
}

# ========================all=========================

x = all
#model
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$sea_land[i])) &
     model_data$sea_land[i] == "sea" || model_data$sea_land[i] == "land"
  ){
    for(j in 1:220){
      temp[j,k]<-model_data[i,j+11]
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
  model_hemi$sea[i]<-temp[i,x+2]
}

#proxy
#change here
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$sea_land[i])) &
     proxy_data$sea_land[i] == "sea" || proxy_data$sea_land[i] == "all"
  ){
    for(j in 1:220){
      #change here
      temp[j,k]<-proxy_data[i,j+16]
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
  proxy_hemi$sea[i]<-temp[i,x+2]
}


write.csv(proxy_hemi,"data/proxy_lat_band3.csv")
write.csv(model_hemi,"data/model_lat_band3.csv")

# se_usa =====================
x=se_usa
#model
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(model_data$regional[i])) && model_data$regional[i]=="se_usa"){
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
  model_region$se_usa[i]<-temp[i,x+2]
}

#proxy
temp <- data.frame(matrix(vector(),220,x+2))
temp$X1 <- seq(100,22000,100)
k <- 2
for(i in 1:nrow(proxy_data)){
  if(!(is.na(proxy_data$regional[i])) && proxy_data$regional[i]=="se_usa"){
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
  proxy_region$se_usa[i]<-temp[i,x+2]
}


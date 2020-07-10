library(readr)
library(ggplot2)
library(stats)

model_anomaly <- read_csv("data/Feng_model_anomaly.csv")
model_anomaly <- model_anomaly[-1]
model_data <- model_anomaly

proxy_data <- read_csv("data/grid_anomaly.csv")
proxy_data <- proxy_data[-1]

proxy_data2 <- proxy_data[,10:230]
proxy_data2$sum <- rowSums (proxy_data2, na.rm = TRUE, dims = 1)
proxy_data$sum <- proxy_data2$sum

proxy_hemi <- data.frame(
  (matrix(vector(), 220, 4, dimnames=list(
    c(), 
    c("time",
      "NH", 
      "Tropical", 
      "SH"
    )
  ))
  )
)

model_hemi <- data.frame(
  (matrix(vector(), 220, 4, dimnames=list(
    c(), 
    c("time",
      "NH", 
      "Tropical", 
      "SH"
    )
  ))
  )
)
diff_hemi <- data.frame(
  (matrix(vector(), 220, 4, dimnames=list(
    c(), 
    c("time",
      "NH", 
      "Tropical", 
      "SH"
    )
  ))
  )
)
model_hemi$time = seq(100,22000,100)
proxy_hemi$time = seq(100,22000,100)

NH <- 0
Tropical <- 0
SH <- 0
grid_NH <- vector()
grid_Tropical <- vector()
grid_SH <- vector()

for(i in 1:nrow(proxy_data)){
  if(proxy_data$sum[i] != 0 && proxy_data$PageNumber[i] <= 864){
    NH <- NH + 1
    grid_NH <- c(grid_NH, proxy_data$PageName[i])
  } else if (proxy_data$sum[i] != 0 && proxy_data$PageNumber[i] <= 1728) {
    Tropical <- Tropical + 1
    grid_Tropical <- c(grid_Tropical, proxy_data$PageName[i])
  } else if (proxy_data$sum[i] != 0 && proxy_data$PageNumber[i] > 864) {
    SH <- SH + 1
    grid_SH <- c(grid_SH, proxy_data$PageName[i])
  }
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
      temp[j,k]<-proxy_data[i,j+10]
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
      temp[j,k]<-proxy_data[i,j+10]
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
      temp[j,k]<-proxy_data[i,j+10]
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
      temp[j,k]<-proxy_data[i,j+10]
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


write.csv(proxy_hemi,"data/proxy_hemi.csv")
write.csv(model_hemi,"data/model_hemi.csv")


diff_hemi$Global <- 0
diff_hemi$time <- proxy_hemi$time
for(row in 1:220){
  for(col in 2:5){
    diff_hemi[row,col] <- as.numeric(model_hemi[row,col]) - as.numeric(proxy_hemi[row,col])
  }
}
write.csv(diff_hemi,"data/diff_hemi.csv")

# plot NH ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$NH, model=model_hemi$NH, diff=diff_hemi$NH)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-6, 3))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly 25°N ~ 90°N") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/NH.jpg", width = 6, height = 4)

# plot SH ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$SH, model=model_hemi$SH, diff=diff_hemi$SH)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-6, 3))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly 25°S ~ 90°S") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/SH.jpg", width = 6, height = 4)

# plot Tropical ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$Tropical, model=model_hemi$Tropical)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-6, 3))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly 25°S ~ 25°N") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/Tropical.jpg", width = 6, height = 4)


# plot Global ++++++++++++++++++++++++++

plot_data <- data.frame(time=proxy_hemi$time, proxy=proxy_hemi$Global, model=model_hemi$Global)
plot <- ggplot()+
  geom_line(data=plot_data,aes(time, proxy, colour = "Anomaly in Proxy Data"))+
  geom_line(data=plot_data,aes(time, model, colour = "Anomaly in Model Data"))+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'Anomaly in Proxy Data' = 'blue',
    'Anomaly in Model Data' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 0.6, linetype='dotted') +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-6, 3))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  ggtitle("Temperature Anomaly, Global") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = "top",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank()
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/hemisphere/Global.jpg", width = 6, height = 4)

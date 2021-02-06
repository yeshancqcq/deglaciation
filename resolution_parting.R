library(readr)
X1 <- read_csv(x)
setwd("C:/Users/yesha/Documents/GitHub/deglaciation")

filenames <- list.files(path = "Holocene_data/raw_data/", pattern=NULL, all.files=FALSE,
                        full.names=FALSE)

res = data.frame(id = 1:length(filenames),
                name = filenames,
                min_yr = NA,
                max_yr = NA,
                total_record = NA,
                resolution = NA,
                record_holocene = NA,
                record_deglacier = NA,
                resolution_holocene = NA,
                resolution_deglacier = NA
                 )

for(i in 1:length(filenames)){
  x = paste("Holocene_data/raw_data/",filenames[i],sep="")
  df = read_csv(x)
  df <- df[!is.na(as.numeric(as.character(df$age))),]
  #df <- df[!is.na(as.numeric(as.character(df$temperature))),]
  record_total <- nrow(df)
  res$min_yr[i] <- df$age[1]
  res$max_yr[i] <- df$age[record_total]
  res$total_record[i] <- record_total
  if(record_total != 0){
    res$resolution[i] <- (df$age[record_total] - df$age[1]) / record_total 
  } else {
    res$resolution[i] <- 0
  }
  max_holocene <- 0
  if(df$age[1]>10000){
    max_holocene <- 0
  } else if(df$age[record_total] <= 10000){
    max_holocene <- record_total
  } else {
    j <- 1
    while(j <= nrow(df) - 1){
      if(df$age[j] <= 10000 && df$age[j + 1] > 10000){
        max_holocene <- j
        j <- nrow(df)
      } else {
        j <- j + 1
      }
    }
  }
  res$record_holocene[i] <- max_holocene
  num_deg <- record_total - max_holocene
  res$record_deglacier[i] <- num_deg
  if(max_holocene == 0){
    res$resolution_holocene[i] <- 0
  } else if (max_holocene == record_total){
    res$resolution_holocene[i] <- (df$age[record_total] - df$age[1]) / record_total 
  } else {
    res$resolution_holocene[i] <- (df$age[max_holocene] - df$age[1]) / max_holocene
  } 
  if(num_deg != 0){
    if(max_holocene <= 1){
      res$record_deglacier[i] <- (df$age[record_total] - df$age[1] ) / num_deg
    } else{
      res$resolution_deglacier[i] <- (df$age[record_total] - df$age[max_holocene - 1]) / num_deg
    }
  } else {
    res$resolution_deglacier[i] <- 0
  }
  
  cat("finishing", i,"\n")
}

filenames <- list.files(path = "Holocene_data/andy/", pattern=NULL, all.files=FALSE,
                        full.names=FALSE)

res2 = data.frame(id = 1:length(filenames),
                  name = filenames,
                  min_yr = NA,
                  max_yr = NA,
                  total_record = NA,
                  resolution = NA,
                  record_holocene = NA,
                  record_deglacier = NA,
                  resolution_holocene = NA,
                  resolution_deglacier = NA
)

for(i in 1:length(filenames)){
  x = paste("Holocene_data/andy/",filenames[i],sep="")
  df = read_csv(x)
  df <- df[!is.na(as.numeric(as.character(df$Age))),]
  #df <- df[!is.na(as.numeric(as.character(df$temperature))),]
  record_total <- nrow(df)
  res2$min_yr[i] <- df$Age[1]
  res2$max_yr[i] <- df$Age[record_total]
  res2$total_record[i] <- record_total
  if(record_total != 0){
    res2$resolution[i] <- (df$Age[record_total] - df$Age[1]) / record_total 
  } else {
    res2$resolution[i] <- 0
  }
  max_holocene <- 0
  if(df$Age[1]>10000){
    max_holocene <- 0
  } else if(df$Age[record_total] <= 10000){
    max_holocene <- record_total
  } else {
    j <- 1
    while(j <= nrow(df) - 1){
      if(df$Age[j] <= 10000 && df$Age[j + 1] > 10000){
        max_holocene <- j
        j <- nrow(df)
      } else {
        j <- j + 1
      }
    }
  }
  res2$record_holocene[i] <- max_holocene
  num_deg <- record_total - max_holocene
  res2$record_deglacier[i] <- num_deg
  if(max_holocene == 0){
    res2$resolution_holocene[i] <- 0
  } else if (max_holocene == record_total){
    res2$resolution_holocene[i] <- (df$Age[record_total] - df$Age[1]) / record_total 
  } else {
    res2$resolution_holocene[i] <- (df$Age[max_holocene] - df$Age[1]) / max_holocene
  } 
  if(num_deg != 0){
    if(max_holocene <= 1){
      res2$record_deglacier[i] <- (df$Age[record_total] - df$Age[1] ) / num_deg
    } else{
      res2$resolution_deglacier[i] <- (df$Age[record_total] - df$Age[max_holocene - 1]) / num_deg
    }
  } else {
    res2$resolution_deglacier[i] <- 0
  }
  
  cat("finishing", i,"\n")
}

total_res <- rbind(res, res2)

write.csv(total_res, file="GitHub/deglaciation/data/resolution.csv",fileEncoding="UTF-8")

#======= plots ========

total_res <- read_csv("data/resolution.csv")

library(ggplot2)
# Basic density
p <- ggplot() + 
  geom_density(data = total_res, aes(x=resolution_deglacier, color = "Deglaciation"))+
  geom_density(data = total_res, aes(x=resolution_holocene, color = "Holocene"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10),
        legend.justification = c(0, 0),
        legend.position = c(0.7, 0.7),
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  labs(y = "Density",
       x = "Resolution",
       colour = "Legend"
       )+
  scale_x_continuous(limits = c(-10,500), breaks = scales::pretty_breaks(n = 10))
p
# Add mean line
p+ geom_vline(aes(xintercept=mean(weight)),
              color="blue", linetype="dashed", size=1)


p <- ggplot()+
  geom_histogram(data = total_res, aes(x=resolution_holocene, fill = "11.5 Ka ~ Present"),binwidth = 10, alpha = 0.4) +
  geom_histogram(data = total_res, aes(x=resolution_deglacier, fill = "22 ~ 11.5 Ka"),binwidth = 10, alpha = 0.5) +
  scale_x_continuous(expand = c(0, 0),limits = c(5,1015), breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(expand = c(0, 0),name = expression("Number of Data Points"), limits = c(0, 50),  breaks = scales::pretty_breaks(n = 10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.justification = c(0, 0),
        legend.position = c(0.6, 0.7),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  labs(y = "Num. of Data Points",
       x = "Resolution",
       colour = "",
       fill = ""
  )+
  scale_fill_manual(values = c(
    "11.5 Ka ~ Present" = "gray30",
    "22 ~ 11.5 Ka" = "salmon"
  ))
p




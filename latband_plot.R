library(ggplot2)
library(readr)
library(gtable)
library(grid)
library(dplyr)
library(gridExtra)
library(cowplot)
setwd("C:/Users/yesha/Documents/GitHub/deglaciation")

proxy_region <- read_csv("data/proxy_lat_band3.csv")
model_region <- read_csv("data/model_lat_band3.csv")

plot_data1 <- data.frame(time=proxy_region$time, proxy=proxy_region$NH, model=model_region$NH)
plot1 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 3000, y = 2.5, label = "90°~60°N", size = 8)+
  annotate("text", x = 21000, y = 2.5, label = "A", size = 8)+
  #annotate("text", x = 12300, y = -6, label = "YD", size = 6, color = 'orange3')+
  #annotate("text", x = 10000, y = -4.5, label = "anomaly base", size = 6, color = 'purple')+
  geom_line(data=plot_data1,aes(time, proxy, colour = "proxy"), size = 2)+
  geom_line(data=plot_data1,aes(time, model, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  #geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "black") +
  scale_y_continuous(name = expression(" "), limits = c(-10, 3),  breaks = scales::pretty_breaks(n = 10))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂÃÂ°S ~ 90ÃÂÃÂ°S") +
  theme(panel.grid.major = element_line(color="gray80"),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = "none",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.background = element_rect(colour = "transparent",fill = "white"),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(expand = c(0, 0),limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot1

plot_data2 <- data.frame(time=proxy_region$time, proxy=proxy_region$NM, model=model_region$NM)
plot2 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 21000, y = 2.5, label = "B", size = 8)+
  annotate("text", x = 3000, y = 2.5, label = "60°~20°N", size = 8)+
  #annotate("text", x = 12300, y = -6, label = "YD", size = 6, color = 'orange3')+
  #annotate("text", x = 10000, y = -4.5, label = "anomaly base", size = 6, color = 'purple')+
  geom_line(data=plot_data2,aes(time, proxy, colour = "proxy"), size = 2)+
  geom_line(data=plot_data2,aes(time, model, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  #geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "black") +
  scale_y_continuous(name = expression(" "), limits = c(-4.5, 3),  breaks = scales::pretty_breaks(n = 5))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂÃÂ°S ~ 90ÃÂÃÂ°S") +
  theme(panel.grid.major = element_line(color="gray80"),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = "none",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.background = element_rect(colour = "transparent",fill = "white"),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(expand = c(0, 0),limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot2

plot_data3 <- data.frame(time=proxy_region$time, proxy=proxy_region$Tropical, model=model_region$Tropical)
plot3 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 21000, y = 2.5, label = "C", size = 8)+
  annotate("text", x = 3000, y = 2.5, label = "20°N~20°S", size = 8)+
  #annotate("text", x = 12300, y = -6, label = "YD", size = 6, color = 'orange3')+
  #annotate("text", x = 10000, y = -4.5, label = "anomaly base", size = 6, color = 'purple')+
  geom_line(data=plot_data3,aes(time, proxy, colour = "proxy"), size = 2)+
  geom_line(data=plot_data3,aes(time, model, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  #geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "black") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-4.5, 3),  breaks = scales::pretty_breaks(n = 5))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂÃÂ°S ~ 90ÃÂÃÂ°S") +
  theme(panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_text(size = 15),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = "none",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.background = element_rect(colour = "transparent",fill = "white"),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(expand = c(0, 0),limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot3

plot_data4 <- data.frame(time=proxy_region$time, proxy=proxy_region$SM, model=model_region$SM)
plot4 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 21000, y = 2.5, label = "D", size = 8)+
  annotate("text", x = 3000, y = 2.5, label = "20°~60°S", size = 8)+
  #annotate("text", x = 12300, y = -6, label = "YD", size = 6, color = 'orange3')+
  #annotate("text", x = 10000, y = -4.5, label = "anomaly base", size = 6, color = 'purple')+
  geom_line(data=plot_data4,aes(time, proxy, colour = "proxy"), size = 2)+
  geom_line(data=plot_data4,aes(time, model, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  #geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "black") +
  scale_y_continuous(name = expression(" "), limits = c(-4.5, 3),  breaks = scales::pretty_breaks(n = 5))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂÃÂ°S ~ 90ÃÂÃÂ°S") +
  theme(panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = "none",
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.background = element_rect(colour = "transparent",fill = "white"),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(expand = c(0, 0),limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot4

plot_data5 <- data.frame(time=proxy_region$time, proxy=proxy_region$SH, model=model_region$SH)
plot5 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 21000, y = 2.5, label = "E", size = 8)+
  annotate("text", x = 3000, y = 2.9, label = "60°~90°S", size = 8)+
  annotate("text", x = 12300, y = -9.5, label = "YD", size = 6, color = 'orange3')+
  annotate("text", x = 10000, y = -8.5, label = "anomaly", size = 6, color = 'purple')+
  annotate("text", x = 10000, y = -9.5, label = "base", size = 6, color = 'purple')+
  geom_line(data=plot_data5,aes(time, proxy, colour = "proxy"), size = 2)+
  geom_line(data=plot_data5,aes(time, model, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  #geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "black") +
  scale_y_continuous(name = expression(" "), limits = c(-10, 3),  breaks = scales::pretty_breaks(n = 10))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂÃÂ°S ~ 90ÃÂÃÂ°S") +
  theme(panel.grid.major = element_line(color="gray80"),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_text(size = 15),
        legend.justification = c(0, 0),
        legend.position = c(0.8,0.1),
        legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.background = element_rect(colour = "transparent",fill = "white"),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(expand = c(0, 0),limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot5

gp2 <- plot_grid(plot1, plot2, plot3, plot4, plot5, align = "v", nrow = 5, rel_heights = c(2, 1.2, 1.2, 1.2, 2.3))
gp2

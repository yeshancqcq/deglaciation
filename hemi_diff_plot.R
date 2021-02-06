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


plot_data <- data.frame(time=proxy_region$time, proxy=proxy_region$AN, model=model_region$AN)
plot1 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 3000, y = 2.5, label = "NH", size = 8)+
  annotate("text", x = 21000, y = 2.5, label = "A", size = 8)+
  #annotate("text", x = 12300, y = -6, label = "YD", size = 6, color = 'orange3')+
  #annotate("text", x = 10000, y = -4.5, label = "anomaly base", size = 6, color = 'purple')+
  geom_line(data=plot_data,aes(time, proxy, colour = "proxy"), size = 2)+
  geom_line(data=plot_data,aes(time, model, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "black") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-7, 3),  breaks = scales::pretty_breaks(n = 10))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂ°S ~ 90ÃÂ°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_blank(),
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
plot1

plot_data2 <- data.frame(time=proxy_region$time, proxy=proxy_region$AS, model=model_region$AS)
plot2 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 21000, y = 2.5, label = "B", size = 8)+
  annotate("text", x = 3000, y = 2.5, label = "SH", size = 8)+
  #annotate("text", x = 12300, y = -6, label = "YD", size = 6, color = 'orange3')+
  #annotate("text", x = 10000, y = -4.5, label = "anomaly base", size = 6, color = 'purple')+
  geom_line(data=plot_data2,aes(time, proxy, colour = "proxy"), size = 2)+
  geom_line(data=plot_data2,aes(time, model, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "black") +
  scale_y_continuous(name = expression("Temperature Anomaly (°C)"), limits = c(-7, 3),  breaks = scales::pretty_breaks(n = 10))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂ°S ~ 90ÃÂ°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_blank(),
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
plot2



plot_data3 <- data.frame(time=proxy_hemi$time, proxy_n=proxy_region$AN, model_n=model_region$AN, proxy_s=proxy_region$AS, model_s=model_region$AS)
plot3 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "light blue", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           alpha = .2)+
  annotate("rect", fill = "pink", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 2000, y = 3.5, label = "NH-SH", size = 8)+
  annotate("text", x = 21000, y =3.5, label = "C", size = 8)+
  annotate("text", x = 2000, y = 1.5, label = "NH warmer", size = 6)+
  annotate("text", x = 2000, y = -0.5, label = "SH warmer", size = 6)+
  annotate("text", x = 10000, y = -3, label = "anomaly", size = 6, color = 'purple')+
  annotate("text", x = 10000, y = -4, label = "base", size = 6, color = 'purple')+
  annotate("text", x = 12300, y = -4, label = "YD", size = 6, color = 'orange3')+
  geom_line(data=plot_data3,aes(time, proxy_n - proxy_s, colour = "proxy"), size = 2)+
  geom_line(data=plot_data3,aes(time, model_n - model_s, colour = "model"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy' = 'blue',
    'model' = 'red'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Hemispheric Differences (°C)"), limits = c(-4, 4),  breaks = scales::pretty_breaks(n = 10))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂ°S ~ 90ÃÂ°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text = element_text(size=12),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title= element_text(size=12),
        legend.justification = c(0, 0),
        legend.position = c(0.8,0.1),
        #legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(expand = c(0, 0),limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot3

gp1 <- plot_grid(plot1, plot2, plot3, align = "v", nrow = 3, rel_heights = c(1, 1, 1.1))
gp1



# ===test ====
plot_data4 <- data.frame(time=proxy_hemi$time, proxy1=proxy_hemi$AN, model1=model_hemi$AN, proxy2=proxy_region$AN, model2=model_region$AN)
plot4 <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "light blue", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           alpha = .2)+
  annotate("rect", fill = "pink", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 2000, y = 3.5, label = "NH-SH", size = 8)+
  annotate("text", x = 21000, y =3.5, label = "C", size = 8)+
  annotate("text", x = 2000, y = 1.5, label = "NH warmer", size = 6)+
  annotate("text", x = 2000, y = -0.5, label = "SH warmer", size = 6)+
  annotate("text", x = 10000, y = -3, label = "anomaly", size = 6, color = 'purple')+
  annotate("text", x = 10000, y = -4, label = "base", size = 6, color = 'purple')+
  annotate("text", x = 12300, y = -4, label = "YD", size = 6, color = 'orange3')+
  geom_line(data=plot_data4,aes(time, proxy1, colour = "proxy1"), size = 2)+
  geom_line(data=plot_data4,aes(time, model1 , colour = "model1"), size = 2)+
  geom_line(data=plot_data4,aes(time, proxy2, colour = "proxy2"), size = 2)+
  geom_line(data=plot_data4,aes(time, model2, colour = "model2"), size = 2)+
  #geom_line(data=plot_data,aes(time, diff+5, colour = "Model - Proxy"))+
  scale_colour_manual(values = c(
    'proxy1' = 'blue',
    'model1' = 'red',
    'proxy2' = 'light blue',
    'model2' = 'pink'
  ))+  
  geom_hline(yintercept = 0, size = 1, linetype='dotted', colour = "black") +
  #geom_hline(yintercept = -5, size = 1, linetype='dotted') +
  #geom_hline(yintercept = -10, size = 1, linetype='dotted', colour = "yellow3") +
  scale_y_continuous(name = expression("Hemispheric Differences (°C)"), limits = c(-7, 4),  breaks = scales::pretty_breaks(n = 10))+
  labs(y = "Anomaly",
       x = "Time (Years BP)",
       colour = 'Legend') +
  #ggtitle("Temperature Anomaly 25ÃÂ°S ~ 90ÃÂ°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text = element_text(size=12),
        #axis.ticks.y = element_blank(),
        #axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title= element_text(size=12),
        legend.justification = c(0, 0),
        legend.position = c(0.8,0.1),
        #legend.spacing.x = unit(10, 'pt'),
        legend.text = element_text(margin = margin(t = 3), size=rel(1.2)),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(expand = c(0, 0),limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot4

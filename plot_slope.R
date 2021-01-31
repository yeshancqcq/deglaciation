library(readr)
library(ggplot2)
library(gtable)
library(grid)
library(dplyr)
library(gridExtra)
library(cowplot)


trends <- read_csv("C:/Users/yesha/Documents/GitHub/deglaciation/data/trends.csv")

trends$band <- factor(trends$band, levels = c("NH High Lat","NH Mid Lat", "Low Lat","SH Mid Lat", "SH High Lat"))

all <- ggplot()+
  geom_vline(xintercept = 0, colour = 'black',linetype="dashed")+
  geom_hline(yintercept = 0, colour = 'black',linetype="dashed")+
  geom_point(data=trends,aes(p_all_slope, m_all_slope, colour=band),size = 3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 10),
        legend.justification = c(0, 0),
        axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        legend.background = element_rect(colour=NA, fill = NA),
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  annotate("text", x = -9.8e-4, y = -9e-4, label = "A")+
  annotate("text", x = 8e-4, y = -9e-4, label = "22 ka to present")+
  labs(y = "Model Data Trend",
       x = "Proxy Data Trend",
       colour = "Latitude Band")+
  scale_colour_manual(values = c(
    "Low Lat" = "salmon",
    "NH High Lat" = "light blue",
    "NH Mid Lat" = "blue",
    "SH High Lat" = "gray70",
    "SH Mid Lat" = "black"
  ))+
  scale_x_continuous(expand = c(0, 0), limits = c(-0.001, 0.001), breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(expand = c(0, 0), limits = c(-0.001, 0.001), breaks = scales::pretty_breaks(n = 10))
  #scale_x_reverse(expand = c(0, 0),limits = c(-0.001,0.001), breaks = scales::pretty_breaks(n = 10))+
  #scale_y_continuous(expand = c(0, 0),limits = c(-0.001,0.001), breaks = scales::pretty_breaks(n = 10))

all


deg <- ggplot()+
  geom_vline(xintercept = 0, colour = 'black',linetype="dashed")+
  geom_hline(yintercept = 0, colour = 'black',linetype="dashed")+
  geom_point(data=trends,aes(p_deg_slope, m_deg_slope, colour=band),size = 3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 10),
        legend.justification = c(0, 0),
        axis.text.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        legend.background = element_rect(colour=NA, fill = NA),
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  annotate("text", x = -9.8e-4, y = -9e-4, label = "B")+
  annotate("text", x = 8e-4, y = -9e-4, label = "22 - 11.5 ka")+
  labs(y = "Model Data Trend",
       x = "Proxy Data Trend",
       colour = "Latitude Band")+
  scale_colour_manual(values = c(
    "Low Lat" = "salmon",
    "NH High Lat" = "light blue",
    "NH Mid Lat" = "blue",
    "SH High Lat" = "gray70",
    "SH Mid Lat" = "black"
  ))+
  scale_x_continuous(expand = c(0, 0), limits = c(-0.001, 0.001), breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(expand = c(0, 0), limits = c(-0.001, 0.001), breaks = scales::pretty_breaks(n = 10))
#scale_x_reverse(expand = c(0, 0),limits = c(-0.001,0.001), breaks = scales::pretty_breaks(n = 10))+
#scale_y_continuous(expand = c(0, 0),limits = c(-0.001,0.001), breaks = scales::pretty_breaks(n = 10))

deg


hol <- ggplot()+
  geom_vline(xintercept = 0, colour = 'black',linetype="dashed")+
  geom_hline(yintercept = 0, colour = 'black',linetype="dashed")+
  geom_point(data=trends,aes(p_hol_slope, m_hol_slope, colour=band),size = 3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 10),
        legend.justification = c(0, 0),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position = c(0.05,0.2),
        legend.background = element_rect(colour=NA, fill = 'white'),
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  annotate("text", x = -9.8e-4, y = -9e-4, label = "C")+
  annotate("text", x = 8e-4, y = -9e-4, label = "11.5 ka to present")+
  labs(y = "Model Data Trend",
       x = "Proxy Data Trend",
       colour = "Latitude Band")+
  scale_colour_manual(values = c(
    "Low Lat" = "salmon",
    "NH High Lat" = "light blue",
    "NH Mid Lat" = "blue",
    "SH High Lat" = "gray70",
    "SH Mid Lat" = "black"
  ))+
  scale_x_continuous(expand = c(0, 0), limits = c(-0.001, 0.001), breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(expand = c(0, 0), limits = c(-0.001, 0.001), breaks = scales::pretty_breaks(n = 10))
#scale_x_reverse(expand = c(0, 0),limits = c(-0.001,0.001), breaks = scales::pretty_breaks(n = 10))+
#scale_y_continuous(expand = c(0, 0),limits = c(-0.001,0.001), breaks = scales::pretty_breaks(n = 10))

hol


gp <- plot_grid(all, deg, hol, align = "v", nrow = 3, rel_heights  = c(1, 1, 11/9))
gp

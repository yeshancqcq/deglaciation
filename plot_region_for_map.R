library(ggplot2)
library(readr)
proxy_region <- read_csv("data/proxy_region.csv")
model_region <- read_csv("data/model_region.csv")

plot_data <- data.frame(time=proxy_region$time, proxy=proxy_region$e_china_japan, model=model_region$e_china_japan)
plot <- ggplot()+
  annotate("rect", fill = "purple", xmin = 8000, xmax = 12000, ymin = -Inf, ymax = Inf,
           alpha = .2)+
  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
           alpha = .3)+
  annotate("text", x = 11000, y = 5, label = "Eastern China and Japan", size = 8)+
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
  #ggtitle("Temperature Anomaly 25°S ~ 90°S") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        axis.line.x = element_blank(),
        axis.text = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title.y= element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line= element_blank(),
        axis.title.x= element_blank(),
        legend.justification = c(0, 0),
        legend.position = "none",
        #legend.spacing.x = unit(10, 'pt'),
        #legend.text = element_text(margin = margin(t = 3), size=rel(0.83)),
        #legend.background = element_rect(colour = NA),
        #legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  scale_x_reverse(limits = c(22000, 100), breaks = scales::pretty_breaks(n = 10))
plot

ggsave("img/regional/e_china_japan.png", width = 6, height = 4, bg = "transparent")

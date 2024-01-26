#The purpose of this script is to visualize survey-weighted mean trust by US institution

# Library
library(ggplot2)
library(dplyr)
library(ggpubr)

##############################################
#Item: Artificial intelligence
##############################################

g <- ggplot(data = useAI) +
  coord_flip()+
  theme(legend.position = "none",
  ) +
  labs(title="Mean Trust by US Institution",
       subtitle="Tool: Artificial Intelligence",
       x ="", y = "") +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        
        axis.title.x = element_text(size = 10, vjust=-1.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust= 2, face = "bold"),
        axis.line = element_line(colour = "grey80"),
        plot.margin = margin(t = 0.7, r = 1.5, b = 0, l = 0.7,
                             unit = "cm")) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1)) +

  scale_x_discrete(limits = c("American Government Agencies","American Hospitals","American Companies","American Police Precincts"),
             labels=c("American Police Precincts" = "Police Precincts", "American Companies" = "Companies",
                      "American Hospitals" = "Hospitals", "American Research Labs" = "Research Labs",
                      "American Government Agencies" = "Gov. Agencies",
                      expand = c(10,10)))+
  
  scale_y_continuous(breaks = c(-1, 0, 1), lim = c(-1, 1),
                     labels = c("3", "4", "5"), expand = c(0,0)) +
  
#Removing unwanted elements
  theme(panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.ticks.x = element_blank()) +

  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 1, 
            fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = -1, ymax = 0, 
           fill = "red", alpha = 0.2) +
  geom_segment(aes(x = 0.5, xend = 4.5, y = 0, yend = 0),
               color = "yellow", size = 0.5, alpha = 0.2) +
  geom_segment(aes(x = "American Companies", xend = "American Companies", y = -1, yend = 1),
               color = "white", size = 0.25) +
  geom_segment(aes(x = "American Police Precincts", xend = "American Police Precincts", y = -1, yend = 1),
               color = "white", size = 0.25) +
  geom_segment(aes(x = "American Hospitals", xend = "American Hospitals", y = -1, yend = 1),
               color = "white", size = 0.25) +
  geom_segment(aes(x = "American Government Agencies", xend = "American Government Agencies", y = -1, yend = 1),
               color = "white", size = 0.25) +
  
#Police
  geom_segment( aes(x="American Police Precincts", xend="American Police Precincts", y= -0.41,
                    yend=0.32), color="black", size = 0.5) +
  geom_point( aes(x="American Police Precincts", y=0.32), color="black", size=1) +
  annotate("rect", xmin="American Police Precincts", xmax = "American Police Precincts",
           ymin=0.73, ymax = 0.87, color = "white", alpha = 0.3, fill = "white", size = 6) + 
  annotate("text", x="American Police Precincts", y=0.8, label="- 0.80***", color = "grey20", size = 2.75)+
  geom_segment(aes(x = "American Police Precincts", y = -0.41, xend = "American Police Precincts",
                   yend = -0.41), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +

#Companies
  geom_segment( aes(x="American Companies", xend="American Companies", y= -0.62, yend=-0.34),
              color="black", size = 0.5) +
  geom_point( aes(x="American Companies", y=-0.34), color="black", size=1 ) +
  annotate("rect", xmin="American Companies", xmax = "American Companies",
           ymin=0.73, ymax = 0.87, color = "white", alpha = 0.3, fill = "white", size = 6) + 
  annotate("text", x="American Companies", y=0.8, label="- 0.42***", color = "grey20", size = 2.75)+
  geom_segment(aes(x = "American Companies", y = -0.62, xend = "American Companies",
                   yend = -0.62), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  
#Hospitals
  geom_segment( aes(x="American Hospitals", xend="American Hospitals", y=0.07, yend=0.32),
                color="black", size = 0.5) +
  geom_point( aes(x="American Hospitals", y=0.32), color="black", size=1 ) +
  annotate("rect", xmin="American Hospitals", xmax = "American Hospitals",
           ymin=0.73, ymax = 0.87, color = "white", alpha = 0.3, fill = "white", size = 6) + 
  annotate("text", x="American Hospitals", y=0.8, label="- 0.36***", color = "grey20", size = 2.75)+
  geom_segment(aes(x = "American Hospitals", y = 0.07, xend = "American Hospitals",
                   yend = 0.07), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +

#Government
  geom_segment( aes(x="American Government Agencies", xend="American Government Agencies", y=-0.95, yend=-0.8),
                color="black", size = 0.5) +
  geom_point( aes(x="American Government Agencies", y=-0.95), color="black", size=1 ) +
  annotate("rect", xmin="American Government Agencies", xmax = "American Government Agencies",
           ymin=0.73, ymax = 0.87, color = "white", alpha = 0.3, fill = "white", size = 6) + 
  annotate("text", x="American Government Agencies", y=0.8, label="+ 0.20*  ", color = "grey20", size = 2.75)+
  geom_segment(aes(x = "American Government Agencies", y = -0.8, xend = "American Government Agencies",
                   yend = -0.8), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) 

##############################################
#Item: Machine learning
##############################################

g1 <- ggplot(data = useML) +
  coord_flip()+
  theme(legend.position = "none",
  ) +
  labs(title="",
       subtitle="Tool: Machine learning",
       x ="", y = "") +
  theme(plot.title = element_text(size = 0, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        
        axis.title.x = element_text(size = 10, vjust=-1.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust= 2, face = "bold"),
        axis.line = element_line(colour = "grey80"),
        plot.margin = margin(t = 0.7, r = 1.5, b = 0, l = 0.7,
                             unit = "cm")) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels=c("American Police Precincts" = "Police Precincts", "American Companies" = "Companies",
                            expand = c(10,10))) +
  
  scale_y_continuous(breaks = c(-1, 0, 1), lim = c(-1, 1),
                     labels = c("3", "4", "5"), expand = c(0,0)) +
  
#Removing unwanted elements
  theme(panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.ticks.x = element_blank()) +
  
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 1, 
           fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -1, ymax = 0, 
           fill = "red", alpha = 0.2) +
  geom_segment(aes(x = 0.5, xend = 2.5, y = 0, yend = 0),
               color = "yellow", size = 0.5, alpha = 0.2) +
  geom_segment(aes(x = "American Companies", xend = "American Companies", y = -1, yend = 1),
               color = "white", size = 0.25) +
  geom_segment(aes(x = "American Police Precincts", xend = "American Police Precincts", y = -1, yend = 1),
               color = "white", size = 0.25) +
  
#Police
  geom_segment( aes(x="American Police Precincts", xend="American Police Precincts", y= 0.53,
                    yend=-0.02), color="black", size = 0.5) +
  geom_point( aes(x="American Police Precincts", y=0.53), color="black", size=1) +
  annotate("rect", xmin="American Police Precincts", xmax = "American Police Precincts",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Police Precincts", y=0.8, label="-0.55***", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Police Precincts", y = -0.02, xend = "American Police Precincts",
                   yend = -0.02), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  
#Companies
  geom_segment( aes(x="American Companies", xend="American Companies", y= -0.53, yend=-0.24),
                color="black", size = 0.5) +
  geom_point( aes(x="American Companies", y=-0.24), color="black", size=1 ) +
  annotate("rect", xmin="American Companies", xmax = "American Companies",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Companies", y=0.8, label="-0.28***", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Companies", y = -0.53, xend = "American Companies",
                   yend = -0.53), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) 

##############################################
#Item: Smartphone Apps
##############################################

g2 <- ggplot(data = useAPP) +
  coord_flip()+
  theme(legend.position = "none",
  ) +
  labs(title="",
       subtitle="Tool: Smartphone apps",
       x ="", y = "") +
  theme(plot.title = element_text(size = 0, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        
        axis.title.x = element_text(size = 10, vjust=-1.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust= 2, face = "bold"),
        axis.line = element_line(colour = "grey80"),
        plot.margin = margin(t = 0.7, r = 1.5, b = 0, l = 0.7,
                             unit = "cm")) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  scale_x_discrete(limits = c("American Companies","American Police Precincts"),
      labels=c("American Police Precincts" = "Police Precincts", "American Companies" = "Companies",
                            expand = c(10,10))) +
  
  scale_y_continuous(breaks = c(-1, 0, 1), lim = c(-1, 1),
                     labels = c("3", "4", "5"), expand = c(0,0)) +
  
#Removing unwanted elements
  theme(panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.ticks.x = element_blank()) +
  
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 1, 
           fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -1, ymax = 0, 
           fill = "red", alpha = 0.2) +
  geom_segment(aes(x = 0.5, xend = 2.5, y = 0, yend = 0),
               color = "yellow", size = 0.5, alpha = 0.2) +
  geom_segment(aes(x = "American Companies", xend = "American Companies", y = -1, yend = 1),
               color = "white", size = 0.25) +
  geom_segment(aes(x = "American Police Precincts", xend = "American Police Precincts", y = -1, yend = 1),
               color = "white", size = 0.25) +
  
#Police
  geom_segment( aes(x="American Police Precincts", xend="American Police Precincts", y= 0.32,
                    yend=0.53), color="black", size = 0.5) +
  geom_point( aes(x="American Police Precincts", y= 0.53), color="black", size=1) +
  annotate("rect", xmin="American Police Precincts", xmax = "American Police Precincts",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Police Precincts", y=0.8, label="- 0.21", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Police Precincts", y = 0.32, xend = "American Police Precincts",
                   yend = 0.32), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  
#Companies
  geom_segment( aes(x="American Companies", xend="American Companies", y= -0.48, yend=-0.24),
                color="black", size = 0.5) +
  geom_point( aes(x="American Companies", y=-0.24), color="black", size=1 ) +
  annotate("rect", xmin="American Companies", xmax = "American Companies",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Companies", y=0.8, label="- 0.23**", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Companies", y = -0.48, xend = "American Companies",
                   yend = -0.48), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) 

##############################################
#Item: Implicit bias training
##############################################

g3 <- ggplot(data = useBT) +
  coord_flip()+
  theme(legend.position = "none",
  ) +
  labs(title="",
       subtitle="Tool: Implicit bias training",
       x ="", y = "") +
  theme(plot.title = element_text(size = 0, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        
        axis.title.x = element_text(size = 10, vjust=-1.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust= 2, face = "bold"),
        axis.line = element_line(colour = "grey80"),
        plot.margin = margin(t = 0.7, r = 1.5, b = 0, l = 0.7,
                             unit = "cm")) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels=c("American Police Precincts" = "Police Precincts", "American Companies" = "Companies",
                            expand = c(10,10))) +
  
  scale_y_continuous(breaks = c(-1, 0, 1), lim = c(-1, 1),
                     labels = c("3", "4", "5"), expand = c(0,0)) +
  
#Removing unwanted elements
  theme(panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.ticks.x = element_blank()) +
  
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 1, 
           fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -1, ymax = 0, 
           fill = "red", alpha = 0.2) +
  geom_segment(aes(x = 0.5, xend = 2.5, y = 0, yend = 0),
               color = "yellow", size = 0.5, alpha = 0.2) +
  geom_segment(aes(x = "American Companies", xend = "American Companies", y = -1, yend = 1),
               color = "white", size = 0.25) +
  geom_segment(aes(x = "American Police Precincts", xend = "American Police Precincts", y = -1, yend = 1),
               color = "white", size = 0.25) +
  
#Police
  geom_segment( aes(x="American Police Precincts", xend="American Police Precincts", y= 0.02,
                    yend= 0.53), color="black", size = 0.5) +
  geom_point( aes(x="American Police Precincts", y= 0.53), color="black", size=1) +
  annotate("rect", xmin="American Police Precincts", xmax = "American Police Precincts",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Police Precincts", y=0.8, label="- 0.51***", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Police Precincts", y = 0.02, xend = "American Police Precincts",
                   yend = 0.02), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  
#Companies
  geom_segment( aes(x="American Companies", xend="American Companies", y= -0.24, yend=-0.71),
                color="black", size = 0.5) +
  geom_point( aes(x="American Companies", y=-0.24), color="black", size=1 ) +
  annotate("rect", xmin="American Companies", xmax = "American Companies",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Companies", y=0.8, label="- 0.46***", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Companies", y = -0.71, xend = "American Companies",
                   yend = -0.71), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) 

##############################################
#Item: Mindfullness training
##############################################

g4 <- ggplot(data = useBT) +
  coord_flip()+
  theme(legend.position = "none",
  ) +
  labs(title="",
       subtitle="Tool: Mindfullness training",
       x ="", y = "") +
  theme(plot.title = element_text(size = 0, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        
        axis.title.x = element_text(size = 10, vjust=-1.5, face = "bold"),
        axis.title.y = element_text(size = 10, vjust= 2, face = "bold"),
        axis.line = element_line(colour = "grey80"),
        plot.margin = margin(t = 0.7, r = 1.5, b = 0, l = 0.7,
                             unit = "cm")) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  scale_x_discrete(limits = c("American Companies","American Police Precincts"),
    labels=c("American Police Precincts" = "Police Precincts", "American Companies" = "Companies",
                            expand = c(10,10))) +
  
  scale_y_continuous(breaks = c(-1, 0, 1), lim = c(-1, 1),
                     labels = c("3", "4", "5"), expand = c(0,0)) +
  
#Removing unwanted elements
  theme(panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.ticks.x = element_blank()) +
  
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = 0, ymax = 1, 
           fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -1, ymax = 0, 
           fill = "red", alpha = 0.2) +
  geom_segment(aes(x = 0.5, xend = 2.5, y = 0, yend = 0),
               color = "yellow", size = 0.5, alpha = 0.2) +
  geom_segment(aes(x = "American Companies", xend = "American Companies", y = -1, yend = 1),
               color = "white", size = 0.25) +
  geom_segment(aes(x = "American Police Precincts", xend = "American Police Precincts", y = -1, yend = 1),
               color = "white", size = 0.25) +
  
#Police
  geom_segment( aes(x="American Police Precincts", xend="American Police Precincts", y= 0.50,
                    yend= 0.53), color="black", size = 0.5) +
  geom_point( aes(x="American Police Precincts", y= 0.53), color="black", size=1) +
  annotate("rect", xmin="American Police Precincts", xmax = "American Police Precincts",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Police Precincts", y=0.8, label="- 0.03***", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Police Precincts", y = 0.50, xend = "American Police Precincts",
                   yend = 0.50), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  
#Companies
  geom_segment( aes(x="American Companies", xend="American Companies", y= -0.24, yend=-0.10),
                color="black", size = 0.5) +
  geom_point( aes(x="American Companies", y=-0.24), color="black", size=1 ) +
  annotate("rect", xmin="American Companies", xmax = "American Companies",
           ymin=0.68, ymax = 0.92, alpha = 0.5, color = "white", fill = "white", size = 6) + 
  annotate("text", x="American Companies", y=0.8, label="+ 0.14", color = "grey20", size = 2.5)+
  geom_segment(aes(x = "American Companies", y = -0.100000000001, xend = "American Companies",
                   yend = -0.10), arrow = arrow(length = unit(0.2, "cm"), type = "closed")) 

##################################
#Combining graphs into one figure
##################################

g5 <- ggarrange(g,ggarrange(g1,g2,g3,g4),nrow = 2)
g5 <- annotate_figure(g5, bottom = text_grob("* = p < 0.05    ** = p < 0.01    *** = p < 0.001",
                                             color = "grey50", face = "italic", size = 9, hjust = 0.42, vjust = -0.7))
g5

ggsave("myplot.pdf", width = 11, path = "/Users/andrew/Desktop/")


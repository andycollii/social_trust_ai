#The purpose of this script is to visualize the survey-weighted means for research labs

#Read in the "meansgraphing.csv" file
barplot <- read_csv(file = "/Users/acac/Desktop/Academic/Jason/PLOS_one/Data/meansgraphing2.csv")

barplotlab <- barplot %>%
  filter(Institution == "Labs")

##########################################
#Labs
##########################################

g <- ggplot(data = barplotlab, mapping = aes(x = Tech, y = use, fill = Tech)) +
  geom_bar(position="dodge", stat="summary", fun = "mean") +
  theme_minimal() +
  geom_hline(yintercept=4.31, color="red", size=1) +
  geom_hline(yintercept=4.31, color="pink", size=0.4) +
  coord_cartesian(ylim = c(3, 5)) +
  
  #error bars
  geom_errorbar( aes(x="AI", ymin= 3.6469 - 0.0687,
                     ymax= 3.6469 + 0.0687),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="APP", ymin= 3.6657 - 0.0639,
                     ymax= 3.6657 + 0.0639),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="BT", ymin= 3.6027 - 0.0674,
                     ymax= 3.6027 + 0.0674),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="ML", ymin= 3.8233 - 0.0683,
                     ymax= 3.8233 + 0.0683),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="MT", ymin= 3.9 - 0.0719,
                     ymax= 3.9 + 0.0719),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +

#Basics
labs(title="Mean Trust by Tool",
     subtitle="American Research Labs",
     x ="", y = "Response") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        
        axis.title.x = element_text(size = 12, vjust=-1.5),
        axis.title.y = element_text(size = 12, vjust= 7),
        axis.line = element_line(colour = "grey80"),
        
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        legend.title=element_text(size=7.5, hjust = 0.5),
        
        plot.margin = margin(t = 0.7, r = 1, b = 0.6, l = 1,
                             unit = "cm")) +
  scale_x_discrete(limits = c("BT", "AI", "APP", "ML", "MT")) +
  
  #Removals
  theme(panel.grid.major = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(), 
        axis.ticks.x = element_blank()) +
  
  #Annotating P values
  annotate("text", x="AI", y=3.0, label="p < 0.001", color = "white", size = 3) +
  annotate("text", x="BT", y=3.0, label="p < 0.001", color = "white", size = 3) +
  annotate("text", x="ML", y=3.0, label="p < 0.001", color = "white", size = 3) +
  annotate("text", x="MT", y=3.0, label="p < 0.001", color = "white", size = 3) +
  annotate("text", x="APP", y=3.0, label="p < 0.001", color = "white", size = 3) 

g <- ggarrange (g, nrow = 1)
g <- annotate_figure(g, bottom = text_grob("The red line indicates baseline levels of trust. Verb: create",
                                               color = "grey50", face = "italic", size = 9, hjust = 0.5, vjust = -0.7))
g

ggsave("myplot.pdf", path = "/Users/andrew/Desktop/")
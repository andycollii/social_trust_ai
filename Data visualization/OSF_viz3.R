#The purpose of this script is to visualize survey-weighted means for American companies and American police precincts

#Read in the "meansgraphing.csv" file
barplot <- read_csv(file = "/Users/acac/Desktop/Academic/Jason/PLOS_one/Data/meansgraphing2.csv")

barplotpolice <- barplot %>%
  filter(Institution == "Police")
barplotcomp <- barplot %>%
  filter(Institution == "Companies")

##########################################
#Police
##########################################

g1 <- ggplot(data = barplotpolice, mapping = aes(x = Tech, y = use, fill = Tech)) +
  geom_bar(position="dodge", stat="summary", fun = "mean") +
  theme_minimal() +
  geom_hline(yintercept=4.53, color="red", size=1) +
  geom_hline(yintercept=4.53, color="pink", size=0.4) +
  coord_cartesian(ylim = c(3, 5)) +

  #error bars
  geom_errorbar( aes(x="AI", ymin= 3.7275 - 0.0723,
                     ymax= 3.7275 + 0.0723),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="APP", ymin= 4.3169 - 0.0794,
                     ymax= 4.3169 + 0.0794),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="BT", ymin= 4.0184 - 0.0786,
                     ymax= 4.0184 + 0.0786),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="ML", ymin= 3.9759 - 0.0752,
                     ymax= 3.9759 + 0.0752),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="MT", ymin= 4.5004 - 0.0757,
                     ymax= 4.5004 + 0.0757),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +

#Basics
  labs(title="",
       subtitle="American Police Precincts",
       x ="", y = "") +
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
  scale_x_discrete(limits = c("AI", "ML", "BT", "APP", "MT")) +

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
annotate("text", x="MT", y=3.0, label="p > 0.05", color = "white", size = 3) +
annotate("text", x="APP", y=3.0, label="p > 0.05", color = "white", size = 3) 
  
g1

##########################################
#Companies
##########################################

g2 <- ggplot(data = barplotcomp, mapping = aes(x = Tech, y = use, fill = Tech)) +
  geom_bar(position="dodge", stat="summary", fun = "mean") +
  theme_minimal() +
  geom_hline(yintercept=3.76, color="red", size=1) +
  geom_hline(yintercept=3.76, color="pink", size=0.4) +
  coord_cartesian(ylim = c(3, 5)) +
  
  #error bars
  geom_errorbar( aes(x="AI", ymin= 3.342 - 0.0601,
                     ymax= 3.342 + 0.0601),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="APP", ymin= 3.525 - 0.0645,
                     ymax= 3.525 + 0.0645),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="BT", ymin= 3.294 - 0.0728,
                     ymax= 3.294 + 0.0728),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="ML", ymin= 3.474 - 0.066,
                     ymax= 3.474 + 0.066),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  geom_errorbar( aes(x="MT", ymin= 3.9 - 0.0719,
                     ymax= 3.9 + 0.0719),
                 width=0.4, colour="orange", alpha=0.9, size=0.5) +
  
  #Basics
  labs(title="",
       subtitle="American Companies",
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
  scale_x_discrete(limits = c("BT", "AI", "ML", "APP", "MT")) +
  
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
  annotate("text", x="MT", y=3.0, label="p > 0.05", color = "white", size = 3) +
  annotate("text", x="APP", y=3.0, label="p < 0.001", color = "white", size = 3) 

g2


#################################


g10 <- ggarrange (g2,g1, nrow = 1)
g10 <- annotate_figure(g10, bottom = text_grob("The red line indicates baseline levels of trust. Verb: create",
                                             color = "grey50", face = "italic", size = 9, hjust = 0.5, vjust = -0.7))

g10 <- annotate_figure(g10, top = text_grob("Mean Trust by Tool", 
                                      color = "black", face = "bold", size = 14, vjust = 2))


g10




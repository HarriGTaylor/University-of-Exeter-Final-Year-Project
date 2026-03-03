library(openxlsx)
library(ggplot2)
library(tidyverse)
library(readxl)

setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves")
dummy<-read.xlsx("Demonstration Curve Data.xlsx")

dummy_long<-pivot_longer(dummy, 
                         cols=-Condition,
                         values_to = "OD",
                         names_to = "Hours")

dummy_long$Hours<-as.numeric(dummy_long$Hours)

condition_colours<-c("No activity"="black", "Strong activity"="slategray1", 
                    "Development of phage resistance; low fitness cost"="slategray",
                    "Development of phage resistance; high fitness cost"="slategray3")


plot<-ggplot(dummy_long, aes(x = Hours, y=OD, color = Condition, group = Condition)) +
  geom_line(size = 1) +  
  labs(
    title = "Example Curves",
    x = "Time (hours)",
    y = "OD600")+
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.00,2.2), breaks = seq(0.00, 2.2, 0.2), expand = c(0, 0)) +
  scale_color_manual(values = condition_colours)+
  theme_classic()+
  theme(
    plot.title = element_text(size=14, face="bold", hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size=12, face="bold"),
    axis.line = element_line(linewidth=0.8),
    axis.ticks = element_line(linewidth=0.8),
    legend.position="none")

setwd("C:/Users/harri/Documents/uni/Year 4/Dissertation/Lab Work/Growth Curves/Cocktail Testing/graphs/summary")
ggsave("Demonstration.png",plot, width = 10, height = 6, dpi = 300)

